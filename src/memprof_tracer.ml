open Kcas

type t =
  { locked : bool Loc.t;
    locked_ext : bool Loc.t;
    failed : bool Loc.t;
    stopped : bool Loc.t;
    report_exn : exn -> unit;
    trace : Trace.Writer.t;
    ext_sampler : Geometric_sampler.t; }

let curr_active_tracer : t option Loc.t = Loc.make None

let active_tracer () = Xt.commit { tx = fun ~xt -> Xt.get ~xt curr_active_tracer }

let bytes_before_ext_sample = Loc.make max_int

let draw_sampler_bytes t =
  Geometric_sampler.draw t.ext_sampler * (Sys.word_size / 8)

let[@inline never] rec lock_tracer ~xt s =
  if Xt.get ~xt s.locked then
    if Xt.get ~xt s.locked_ext then false
    else (Thread.yield (); lock_tracer s ~xt)
  else if Xt.get ~xt s.failed then
    false
  else
    (Xt.set ~xt s.locked true; true)

let[@inline never] rec lock_tracer_ext ~xt s =
  if Xt.get ~xt s.locked then
    (Thread.yield (); lock_tracer_ext ~xt s)
  else if Xt.get ~xt s.failed then
    false
  else
    (Xt.set ~xt s.locked true; Xt.set ~xt s.locked_ext true; true)

let[@inline never] unlock_tracer ~xt s =
  assert (Xt.get ~xt s.locked && not (Xt.get ~xt s.locked_ext) && not (Xt.get ~xt s.failed));
  Xt.set ~xt s.locked false

let[@inline never] unlock_tracer_ext ~xt s =
  assert (Xt.get ~xt s.locked && Xt.get ~xt s.locked_ext && not (Xt.get ~xt s.failed));
  Xt.set ~xt s.locked_ext false;
  Xt.set ~xt s.locked false

let[@inline never] mark_failed ~xt s e =
  assert (Xt.get ~xt s.locked && not (Xt.get ~xt s.failed));
  Xt.set ~xt s.failed true;
  Xt.set ~xt s.locked false;
  Xt.set ~xt s.locked_ext false;
  s.report_exn e

let default_report_exn e =
  match e with
  | Trace.Writer.Pid_changed ->
     (* This error is silently ignored, so that if Memtrace is active across
        Unix.fork () then the child process silently stops tracing *)
     ()
  | e ->
     let msg = Printf.sprintf "Memtrace failure: %s\n" (Printexc.to_string e) in
     output_string stderr msg;
     Printexc.print_backtrace stderr;
     flush stderr

let start ?(report_exn=default_report_exn) ~sampling_rate trace =
  let ext_sampler = Geometric_sampler.make ~sampling_rate () in
  let s = { trace; locked = Loc.make false; locked_ext = Loc.make false; stopped = Loc.make false; failed = Loc.make false;
            report_exn; ext_sampler } in
  let tracker : (_,_) Gc.Memprof.tracker = {
    alloc_minor = (fun info ->
      let tx ~xt =
        if lock_tracer ~xt s then begin
            match Trace.Writer.put_alloc_with_raw_backtrace trace (Trace.Timestamp.now ())
                    ~length:info.size
                    ~nsamples:info.n_samples
                    ~source:Minor
                    ~callstack:info.callstack
            with
            | r -> unlock_tracer ~xt s; Some r
            | exception e -> mark_failed ~xt s e; None
          end else None in
      Xt.commit { tx });
    alloc_major = (fun info ->
      let tx ~xt =
        if lock_tracer ~xt s then begin
        match Trace.Writer.put_alloc_with_raw_backtrace trace (Trace.Timestamp.now ())
                ~length:info.size
                ~nsamples:info.n_samples
                ~source:Major
                ~callstack:info.callstack
        with
        | r -> unlock_tracer ~xt s; Some r
        | exception e -> mark_failed ~xt s e; None
          end else None in
      Xt.commit {tx});
    promote = (fun id ->
      let tx ~xt = if lock_tracer ~xt s then
        match Trace.Writer.put_promote trace (Trace.Timestamp.now ()) id with
        | () -> unlock_tracer ~xt s; Some id
        | exception e -> mark_failed ~xt s e; None
                   else None in
      Xt.commit {tx});
    dealloc_minor = (fun id ->
      let tx ~xt = if lock_tracer ~xt s then
        match Trace.Writer.put_collect trace (Trace.Timestamp.now ()) id with
        | () -> unlock_tracer ~xt s
        | exception e -> mark_failed ~xt s e in
      Xt.commit {tx});
    dealloc_major = (fun id ->
      let tx ~xt = if lock_tracer ~xt s then
        match Trace.Writer.put_collect trace (Trace.Timestamp.now ()) id with
        | () -> unlock_tracer ~xt s
        | exception e -> mark_failed ~xt s e in
      Xt.commit {tx}) } in
  let tx ~xt =
    Xt.set ~xt curr_active_tracer (Some s);
    Xt.set ~xt bytes_before_ext_sample (draw_sampler_bytes s)
  in
  Xt.commit {tx};
  let _t = Gc.Memprof.start
    ~sampling_rate
    ~callstack_size:max_int
    tracker in
  s

let stop s =
  let tx ~xt = if not (Xt.get ~xt s.stopped) then begin
    Xt.set ~xt s.stopped true;
    Gc.Memprof.stop ();
    if lock_tracer ~xt s then begin
      try Trace.Writer.close s.trace with e -> mark_failed ~xt s e
    end;
    Xt.set ~xt curr_active_tracer None
                 end in
  Xt.commit {tx}

let[@inline never] ext_alloc_slowpath ~xt ~bytes =
  match Xt.get ~xt curr_active_tracer with
  | None -> Xt.set ~xt bytes_before_ext_sample max_int; None
  | Some s ->
    if lock_tracer_ext ~xt s then begin
      match
        let bytes_per_word = Sys.word_size / 8 in
        (* round up to an integer number of words *)
        let size_words = (bytes + bytes_per_word - 1) / bytes_per_word in
        let samples = Atomic.make 0 in
        let a = Xt.get ~xt bytes_before_ext_sample in
        while a <= 0 do
          Xt.set ~xt bytes_before_ext_sample (a + draw_sampler_bytes s);
          (* bytes_before_ext_sample := *)
          (*   !bytes_before_ext_sample + draw_sampler_bytes s; *)
          Atomic.incr samples
        done;
        assert ((Atomic.get samples) > 0);
        let callstack = Printexc.get_callstack max_int in
        Some (Trace.Writer.put_alloc_with_raw_backtrace s.trace
                (Trace.Timestamp.now ())
                ~length:size_words
                ~nsamples:(Atomic.get samples)
                ~source:External
                ~callstack)
      with
      | r -> unlock_tracer_ext ~xt s; r
      | exception e -> mark_failed ~xt s e; None
    end else None


type ext_token = Trace.Obj_id.t

let ext_alloc ~xt ~bytes =
  let n = Xt.get ~xt bytes_before_ext_sample - bytes in
  Xt.set ~xt bytes_before_ext_sample n;
  if n <= 0 then ext_alloc_slowpath ~xt ~bytes else None

let ext_free ~xt id =
  match Xt.get ~xt curr_active_tracer with
  | None -> ()
  | Some s ->
    if lock_tracer_ext ~xt s then begin
      match
        Trace.Writer.put_collect s.trace (Trace.Timestamp.now ()) id
      with
      | () -> unlock_tracer_ext ~xt s; ()
      | exception e -> mark_failed ~xt s e; ()
    end

type t =
  { mutable failed : bool;
    mutable stopped : bool;
    mutex : Mutex.t;
    report_exn : exn -> unit;
    trace : Proto.Writer.t;
    ext_sampler : Geometric_sampler.t; }

let curr_active_tracer : t option Atomic.t  = Atomic.make None

let active_tracer () = Atomic.get curr_active_tracer

let bytes_before_ext_sample = Atomic.make max_int

let draw_sampler_bytes (t : t) =
  Geometric_sampler.draw t.ext_sampler * (Sys.word_size / 8)

(* This is also all duplicated for now *)
let[@inline never] rec lock_tracer (s: t) =
  (* Try unlocking mutex returning true if success or
     Thread.yield () until it can acquire the mutex successfully.
   *)
  if Mutex.try_lock s.mutex then
    true
    else if s.failed then
      false
      else
        (Thread.yield (); lock_tracer s)

let[@inline never] unlock_tracer (s: t) =
  assert (not s.failed);
  Mutex.unlock s.mutex

let[@inline never] mark_failed_tracer (s: t) e =
  s.failed <- true;
  s.report_exn e;
  Mutex.unlock s.mutex

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
  let mutex = Mutex.create () in
  let s = { mutex; stopped = false; failed = false;
            report_exn; ext_sampler; trace; } in
  let tracker : (_,_) Gc.Memprof.tracker = {
    alloc_minor = (fun info ->
      if lock_tracer s then begin
        match Proto.Writer.put_alloc_with_raw_backtrace trace (Int64.of_float (Unix.gettimeofday ()))
              ~length:info.size
              ~nsamples:info.n_samples
              ~source:Minor
              ~callstack:info.callstack
        with
        | r -> unlock_tracer s; Some r
        | exception e ->
          mark_failed_tracer s e;
          None
      end
      else None);
    alloc_major = (fun info ->
      if lock_tracer s then begin
        match Proto.Writer.put_alloc_with_raw_backtrace trace (Int64.of_float (Unix.gettimeofday ()))
              ~length:info.size
              ~nsamples:info.n_samples
              ~source:Major
              ~callstack:info.callstack
        with
        | r -> unlock_tracer s; Some r
        | exception e -> mark_failed_tracer s e; None
      end else None);
    promote = (fun id ->
      if lock_tracer s then
        match Proto.Writer.put_promote trace (Proto.Timestamp.of_float (Unix.gettimeofday ())) id with
        | () -> unlock_tracer s; Some id
        | exception e -> mark_failed_tracer s e; None
      else None);
    dealloc_minor = (fun id ->
      if lock_tracer s then
        match Proto.Writer.put_collect trace (Proto.Timestamp.of_float (Unix.gettimeofday ())) id with
        | () -> unlock_tracer s
        | exception e -> mark_failed_tracer s e);
    dealloc_major = (fun id ->
      if lock_tracer s then
        match Proto.Writer.put_collect trace (Proto.Timestamp.of_float (Unix.gettimeofday ())) id with
        | () -> unlock_tracer s
        | exception e -> mark_failed_tracer s e) } in
  Atomic.set curr_active_tracer (Some s);
  (*Atomic.set bytes_before_ext_sample (draw_sampler_bytes s);*)
  let _t = Gc.Memprof.start
    ~sampling_rate
    ~callstack_size:max_int
    tracker in
  s

let stop s =
  if not s.stopped then begin
    s.stopped <- true;
    Gc.Memprof.stop ();
    Mutex.protect s.mutex (fun () ->
      (try Proto.Writer.close s.trace
        with e ->
          (s.failed <- true; s.report_exn e);
          Atomic.set curr_active_tracer None)
    )
  end

let[@inline never] ext_alloc_slowpath ~bytes =
  match Atomic.get curr_active_tracer with
  | None -> Atomic.set bytes_before_ext_sample max_int; None
  | Some s ->
    if lock_tracer s then begin
      match
        let bytes_per_word = Sys.word_size / 8 in
        (* round up to an integer number of words *)
        let size_words = (bytes + bytes_per_word - 1) / bytes_per_word in
        let samples = Atomic.make 0 in
        while Atomic.get bytes_before_ext_sample <= 0 do
          Atomic.set bytes_before_ext_sample
            (Atomic.get bytes_before_ext_sample + draw_sampler_bytes s);
          Atomic.incr samples
        done;
        assert (Atomic.get samples > 0);
        let callstack = Printexc.get_callstack max_int in
        Some (Proto.Writer.put_alloc_with_raw_backtrace s.trace
                (Int64.of_float (Unix.gettimeofday ()))
                (* (Trace.Timestamp.now ()) *)
                ~length:size_words
                ~nsamples:(Atomic.get samples)
                ~source:External
                ~callstack)
      with
      | r -> unlock_tracer s; r
      | exception e -> mark_failed_tracer s e; None
    end else None

type ext_token = Proto.Obj_id.t

let ext_alloc ~bytes =
  let n = Atomic.get bytes_before_ext_sample - bytes in
  Atomic.set bytes_before_ext_sample n;
  if n <= 0 then ext_alloc_slowpath ~bytes else None

let ext_free id =
  match Atomic.get curr_active_tracer with
  | None -> ()
  | Some s ->
    if lock_tracer s then begin
        match
          Proto.Writer.put_collect s.trace (Proto.Timestamp.of_float (Unix.gettimeofday ())) id
(* (Trace.Timestamp.now ()) id *)
        with
        | () -> unlock_tracer s; ()
        | exception e -> mark_failed_tracer s e; ()
    end


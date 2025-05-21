(** Encoder and decoder for Memtrace traces *)

(* Time since the epoch *)
module Timestamp = struct
  type t = int64

  let of_int64 t = t
  let to_int64 t = t

  let to_float t =
    (Int64.to_float t) /. 1_000_000.

  let of_float f =
    f *. 1_000_000. |> Int64.of_float

  let now () = of_float (Unix.gettimeofday ())
  end

(* Time since the start of the trace *)
module Timedelta = struct
  type t = int64

  let to_int64 t = t
  let offset = Int64.add
  end

(** Trace info *)
module Info = struct
  type t = {
    sample_rate : float;
    word_size : int;
    executable_name : string;
    host_name : string;
    ocaml_runtime_params : string;
    pid : Int64.t;
    start_time : int64;
    context : string option;
    }
end

module IntTbl = Hashtbl.MakeSeeded (struct
  type t = int

  let hash _seed (id : t) =
    let h = id * 189696287 in
    h lxor (h lsr 23)

  (* Required for OCaml >= 5.0.0, but causes errors for older compilers
     because it is an unused value declaration. *)
  let [@warning "-32"] seeded_hash = hash

  let equal (a : t) (b : t) = a = b
  end)


module Obj_id = struct
  type t = int
  module Tbl = IntTbl
end

module Location_code = struct
  type t = int
  module Tbl = IntTbl
end

module Allocation_source = struct
  type t = Minor | Major | External
end

module Event = struct
  type t =
    | Alloc of {
        obj_id : Obj_id.t;
        length : int;
        nsamples : int;
        source : Allocation_source.t;
        backtrace_buffer : Location_code.t array;
        backtrace_length : int;
        common_prefix : int;
      }
    | Promote of Obj_id.t
    | Collect of Obj_id.t

  let to_string decode_loc = function
    | Alloc {obj_id; length; nsamples; source;
             backtrace_buffer; backtrace_length; common_prefix} ->
      let backtrace =
        List.init backtrace_length (fun i ->
            let s = backtrace_buffer.(i) in
            match decode_loc s with
            | [] -> Printf.sprintf "$%d" (s :> int)
            | ls -> String.concat " " (List.map Location.to_string ls))
        |> String.concat " " in
      let alloc_src =
        match source with
        | Minor -> "alloc"
        | Major -> "alloc_major"
        | External -> "alloc_ext" in
      Printf.sprintf "%010d %s %d len=%d % 4d: %s"
        (obj_id :> int) alloc_src
        nsamples length common_prefix
        backtrace;
    | Promote id ->
      Printf.sprintf "%010d promote" (id :> int)
    | Collect id ->
      Printf.sprintf "%010d collect" (id :> int)
end

(** Writing traces *)
module type Writer = sig
  type t
  exception Pid_changed
  val create : Unix.file_descr -> ?getpid:(unit -> int64) -> Info.t -> t

  (** All of the functions below may raise Unix_error if
      writing to the file descriptor fails, or Pid_changed
      if getpid returns a different value. *)

  val put_alloc :
    t
    -> Timestamp.t
    -> length:int
    -> nsamples:int
    -> source:Allocation_source.t
    -> callstack:Location_code.t array
    -> decode_callstack_entry:(Location_code.t -> Location.t list)
    -> Obj_id.t
  val put_alloc_with_raw_backtrace :
    t
    -> Timestamp.t
    -> length:int
    -> nsamples:int
    -> source:Allocation_source.t
    -> callstack:Printexc.raw_backtrace
    -> Obj_id.t
  val put_collect : t -> Timestamp.t -> Obj_id.t -> unit
  val put_promote : t -> Timestamp.t -> Obj_id.t -> unit
  val put_event :
    t
    -> decode_callstack_entry:(Location_code.t -> Location.t list)
    -> Timestamp.t
    -> Event.t
    -> unit

  val flush : t -> unit
  val close : t -> unit
end

module Make(W : Writer) = struct
  type t =
    { failed : bool Atomic.t;
      stopped : bool Atomic.t;
      mutex : Mutex.t;
      report_exn : exn -> unit;
      trace : W.t;
      profile : Gc.Memprof.t option Atomic.t;
      ext_sampler : Geometric_sampler.t; }

  let getpid64 () = Int64.of_int (Unix.getpid ())

  let curr_active_tracer : t option Atomic.t  = Atomic.make None

  let active_tracer () = Atomic.get curr_active_tracer

  let bytes_before_ext_sample = Atomic.make max_int

  let draw_sampler_bytes t =
    Geometric_sampler.draw t.ext_sampler * (Sys.word_size / 8)

  let[@inline never] rec lock_tracer s =
    (* Try unlocking mutex returning true if success or
     Thread.yield () until it can acquire the mutex successfully.
   *)
    if (Atomic.get s.failed) then
      false
      else if Mutex.try_lock s.mutex then
        true
        else
          (Thread.yield (); lock_tracer s)

  let[@inline never] unlock_tracer s =
    assert (not (Atomic.get s.failed));
  Mutex.unlock s.mutex

  let[@inline never] mark_failed s e =
    if (Atomic.compare_and_set s.failed false true) then
      s.report_exn e;
    Mutex.unlock s.mutex


  let default_report_exn e =
    match e with
    | W.Pid_changed ->
      (* This error is silently ignored, so that if Memtrace is active across
          Unix.fork () then the child process silently stops tracing *)
      ()
    | e ->
      let msg = Printf.sprintf "Memtrace failure: %s\n" (Printexc.to_string e) in
       output_string stderr msg;
       Printexc.print_backtrace stderr;
       flush stderr

  let start ?(report_exn=default_report_exn) ?context ~sampling_rate fd =
    let ext_sampler = Geometric_sampler.make ~sampling_rate () in
    let mutex = Mutex.create () in
    let info : Info.t =
       { sample_rate = sampling_rate;
         word_size = Sys.word_size;
         executable_name = Sys.executable_name;
         host_name = Unix.gethostname ();
         ocaml_runtime_params = Sys.runtime_parameters ();
         pid = getpid64 ();
         start_time = Timestamp.of_float (Unix.gettimeofday ());
         context;
    } in

    let trace = W.create fd ~getpid:getpid64 info in
    let s = { trace; mutex; stopped = Atomic.make false; failed = Atomic.make false;
              report_exn; ext_sampler; profile = Atomic.make None } in
    let tracker : (_,_) Gc.Memprof.tracker =
       {
         alloc_minor = (fun info ->
           if lock_tracer s then begin
             match W.put_alloc_with_raw_backtrace trace (Timestamp.now ())
                     ~length:info.size
                     ~nsamples:info.n_samples
                     ~source:Minor
                     ~callstack:info.callstack
             with
             | r -> unlock_tracer s; Some r
             | exception e ->
                mark_failed s e;
                None
             end
           else None);
         alloc_major = (fun info ->
           if lock_tracer s then begin
             match W.put_alloc_with_raw_backtrace trace (Timestamp.now ())
                     ~length:info.size
                     ~nsamples:info.n_samples
                     ~source:Major
                     ~callstack:info.callstack
             with
             | r -> unlock_tracer s; Some r
             | exception e -> mark_failed s e; None
           end else None);
         promote = (fun id ->
           if lock_tracer s then
             match W.put_promote trace (Timestamp.now ()) id with
             | () -> unlock_tracer s; Some id
             | exception e -> mark_failed s e; None
           else None);
         dealloc_minor = (fun id ->
           if lock_tracer s then
             match W.put_collect trace (Timestamp.now ()) id with
             | () -> unlock_tracer s
             | exception e -> mark_failed s e);
         dealloc_major = (fun id ->
           if lock_tracer s then
             match W.put_collect trace (Timestamp.now ()) id with
             | () -> unlock_tracer s
             | exception e -> mark_failed s e) } in
    Atomic.set curr_active_tracer (Some s);
    Atomic.set bytes_before_ext_sample (draw_sampler_bytes s);
    let profile = Gc.Memprof.start ~sampling_rate ~callstack_size:max_int tracker in
    Atomic.set s.profile (Some profile);
    s

  let stop s =
    Gc.Memprof.stop ();
    Option.iter Gc.Memprof.discard (Atomic.get s.profile);
    if (Atomic.compare_and_set s.stopped false true) then begin
      if lock_tracer s then begin
        try W.close s.trace with e ->
          (Atomic.set s.failed true; s.report_exn e);
        Mutex.unlock s.mutex;
      end;
      Atomic.set curr_active_tracer None
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
            ignore (Atomic.fetch_and_add bytes_before_ext_sample (draw_sampler_bytes s));
            Atomic.incr samples
          done;
          assert (Atomic.get samples > 0);
          let callstack = Printexc.get_callstack max_int in
          Some (W.put_alloc_with_raw_backtrace s.trace
                  (Timestamp.now ())
                  ~length:size_words
                  ~nsamples:(Atomic.get samples)
                  ~source:External
                  ~callstack)
        with
        | r -> unlock_tracer s; r
        | exception e -> mark_failed s e; None
      end else None

  type ext_token = Obj_id.t

  let ext_alloc ~bytes =
    let n = Atomic.fetch_and_add bytes_before_ext_sample (- bytes) in
    if n <= 0 then ext_alloc_slowpath ~bytes else None

  let ext_free id =
    match Atomic.get curr_active_tracer with
    | None -> ()
    | Some s ->
      if lock_tracer s then begin
        match
          W.put_collect s.trace (Timestamp.now ()) id
        with
        | () -> unlock_tracer s; ()
        | exception e -> mark_failed s e; ()
        end
end

(* Reading traces *)
module type Reader = sig
  type t

  val create : Unix.file_descr -> t
  val info : t -> Info.t
  val lookup_location_code : t -> Location_code.t -> Location.t list

  val iter : t -> ?parse_backtraces:bool -> (Timedelta.t -> Event.t -> unit) -> unit

  val open_ : filename:string -> t
  val size_bytes : t -> int64
  val close : t -> unit
end
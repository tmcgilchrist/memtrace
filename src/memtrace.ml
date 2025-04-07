type tracer =
  | CTF_tracer of Memprof_tracer.t
  | Proto_tracer of Memprof_tracer_proto.t

(* What file format to write *)
type profile_format = CTF | Proto


let file = ref ""
let convert = false

let getpid64 () = Int64.of_int (Unix.getpid ())

(* TODO Duplicate this function based on detected profile format *)
let start_tracing ~context ~sampling_rate ~filename ~trace_format =
  if ((Memprof_tracer.active_tracer () <> None) ||
      (Memprof_tracer_proto.active_tracer () <> None)) then
    failwith "Only one Memtrace instance may be active at a time";
  let fd =
    try Unix.openfile filename Unix.[O_CREAT;O_WRONLY] 0o600
    with Unix.Unix_error (err, _, _) ->
      raise (Invalid_argument ("Cannot open memtrace file " ^ filename ^
                               ": " ^ Unix.error_message err))
  in
  begin
    try Unix.lockf fd F_TLOCK 0
    with Unix.Unix_error _ ->
      Unix.close fd;
      raise (Invalid_argument ("Cannot lock memtrace file " ^ filename ^
                               ": is another process using it?"))
  end;
  begin
    try Unix.ftruncate fd 0
    with Unix.Unix_error _ ->
      (* On special files (e.g. /dev/null), ftruncate fails. Ignoring errors
         here gives us the truncate-if-a-regular-file behaviour of O_TRUNC. *)
      ()
  end;

  match trace_format with
  | CTF ->
      let open Trace in
      let info : Info.t =
         { sample_rate = sampling_rate;
           word_size = Sys.word_size;
           executable_name = Sys.executable_name;
           host_name = Unix.gethostname ();
           ocaml_runtime_params = Sys.runtime_parameters ();
           pid = getpid64 ();
           start_time = Timestamp.of_float (Unix.gettimeofday ()); (*TODO: this timestamp stuff needs to be handled better *)
           context;
      } in
      let trace_writer = Writer.create fd ~getpid:getpid64 info in
      let tracer = Memprof_tracer.start ~sampling_rate trace_writer in
      CTF_tracer tracer
  | Proto ->
      let open Proto in
      let info : Info.t =
        { sample_rate = sampling_rate;
          word_size = Sys.word_size;
          executable_name = Sys.executable_name;
          host_name = Unix.gethostname ();
          ocaml_runtime_params = Sys.runtime_parameters ();
          pid = getpid64 ();
          start_time = Timestamp.of_float (Unix.gettimeofday ()); (*TODO: this timestamp stuff needs to be handled better *)
          context;
          } in
      let trace_writer = Writer.create fd ~getpid:getpid64 info in
      let tracer = Memprof_tracer_proto.start ~sampling_rate trace_writer in
      Proto_tracer tracer

let stop_tracing t =
  match t with
  | CTF_tracer tracer -> Memprof_tracer.stop tracer
  | Proto_tracer tracer -> Memprof_tracer_proto.stop tracer

let create_pb_file filename = Ctf_to_proto.convert_file filename (filename ^ ".pb")

let () =
  at_exit (
    fun () ->
      begin
        Memprof_tracer_proto.active_tracer ()
        |> Option.map (fun x -> Proto_tracer x)
        |> Option.iter stop_tracing ;

        Memprof_tracer.active_tracer ()
        |> Option.map (fun x -> CTF_tracer x)
        |> Option.iter stop_tracing ;

        if convert then create_pb_file !file
        
      end
  ) 

let default_sampling_rate = 1e-6

let trace_if_requested ?context ?sampling_rate () =
  match Sys.getenv_opt "MEMTRACE" with
  | None | Some "" -> ()
  | Some filename ->
     (* Prevent spawned OCaml programs from being traced *)
     file := filename;
     Unix.putenv "MEMTRACE" "";
     let check_rate = function
       | Some rate when 0. < rate && rate <= 1. -> rate
       | _ ->
         raise (Invalid_argument ("Memtrace.trace_if_requested: " ^
                                  "sampling_rate must be between 0 and 1"))
     in
     let sampling_rate =
       match Sys.getenv_opt "MEMTRACE_RATE" with
       | Some rate -> check_rate (float_of_string_opt rate)
       | None ->
         match sampling_rate with
         | Some _ -> check_rate sampling_rate
         | None -> default_sampling_rate
     in
    sample_rate := sampling_rate;
    let trace_format =
      match Sys.getenv_opt "MEMTRACE_FORMAT" with
      | Some "proto" -> Proto
      | Some "ctf" | Some _ | None -> CTF (* Default to CTF *)
    in
    ignore (start_tracing ~context ~sampling_rate ~filename ~trace_format)


module Trace = Trace
module Profile = Profile
module Writer_helper = Writer_helper
module Memprof_tracer = Memprof_tracer

module External = struct
  type token = Memprof_tracer.ext_token
  let alloc = Memprof_tracer.ext_alloc
  let free = Memprof_tracer.ext_free
end
module Geometric_sampler = Geometric_sampler

module Ctf_to_proto = Ctf_to_proto
module Memprof_tracer_proto = Memprof_tracer_proto
module Memory_map = Memory_map


module CTF_tracer = Trace_s.Make(Trace.Writer)
module Proto_tracer = Trace_s.Make(Proto.Writer)

type tracer =
  | CTF_tracer of CTF_tracer.t
  | Proto_tracer of Proto_tracer.t

type profile_format = CTF | Proto

let start_tracing ~context ~sampling_rate ~filename ~trace_format =
  if ((CTF_tracer.active_tracer () <> None) (* || *)
    (* (Memprof_tracer_proto.active_tracer () <> None) *)) then
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
  | CTF -> CTF_tracer (CTF_tracer.start ~sampling_rate ?context fd)
  | Proto -> Proto_tracer (Proto_tracer.start ~sampling_rate ?context fd)

let stop_tracing t =
  match t with
  | CTF_tracer tracer -> CTF_tracer.stop tracer
  | Proto_tracer tracer -> Proto_tracer.stop tracer

let () =
  at_exit (
    fun () ->
      Proto_tracer.active_tracer () |> Option.iter Proto_tracer.stop;
      CTF_tracer.active_tracer () |> Option.iter CTF_tracer.stop;
    )

let default_sampling_rate = 1e-6

let trace_if_requested ?context ?sampling_rate () =
  match Sys.getenv_opt "MEMTRACE" with
  | None | Some "" -> ()
  | Some filename ->
     (* Prevent spawned OCaml programs from being traced *)
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
    let trace_format =
      match Sys.getenv_opt "MEMTRACE_FORMAT" with
      | Some "proto" -> Proto
      | Some "ctf" | Some _ | None -> CTF (* Default to CTF *)
    in
    ignore (start_tracing ~context ~sampling_rate ~filename ~trace_format)


module Trace = Trace
module Profile = Profile
(* module Memprof_tracer = Memprof_tracer *)

module Geometric_sampler = Geometric_sampler

module Ctf_to_proto = Ctf_to_proto
module Proto = Proto

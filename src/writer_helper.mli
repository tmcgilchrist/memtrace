(** Global trace info *)
module Info : sig
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

(** Types of allocation *)
module Allocation_source : sig
    type t = Minor | Major | External
end

(** Writing traces *)
module type Writer_interface = sig
    type t
    exception Pid_changed
    val create : Unix.file_descr -> ?getpid:(unit -> int64) -> Info.t -> t
  
    (** All of the functions below may raise Unix_error if
        writing to the file descriptor fails, or Pid_changed
        if getpid returns a different value. *)
    val put_alloc_with_raw_backtrace :
      t
      -> int64
      -> length:int
      -> nsamples:int
      -> source:Allocation_source.t
      -> callstack:Printexc.raw_backtrace
      -> int
    val put_collect : t -> int64 -> int -> unit
    val put_promote : t -> int64 -> int -> unit
  
    val flush : t -> unit
    val close : t -> unit
end
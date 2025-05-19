type line
type location
type function_

(** Encoder and decoder for Memtrace traces *)

(** Timestamps *)
module Timestamp : sig
  type t
  val now : unit -> t

  (** Convert to and from the number of microseconds since the Unix epoch *)
  val of_int64 : int64 -> t
  val to_int64 : t -> int64

  (** Convert back and forth between the Unix module's float format and timestamps *)
  val to_float : t -> float
  val of_float : float -> t
end

(** Times measured from the start of the trace *)
module Timedelta : sig
  type t

  (** Convert to the number of microseconds since the start of the trace *)
  val to_int64 : t -> int64
  val offset : Timestamp.t -> t -> Timestamp.t
end

(** Identifiers to represent allocations *)
module Obj_id : sig
  type t = int

  (** For convenience, a hashtable keyed by object ID *)
  module Tbl : Hashtbl.SeededS with type key = t
end

(** Codes for subsequences of locations in a backtrace *)
module Location_code : sig
  type t = private int

  (** For convenience, a hashtable keyed by location code *)
  module Tbl : Hashtbl.SeededS with type key = t
end

(** Types of allocation *)
module Allocation_source : sig
  type t = Minor | Major | External
end

(** Global trace info *)
module Info : sig
  type t = {
    sample_rate : float;
    word_size : int;
    executable_name : string;
    host_name : string;
    ocaml_runtime_params : string;
    pid : Int64.t;
    start_time : Timestamp.t;
    context : string option;
    }
  end

(** Writing traces *)
module Writer : sig
  type t
  exception Pid_changed
  val create : Unix.file_descr -> ?getpid:(unit -> int64) -> Info.t -> t

  (** All of the functions below may raise Unix_error if
      writing to the file descriptor fails, or Pid_changed
      if getpid returns a different value. *)
  val put_alloc_with_raw_backtrace :
    t
    -> Timestamp.t
    -> length:int
    -> nsamples:int
    -> source:Allocation_source.t
    -> callstack:Printexc.raw_backtrace
    -> int

  val put_collect : t -> Timestamp.t -> Obj_id.t -> unit
  val put_promote : t -> Timestamp.t -> Obj_id.t -> unit

  val flush : t -> unit
  val close : t -> unit
end


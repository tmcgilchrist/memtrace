(** Identifiers to represent allocations *)
module Obj_id : sig
  type t = private int

  (** For convenience, a hashtable keyed by object ID *)
  module Tbl : Hashtbl.SeededS with type key = t
end

(** Writing traces *)
module type Writer : sig
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
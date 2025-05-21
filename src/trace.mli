(** Encoder and decoder for Memtrace traces in CTF format. *)

(** Timestamps *)
module Timestamp = Trace_s.Timestamp

(** Times measured from the start of the trace *)
module Timedelta = Trace_s.Timedelta

(** Trace events *)
module Event = Trace_s.Event

(** Identifiers to represent allocations *)
module Obj_id = Trace_s.Obj_id

(** Source locations *)
module Location = Location

(** Codes for subsequences of locations in a backtrace *)
module Location_code =  Trace_s.Location_code

(** Types of allocation *)
module Allocation_source = Trace_s.Allocation_source

(** Global trace info *)
module Info = Trace_s.Info

(** Writing traces *)
module Writer : Trace_s.Writer

(** Reading traces *)
module Reader : Trace_s.Reader
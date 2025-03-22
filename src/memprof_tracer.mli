type t
type t'

val start : ?report_exn:(exn -> unit) -> sampling_rate:float -> Trace.Writer.t -> t
val stop : t -> unit 
val start_pprof : ?report_exn:(exn -> unit) -> sampling_rate:float -> Proto.Writer.t -> t'
val stop_pprof : t' -> unit

val active_tracer : unit -> t option
val active_proto : unit -> t' option

type ext_token [@@immediate]
val ext_alloc : bytes:int -> ext_token option
val ext_free : ext_token -> unit

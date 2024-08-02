type t
val start : ?report_exn:(exn -> unit) -> sampling_rate:float -> Trace.Writer.t -> t
val stop : t -> unit

val active_tracer : unit -> t option

type ext_token [@@immediate]
val ext_alloc : xt:'a Kcas.Xt.t -> bytes:int -> ext_token option

val ext_free : xt:'a Kcas.Xt.t -> ext_token -> unit

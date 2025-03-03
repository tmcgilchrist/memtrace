val loc_map : (Trace.Location_code.t, Profile.location) Hashtbl.t
val fn_ids : string list ref

val get_or_add_string : string -> string list ref -> int64
val get_or_add_fnid : string -> string list ref -> int64 * bool

val loc_to_int : Trace.Location_code.t -> int64
val micro_to_nanoseconds : int64 -> int64 

val update_locs : Trace.Reader.t -> Trace.Location_code.t array -> Profile.function_ list ref -> Profile.location list ref -> string list ref -> int64 list

val convert_events : string -> float -> float -> Profile.profile
val convert_file : string -> string -> float -> float -> unit

val sample_to_string : Profile.sample -> string list ref -> string
val label_to_string : string list ref -> Profile.label -> string
val print_ev : Trace.Event.t ->  Trace.Reader.t -> unit
val print_value_type : Profile.value_type -> unit
(*val print_label : Profile.label -> unit
val print_sample : Profile.sample -> unit
val print_mapping : Profile.mapping -> unit
val print_line : Profile.line -> unit
val print_location : Profile.location -> unit
val print_function : Profile.function_ -> unit
val print_profile : Profile.profile -> unit*)

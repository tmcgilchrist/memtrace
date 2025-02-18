
(** Code for profile.proto *)

(* generated from "profile.proto", do not edit *)



(** {2 Types} *)

type value_type = {
  type_ : int64;
  unit_ : int64;
}

type label = {
  key : int64;
  str : int64;
  num : int64;
  num_unit : int64;
}

type sample = {
  location_id : int64 list;
  value : int64 list;
  label : label list;
}

type mapping = {
  id : int64;
  memory_start : int64;
  memory_limit : int64;
  file_offset : int64;
  filename : int64;
  build_id : int64;
  has_functions : bool;
  has_filenames : bool;
  has_line_numbers : bool;
  has_inline_frames : bool;
}

type line = {
  function_id : int64;
  line : int64;
  column : int64;
}

type location = {
  id : int64;
  mapping_id : int64;
  address : int64;
  line : line list;
  is_folded : bool;
}

type function_ = {
  id : int64;
  name : int64;
  system_name : int64;
  filename : int64;
  start_line : int64;
}

type profile = {
  sample_type : value_type list;
  sample : sample list;
  mapping : mapping list;
  location : location list;
  function_ : function_ list;
  string_table : string list;
  drop_frames : int64;
  keep_frames : int64;
  time_nanos : int64;
  duration_nanos : int64;
  period_type : value_type option;
  period : int64;
  comment : int64 list;
  default_sample_type : int64;
  doc_url : int64;
}


(** {2 Basic values} *)

val default_value_type : 
  ?type_:int64 ->
  ?unit_:int64 ->
  unit ->
  value_type
(** [default_value_type ()] is the default value for type [value_type] *)

val default_label : 
  ?key:int64 ->
  ?str:int64 ->
  ?num:int64 ->
  ?num_unit:int64 ->
  unit ->
  label
(** [default_label ()] is the default value for type [label] *)

val default_sample : 
  ?location_id:int64 list ->
  ?value:int64 list ->
  ?label:label list ->
  unit ->
  sample
(** [default_sample ()] is the default value for type [sample] *)

val default_mapping : 
  ?id:int64 ->
  ?memory_start:int64 ->
  ?memory_limit:int64 ->
  ?file_offset:int64 ->
  ?filename:int64 ->
  ?build_id:int64 ->
  ?has_functions:bool ->
  ?has_filenames:bool ->
  ?has_line_numbers:bool ->
  ?has_inline_frames:bool ->
  unit ->
  mapping
(** [default_mapping ()] is the default value for type [mapping] *)

val default_line : 
  ?function_id:int64 ->
  ?line:int64 ->
  ?column:int64 ->
  unit ->
  line
(** [default_line ()] is the default value for type [line] *)

val default_location : 
  ?id:int64 ->
  ?mapping_id:int64 ->
  ?address:int64 ->
  ?line:line list ->
  ?is_folded:bool ->
  unit ->
  location
(** [default_location ()] is the default value for type [location] *)

val default_function_ : 
  ?id:int64 ->
  ?name:int64 ->
  ?system_name:int64 ->
  ?filename:int64 ->
  ?start_line:int64 ->
  unit ->
  function_
(** [default_function_ ()] is the default value for type [function_] *)

val default_profile : 
  ?sample_type:value_type list ->
  ?sample:sample list ->
  ?mapping:mapping list ->
  ?location:location list ->
  ?function_:function_ list ->
  ?string_table:string list ->
  ?drop_frames:int64 ->
  ?keep_frames:int64 ->
  ?time_nanos:int64 ->
  ?duration_nanos:int64 ->
  ?period_type:value_type option ->
  ?period:int64 ->
  ?comment:int64 list ->
  ?default_sample_type:int64 ->
  ?doc_url:int64 ->
  unit ->
  profile
(** [default_profile ()] is the default value for type [profile] *)


(** {2 Protobuf Encoding} *)

val encode_pb_value_type : value_type -> Pbrt.Encoder.t -> unit
(** [encode_pb_value_type v encoder] encodes [v] with the given [encoder] *)

val encode_pb_label : label -> Pbrt.Encoder.t -> unit
(** [encode_pb_label v encoder] encodes [v] with the given [encoder] *)

val encode_pb_sample : sample -> Pbrt.Encoder.t -> unit
(** [encode_pb_sample v encoder] encodes [v] with the given [encoder] *)

val encode_pb_mapping : mapping -> Pbrt.Encoder.t -> unit
(** [encode_pb_mapping v encoder] encodes [v] with the given [encoder] *)

val encode_pb_line : line -> Pbrt.Encoder.t -> unit
(** [encode_pb_line v encoder] encodes [v] with the given [encoder] *)

val encode_pb_location : location -> Pbrt.Encoder.t -> unit
(** [encode_pb_location v encoder] encodes [v] with the given [encoder] *)

val encode_pb_function_ : function_ -> Pbrt.Encoder.t -> unit
(** [encode_pb_function_ v encoder] encodes [v] with the given [encoder] *)

val encode_pb_profile : profile -> Pbrt.Encoder.t -> unit
(** [encode_pb_profile v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_pb_value_type : Pbrt.Decoder.t -> value_type
(** [decode_pb_value_type decoder] decodes a [value_type] binary value from [decoder] *)

val decode_pb_label : Pbrt.Decoder.t -> label
(** [decode_pb_label decoder] decodes a [label] binary value from [decoder] *)

val decode_pb_sample : Pbrt.Decoder.t -> sample
(** [decode_pb_sample decoder] decodes a [sample] binary value from [decoder] *)

val decode_pb_mapping : Pbrt.Decoder.t -> mapping
(** [decode_pb_mapping decoder] decodes a [mapping] binary value from [decoder] *)

val decode_pb_line : Pbrt.Decoder.t -> line
(** [decode_pb_line decoder] decodes a [line] binary value from [decoder] *)

val decode_pb_location : Pbrt.Decoder.t -> location
(** [decode_pb_location decoder] decodes a [location] binary value from [decoder] *)

val decode_pb_function_ : Pbrt.Decoder.t -> function_
(** [decode_pb_function_ decoder] decodes a [function_] binary value from [decoder] *)

val decode_pb_profile : Pbrt.Decoder.t -> profile
(** [decode_pb_profile decoder] decodes a [profile] binary value from [decoder] *)

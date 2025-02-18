[@@@ocaml.warning "-27-30-39-44"]

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

let rec default_value_type 
  ?type_:((type_:int64) = 0L)
  ?unit_:((unit_:int64) = 0L)
  () : value_type  = {
  type_;
  unit_;
}

let rec default_label 
  ?key:((key:int64) = 0L)
  ?str:((str:int64) = 0L)
  ?num:((num:int64) = 0L)
  ?num_unit:((num_unit:int64) = 0L)
  () : label  = {
  key;
  str;
  num;
  num_unit;
}

let rec default_sample 
  ?location_id:((location_id:int64 list) = [])
  ?value:((value:int64 list) = [])
  ?label:((label:label list) = [])
  () : sample  = {
  location_id;
  value;
  label;
}

let rec default_mapping 
  ?id:((id:int64) = 0L)
  ?memory_start:((memory_start:int64) = 0L)
  ?memory_limit:((memory_limit:int64) = 0L)
  ?file_offset:((file_offset:int64) = 0L)
  ?filename:((filename:int64) = 0L)
  ?build_id:((build_id:int64) = 0L)
  ?has_functions:((has_functions:bool) = false)
  ?has_filenames:((has_filenames:bool) = false)
  ?has_line_numbers:((has_line_numbers:bool) = false)
  ?has_inline_frames:((has_inline_frames:bool) = false)
  () : mapping  = {
  id;
  memory_start;
  memory_limit;
  file_offset;
  filename;
  build_id;
  has_functions;
  has_filenames;
  has_line_numbers;
  has_inline_frames;
}

let rec default_line 
  ?function_id:((function_id:int64) = 0L)
  ?line:((line:int64) = 0L)
  ?column:((column:int64) = 0L)
  () : line  = {
  function_id;
  line;
  column;
}

let rec default_location 
  ?id:((id:int64) = 0L)
  ?mapping_id:((mapping_id:int64) = 0L)
  ?address:((address:int64) = 0L)
  ?line:((line:line list) = [])
  ?is_folded:((is_folded:bool) = false)
  () : location  = {
  id;
  mapping_id;
  address;
  line;
  is_folded;
}

let rec default_function_ 
  ?id:((id:int64) = 0L)
  ?name:((name:int64) = 0L)
  ?system_name:((system_name:int64) = 0L)
  ?filename:((filename:int64) = 0L)
  ?start_line:((start_line:int64) = 0L)
  () : function_  = {
  id;
  name;
  system_name;
  filename;
  start_line;
}

let rec default_profile 
  ?sample_type:((sample_type:value_type list) = [])
  ?sample:((sample:sample list) = [])
  ?mapping:((mapping:mapping list) = [])
  ?location:((location:location list) = [])
  ?function_:((function_:function_ list) = [])
  ?string_table:((string_table:string list) = [])
  ?drop_frames:((drop_frames:int64) = 0L)
  ?keep_frames:((keep_frames:int64) = 0L)
  ?time_nanos:((time_nanos:int64) = 0L)
  ?duration_nanos:((duration_nanos:int64) = 0L)
  ?period_type:((period_type:value_type option) = None)
  ?period:((period:int64) = 0L)
  ?comment:((comment:int64 list) = [])
  ?default_sample_type:((default_sample_type:int64) = 0L)
  ?doc_url:((doc_url:int64) = 0L)
  () : profile  = {
  sample_type;
  sample;
  mapping;
  location;
  function_;
  string_table;
  drop_frames;
  keep_frames;
  time_nanos;
  duration_nanos;
  period_type;
  period;
  comment;
  default_sample_type;
  doc_url;
}

type value_type_mutable = {
  mutable type_ : int64;
  mutable unit_ : int64;
}

let default_value_type_mutable () : value_type_mutable = {
  type_ = 0L;
  unit_ = 0L;
}

type label_mutable = {
  mutable key : int64;
  mutable str : int64;
  mutable num : int64;
  mutable num_unit : int64;
}

let default_label_mutable () : label_mutable = {
  key = 0L;
  str = 0L;
  num = 0L;
  num_unit = 0L;
}

type sample_mutable = {
  mutable location_id : int64 list;
  mutable value : int64 list;
  mutable label : label list;
}

let default_sample_mutable () : sample_mutable = {
  location_id = [];
  value = [];
  label = [];
}

type mapping_mutable = {
  mutable id : int64;
  mutable memory_start : int64;
  mutable memory_limit : int64;
  mutable file_offset : int64;
  mutable filename : int64;
  mutable build_id : int64;
  mutable has_functions : bool;
  mutable has_filenames : bool;
  mutable has_line_numbers : bool;
  mutable has_inline_frames : bool;
}

let default_mapping_mutable () : mapping_mutable = {
  id = 0L;
  memory_start = 0L;
  memory_limit = 0L;
  file_offset = 0L;
  filename = 0L;
  build_id = 0L;
  has_functions = false;
  has_filenames = false;
  has_line_numbers = false;
  has_inline_frames = false;
}

type line_mutable = {
  mutable function_id : int64;
  mutable line : int64;
  mutable column : int64;
}

let default_line_mutable () : line_mutable = {
  function_id = 0L;
  line = 0L;
  column = 0L;
}

type location_mutable = {
  mutable id : int64;
  mutable mapping_id : int64;
  mutable address : int64;
  mutable line : line list;
  mutable is_folded : bool;
}

let default_location_mutable () : location_mutable = {
  id = 0L;
  mapping_id = 0L;
  address = 0L;
  line = [];
  is_folded = false;
}

type function__mutable = {
  mutable id : int64;
  mutable name : int64;
  mutable system_name : int64;
  mutable filename : int64;
  mutable start_line : int64;
}

let default_function__mutable () : function__mutable = {
  id = 0L;
  name = 0L;
  system_name = 0L;
  filename = 0L;
  start_line = 0L;
}

type profile_mutable = {
  mutable sample_type : value_type list;
  mutable sample : sample list;
  mutable mapping : mapping list;
  mutable location : location list;
  mutable function_ : function_ list;
  mutable string_table : string list;
  mutable drop_frames : int64;
  mutable keep_frames : int64;
  mutable time_nanos : int64;
  mutable duration_nanos : int64;
  mutable period_type : value_type option;
  mutable period : int64;
  mutable comment : int64 list;
  mutable default_sample_type : int64;
  mutable doc_url : int64;
}

let default_profile_mutable () : profile_mutable = {
  sample_type = [];
  sample = [];
  mapping = [];
  location = [];
  function_ = [];
  string_table = [];
  drop_frames = 0L;
  keep_frames = 0L;
  time_nanos = 0L;
  duration_nanos = 0L;
  period_type = None;
  period = 0L;
  comment = [];
  default_sample_type = 0L;
  doc_url = 0L;
}

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Encoding} *)

let rec encode_pb_value_type (v:value_type) encoder = 
  Pbrt.Encoder.int64_as_varint v.type_ encoder;
  Pbrt.Encoder.key 1 Pbrt.Varint encoder; 
  Pbrt.Encoder.int64_as_varint v.unit_ encoder;
  Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  ()

let rec encode_pb_label (v:label) encoder = 
  Pbrt.Encoder.int64_as_varint v.key encoder;
  Pbrt.Encoder.key 1 Pbrt.Varint encoder; 
  Pbrt.Encoder.int64_as_varint v.str encoder;
  Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  Pbrt.Encoder.int64_as_varint v.num encoder;
  Pbrt.Encoder.key 3 Pbrt.Varint encoder; 
  Pbrt.Encoder.int64_as_varint v.num_unit encoder;
  Pbrt.Encoder.key 4 Pbrt.Varint encoder; 
  ()

let rec encode_pb_sample (v:sample) encoder = 
  Pbrt.Encoder.nested (fun lst encoder ->
    Pbrt.List_util.rev_iter_with (fun x encoder -> 
      Pbrt.Encoder.int64_as_varint x encoder;
    ) lst encoder;
  ) v.location_id encoder;
  Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  Pbrt.Encoder.nested (fun lst encoder ->
    Pbrt.List_util.rev_iter_with (fun x encoder -> 
      Pbrt.Encoder.int64_as_varint x encoder;
    ) lst encoder;
  ) v.value encoder;
  Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_label x encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  ) v.label encoder;
  ()

let rec encode_pb_mapping (v:mapping) encoder = 
  Pbrt.Encoder.int64_as_varint v.id encoder;
  Pbrt.Encoder.key 1 Pbrt.Varint encoder; 
  Pbrt.Encoder.int64_as_varint v.memory_start encoder;
  Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  Pbrt.Encoder.int64_as_varint v.memory_limit encoder;
  Pbrt.Encoder.key 3 Pbrt.Varint encoder; 
  Pbrt.Encoder.int64_as_varint v.file_offset encoder;
  Pbrt.Encoder.key 4 Pbrt.Varint encoder; 
  Pbrt.Encoder.int64_as_varint v.filename encoder;
  Pbrt.Encoder.key 5 Pbrt.Varint encoder; 
  Pbrt.Encoder.int64_as_varint v.build_id encoder;
  Pbrt.Encoder.key 6 Pbrt.Varint encoder; 
  Pbrt.Encoder.bool v.has_functions encoder;
  Pbrt.Encoder.key 7 Pbrt.Varint encoder; 
  Pbrt.Encoder.bool v.has_filenames encoder;
  Pbrt.Encoder.key 8 Pbrt.Varint encoder; 
  Pbrt.Encoder.bool v.has_line_numbers encoder;
  Pbrt.Encoder.key 9 Pbrt.Varint encoder; 
  Pbrt.Encoder.bool v.has_inline_frames encoder;
  Pbrt.Encoder.key 10 Pbrt.Varint encoder; 
  ()

let rec encode_pb_line (v:line) encoder = 
  Pbrt.Encoder.int64_as_varint v.function_id encoder;
  Pbrt.Encoder.key 1 Pbrt.Varint encoder; 
  Pbrt.Encoder.int64_as_varint v.line encoder;
  Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  Pbrt.Encoder.int64_as_varint v.column encoder;
  Pbrt.Encoder.key 3 Pbrt.Varint encoder; 
  ()

let rec encode_pb_location (v:location) encoder = 
  Pbrt.Encoder.int64_as_varint v.id encoder;
  Pbrt.Encoder.key 1 Pbrt.Varint encoder; 
  Pbrt.Encoder.int64_as_varint v.mapping_id encoder;
  Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  Pbrt.Encoder.int64_as_varint v.address encoder;
  Pbrt.Encoder.key 3 Pbrt.Varint encoder; 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_line x encoder;
    Pbrt.Encoder.key 4 Pbrt.Bytes encoder; 
  ) v.line encoder;
  Pbrt.Encoder.bool v.is_folded encoder;
  Pbrt.Encoder.key 5 Pbrt.Varint encoder; 
  ()

let rec encode_pb_function_ (v:function_) encoder = 
  Pbrt.Encoder.int64_as_varint v.id encoder;
  Pbrt.Encoder.key 1 Pbrt.Varint encoder; 
  Pbrt.Encoder.int64_as_varint v.name encoder;
  Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  Pbrt.Encoder.int64_as_varint v.system_name encoder;
  Pbrt.Encoder.key 3 Pbrt.Varint encoder; 
  Pbrt.Encoder.int64_as_varint v.filename encoder;
  Pbrt.Encoder.key 4 Pbrt.Varint encoder; 
  Pbrt.Encoder.int64_as_varint v.start_line encoder;
  Pbrt.Encoder.key 5 Pbrt.Varint encoder; 
  ()

let rec encode_pb_profile (v:profile) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_value_type x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.sample_type encoder;
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_sample x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  ) v.sample encoder;
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_mapping x encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  ) v.mapping encoder;
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_location x encoder;
    Pbrt.Encoder.key 4 Pbrt.Bytes encoder; 
  ) v.location encoder;
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_function_ x encoder;
    Pbrt.Encoder.key 5 Pbrt.Bytes encoder; 
  ) v.function_ encoder;
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.string x encoder;
    Pbrt.Encoder.key 6 Pbrt.Bytes encoder; 
  ) v.string_table encoder;
  Pbrt.Encoder.int64_as_varint v.drop_frames encoder;
  Pbrt.Encoder.key 7 Pbrt.Varint encoder; 
  Pbrt.Encoder.int64_as_varint v.keep_frames encoder;
  Pbrt.Encoder.key 8 Pbrt.Varint encoder; 
  Pbrt.Encoder.int64_as_varint v.time_nanos encoder;
  Pbrt.Encoder.key 9 Pbrt.Varint encoder; 
  Pbrt.Encoder.int64_as_varint v.duration_nanos encoder;
  Pbrt.Encoder.key 10 Pbrt.Varint encoder; 
  begin match v.period_type with
  | Some x -> 
    Pbrt.Encoder.nested encode_pb_value_type x encoder;
    Pbrt.Encoder.key 11 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  Pbrt.Encoder.int64_as_varint v.period encoder;
  Pbrt.Encoder.key 12 Pbrt.Varint encoder; 
  Pbrt.Encoder.nested (fun lst encoder ->
    Pbrt.List_util.rev_iter_with (fun x encoder -> 
      Pbrt.Encoder.int64_as_varint x encoder;
    ) lst encoder;
  ) v.comment encoder;
  Pbrt.Encoder.key 13 Pbrt.Bytes encoder; 
  Pbrt.Encoder.int64_as_varint v.default_sample_type encoder;
  Pbrt.Encoder.key 14 Pbrt.Varint encoder; 
  Pbrt.Encoder.int64_as_varint v.doc_url encoder;
  Pbrt.Encoder.key 15 Pbrt.Varint encoder; 
  ()

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Decoding} *)

let rec decode_pb_value_type d =
  let v = default_value_type_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.type_ <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(value_type), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.unit_ <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(value_type), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    type_ = v.type_;
    unit_ = v.unit_;
  } : value_type)

let rec decode_pb_label d =
  let v = default_label_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.key <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(label), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.str <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(label), field(2)" pk
    | Some (3, Pbrt.Varint) -> begin
      v.num <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(label), field(3)" pk
    | Some (4, Pbrt.Varint) -> begin
      v.num_unit <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(label), field(4)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    key = v.key;
    str = v.str;
    num = v.num;
    num_unit = v.num_unit;
  } : label)

let rec decode_pb_sample d =
  let v = default_sample_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.label <- List.rev v.label;
      v.value <- List.rev v.value;
      v.location_id <- List.rev v.location_id;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.location_id <- Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.int64_as_varint d)::l) [] d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(sample), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.value <- Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.int64_as_varint d)::l) [] d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(sample), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.label <- (decode_pb_label (Pbrt.Decoder.nested d)) :: v.label;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(sample), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    location_id = v.location_id;
    value = v.value;
    label = v.label;
  } : sample)

let rec decode_pb_mapping d =
  let v = default_mapping_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.id <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(mapping), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.memory_start <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(mapping), field(2)" pk
    | Some (3, Pbrt.Varint) -> begin
      v.memory_limit <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(mapping), field(3)" pk
    | Some (4, Pbrt.Varint) -> begin
      v.file_offset <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(mapping), field(4)" pk
    | Some (5, Pbrt.Varint) -> begin
      v.filename <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(mapping), field(5)" pk
    | Some (6, Pbrt.Varint) -> begin
      v.build_id <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(mapping), field(6)" pk
    | Some (7, Pbrt.Varint) -> begin
      v.has_functions <- Pbrt.Decoder.bool d;
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(mapping), field(7)" pk
    | Some (8, Pbrt.Varint) -> begin
      v.has_filenames <- Pbrt.Decoder.bool d;
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(mapping), field(8)" pk
    | Some (9, Pbrt.Varint) -> begin
      v.has_line_numbers <- Pbrt.Decoder.bool d;
    end
    | Some (9, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(mapping), field(9)" pk
    | Some (10, Pbrt.Varint) -> begin
      v.has_inline_frames <- Pbrt.Decoder.bool d;
    end
    | Some (10, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(mapping), field(10)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    id = v.id;
    memory_start = v.memory_start;
    memory_limit = v.memory_limit;
    file_offset = v.file_offset;
    filename = v.filename;
    build_id = v.build_id;
    has_functions = v.has_functions;
    has_filenames = v.has_filenames;
    has_line_numbers = v.has_line_numbers;
    has_inline_frames = v.has_inline_frames;
  } : mapping)

let rec decode_pb_line d =
  let v = default_line_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.function_id <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(line), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.line <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(line), field(2)" pk
    | Some (3, Pbrt.Varint) -> begin
      v.column <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(line), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    function_id = v.function_id;
    line = v.line;
    column = v.column;
  } : line)

let rec decode_pb_location d =
  let v = default_location_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.line <- List.rev v.line;
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.id <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(location), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.mapping_id <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(location), field(2)" pk
    | Some (3, Pbrt.Varint) -> begin
      v.address <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(location), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.line <- (decode_pb_line (Pbrt.Decoder.nested d)) :: v.line;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(location), field(4)" pk
    | Some (5, Pbrt.Varint) -> begin
      v.is_folded <- Pbrt.Decoder.bool d;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(location), field(5)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    id = v.id;
    mapping_id = v.mapping_id;
    address = v.address;
    line = v.line;
    is_folded = v.is_folded;
  } : location)

let rec decode_pb_function_ d =
  let v = default_function__mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.id <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(function_), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.name <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(function_), field(2)" pk
    | Some (3, Pbrt.Varint) -> begin
      v.system_name <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(function_), field(3)" pk
    | Some (4, Pbrt.Varint) -> begin
      v.filename <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(function_), field(4)" pk
    | Some (5, Pbrt.Varint) -> begin
      v.start_line <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(function_), field(5)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    id = v.id;
    name = v.name;
    system_name = v.system_name;
    filename = v.filename;
    start_line = v.start_line;
  } : function_)

let rec decode_pb_profile d =
  let v = default_profile_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.comment <- List.rev v.comment;
      v.string_table <- List.rev v.string_table;
      v.function_ <- List.rev v.function_;
      v.location <- List.rev v.location;
      v.mapping <- List.rev v.mapping;
      v.sample <- List.rev v.sample;
      v.sample_type <- List.rev v.sample_type;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.sample_type <- (decode_pb_value_type (Pbrt.Decoder.nested d)) :: v.sample_type;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(profile), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.sample <- (decode_pb_sample (Pbrt.Decoder.nested d)) :: v.sample;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(profile), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.mapping <- (decode_pb_mapping (Pbrt.Decoder.nested d)) :: v.mapping;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(profile), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.location <- (decode_pb_location (Pbrt.Decoder.nested d)) :: v.location;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(profile), field(4)" pk
    | Some (5, Pbrt.Bytes) -> begin
      v.function_ <- (decode_pb_function_ (Pbrt.Decoder.nested d)) :: v.function_;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(profile), field(5)" pk
    | Some (6, Pbrt.Bytes) -> begin
      v.string_table <- (Pbrt.Decoder.string d) :: v.string_table;
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(profile), field(6)" pk
    | Some (7, Pbrt.Varint) -> begin
      v.drop_frames <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(profile), field(7)" pk
    | Some (8, Pbrt.Varint) -> begin
      v.keep_frames <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(profile), field(8)" pk
    | Some (9, Pbrt.Varint) -> begin
      v.time_nanos <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (9, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(profile), field(9)" pk
    | Some (10, Pbrt.Varint) -> begin
      v.duration_nanos <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (10, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(profile), field(10)" pk
    | Some (11, Pbrt.Bytes) -> begin
      v.period_type <- Some (decode_pb_value_type (Pbrt.Decoder.nested d));
    end
    | Some (11, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(profile), field(11)" pk
    | Some (12, Pbrt.Varint) -> begin
      v.period <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (12, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(profile), field(12)" pk
    | Some (13, Pbrt.Bytes) -> begin
      v.comment <- Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.int64_as_varint d)::l) [] d;
    end
    | Some (13, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(profile), field(13)" pk
    | Some (14, Pbrt.Varint) -> begin
      v.default_sample_type <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (14, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(profile), field(14)" pk
    | Some (15, Pbrt.Varint) -> begin
      v.doc_url <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (15, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(profile), field(15)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    sample_type = v.sample_type;
    sample = v.sample;
    mapping = v.mapping;
    location = v.location;
    function_ = v.function_;
    string_table = v.string_table;
    drop_frames = v.drop_frames;
    keep_frames = v.keep_frames;
    time_nanos = v.time_nanos;
    duration_nanos = v.duration_nanos;
    period_type = v.period_type;
    period = v.period;
    comment = v.comment;
    default_sample_type = v.default_sample_type;
    doc_url = v.doc_url;
  } : profile)

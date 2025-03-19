type t
type line 
type location 
type function_ 

val create : int -> string -> t
val register_string : t -> string -> int64  
val register_function : t -> string -> string -> int64 

(*let update_locs (bt: raw_backtrace_entry) t = 
  (* first check if we have already seen this entry *)
  if not (RawBacktraceEntryTable.mem t.loc_table bt) then 
    (* iterate over every "slot" in a frame
      One backtrace_entry (i.e. location) can correspond to
      multiple slots (i.e. lines) due to inlining  *)
    let is_folded = ref false in
    let lines = ref [] in
    let slots = backtrace_slots_of_raw_entry bt in
    Array.iter (fun slot ->
      match Slot.location slot with
      | None -> ()
      | Some { filename; line_number; start_char; _ } ->
        (* this may or may not be correct *)
        is_folded <-  is_inline slot;
        let function_name = match Slot.name slot with Some n -> n | _ -> "??" in
        (* check if we have seen this function *)
        let function_id = match Hashtbl.find_opt t.functions function_name with
          | Some fn_id -> fn_id
          | None -> register_function t function_name filename 
        in let new_line = { function_id; line = Int64.of_int line_number; column = Int64.of_int start_char } in
        lines := new_line :: !lines;
    ) slots;
    RawBacktraceEntryTable.add t.loc_table bt ();
    (* add the location to the location list *)
    let entry_as_int = Int64.of_int (bt :> int) in
    t.locations := { id = entry_as_int; mapping_id = 1L; address = get_next_addr (); line = !lines; is_folded; } :: !(t.locations);
    entry_as_int
  else
    Int64.of_int (bt :> int)*)

(* encode the "number of samples" and "count" sample type *)
(*let encode_sample_type1 () encoder =
  Pbrt.Encoder.int64_as_varint 1L encoder;
  Pbrt.Encoder.key 1 Pbrt.Varint encoder; 
  Pbrt.Encoder.int64_as_varint 2L encoder;
  Pbrt.Encoder.key 2 Pbrt.Varint encoder; 

(* encode the "size" and "bytes" sample type *)
let encode_sample_type2 () encoder =
  Pbrt.Encoder.int64_as_varint 3L encoder;
  Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  Pbrt.Encoder.int64_as_varint 4L encoder;
  Pbrt.Encoder.key 2 Pbrt.Varint encoder; 

let encode_period_type () encoder =
  Pbrt.Encoder.int64_as_varint 9L encoder;
  Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  Pbrt.Encoder.int64_as_varint 10L encoder;
  Pbrt.Encoder.key 2 Pbrt.Varint encoder; 

let encode_sample_types t =
  Pbrt.Encoder.nested encode_sample_type1 () t.encoder;
  Pbrt.Encoder.key 1 Pbrt.Bytes encoder; (* Field 1: Sample Type *)
  Pbrt.Encoder.nested encode_sample_type2 () t.encoder;
  Pbrt.Encoder.key 1 Pbrt.Bytes encoder; (* Field 1: Sample Type*)

let encode_values t =
  Pbrt.Encoder.int64_as_varint (Int64.of_int info.n_samples) encoder;
  Pbrt.Encoder.int64_as_varint (Int64.of_int info.size) encoder;

let encode_label src encoder = 
  Pbrt.Encoder.int64_as_varint 5L encoder;
  Pbrt.Encoder.key 1 Pbrt.Varint encoder;
  let src = match src with
    | Major -> 6L
    | Minor -> 7L
    | External -> 8L
    | _ -> 0L
  in  
  Pbrt.Encoder.int64_as_varint src encoder;
  ()*)

val encode_sample : t -> Gc.Memprof.allocation -> Trace.Allocation_source.t -> int
val encode_line : line -> Pbrt.Encoder.t -> unit
val encode_loc : location -> Pbrt.Encoder.t -> unit
val encode_locations : Pbrt.Encoder.t -> unit
val encode_function : function_ -> Pbrt.Encoder.t -> unit
val encode_functions : t -> unit
val encode_string_table : t -> unit
val encode_metadata : t -> int64 -> unit

val encode_promote : t -> int -> unit
val encode_collect : t -> int -> unit

val close : t -> unit

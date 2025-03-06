(* Printing for debugging *)
open Memtrace.Profile

let label_to_string string_table label = 
  Printf.sprintf "key: %s\n, str: %s\n, num: num_unit:\n" (List.nth !string_table (Int64.to_int label.key)) (List.nth !string_table (Int64.to_int label.str))

let sample_to_string string_table sample =
  let locs = List.map Int64.to_string sample.location_id in
  let vals = List.map Int64.to_string sample.value in
  let labels = List.map (label_to_string string_table) sample.label in
  Printf.sprintf "Locations: %s, Values: %s, Label: %s\n" (String.concat ", " locs) (String.concat ", " vals) (String.concat ", " labels)

let mapping_to_string (m : mapping) = Printf.sprintf "id: %Ld, memory_start: %Ld, memory_limit: %Ld, file_offset: %Ld, filename: %Ld, build_id: %Ld has_functions: %B, has_filenames: %B, has_line_numbers: %B, has_inline_frames: %B\n" m.id m.memory_start m.memory_limit m.file_offset m.filename m.build_id m.has_functions m.has_filenames m.has_line_numbers m.has_inline_frames 

let line_to_string (l : line) = Printf.sprintf "function_id: %Ld, line: %Ld, column: %Ld" l.function_id l.line l.column

let location_to_string (l : location) = Printf.sprintf "id: %Ld, mapping_id: %Ld, address: %Ld, lines:\n%s\n, is_folded: %B" l.id l.mapping_id l.address (String.concat ",\n" (List.map (line_to_string) l.line)) l.is_folded 

let function_to_string string_table f = Printf.sprintf "id: %Ld, name: %s, system_name: %s, filename: %s, start_line: %Ld" f.id (List.nth !string_table (Int64.to_int f.name)) (List.nth !string_table (Int64.to_int f.system_name)) (List.nth !string_table (Int64.to_int f.filename)) f.start_line

let vt_to_string string_table vt = 
  Printf.sprintf "type_: %s, unit: %s" (List.nth !string_table (Int64.to_int vt.type_)) (List.nth !string_table (Int64.to_int vt.unit_))

let profile_to_string profile =
  let string_table = ref profile.string_table in
  let s = "\n--- Profile ---\n" ^ 
  "Sample Types:\n" ^ 
  (String.concat ";\n" (List.map (vt_to_string string_table) profile.sample_type)) ^
  "Samples:\n" ^ 
  (String.concat ";\n" (List.map (sample_to_string string_table) profile.sample)) ^
  "Mappings:\n" ^ 
  (String.concat ";\n" (List.map mapping_to_string profile.mapping)) ^
  "Locations:\n" ^ 
  (String.concat ";\n" (List.map location_to_string profile.location)) ^
  "Functions:\n" ^ 
  (String.concat ";\n" (List.map (function_to_string string_table) profile.function_)) ^
  "String Table:\n  [" ^
  (String.concat ", " profile.string_table) ^ "]\n" ^
  Printf.sprintf "drop_frames: %Ld, keep_frames: %Ld, time_nanos: %Ld, duration_nanos: %Ld, period: %Ld, default_sample_type: %Ld, doc_url: %Ld\n" profile.drop_frames profile.keep_frames profile.time_nanos
  profile.duration_nanos profile.period profile.default_sample_type
  profile.doc_url in s

let () = 
  if Array.length Sys.argv <> 2 then
    Printf.fprintf stderr "Usage: %s <proto file>\n" Sys.executable_name
  else
    let bytes = 
      let ic = open_in Sys.argv.(1) in 
      let len = in_channel_length ic in 
      let bytes = Bytes.create len in 
      really_input ic bytes 0 len; 
      close_in ic; 
      bytes 
    in 
    (* Decode the profile and print it *)
    let decoded_prof = (decode_pb_profile (Pbrt.Decoder.of_bytes bytes)) in 
    Printf.printf "%s" (profile_to_string decoded_prof)

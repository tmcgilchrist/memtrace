open Profile
open Trace


let loc_map = Hashtbl.create 100 (* maps location_ids to locations *)

let fn_ids = ref [""] (* a list of function ids  *)

let get_or_add_string s str_table =
  match List.find_index ((=) s) !str_table with
  | Some idx -> Int64.of_int idx
  | None ->
    str_table := !str_table @ [s];
    Int64.of_int (List.length !str_table - 1)

let get_or_add_fnid f fun_table =
  match List.find_index ((=) f) !fun_table with
    | Some idx -> (Int64.of_int idx, true)
    | None ->
      fun_table := !fun_table @ [f];
      (Int64.of_int (List.length !fun_table - 1), false)

let loc_to_int (loc_code : Location_code.t) = Int64.of_int (loc_code :> int)
let micro_to_nanoseconds s = Int64.mul s 1000L

(* ------------------- *)
(* Printing for debugging *)

let label_to_string string_table label = 
  Printf.sprintf "key: %s\n, str: %s\n, num: num_unit:" (List.nth !string_table (Int64.to_int label.key)) (List.nth !string_table (Int64.to_int label.str))

let sample_to_string sample string_table =
  let locs = List.map Int64.to_string sample.location_id in
  let vals = List.map Int64.to_string sample.value in
  let labels = List.map (label_to_string string_table) sample.label in
  Printf.sprintf "Locations: %s, Values: %s, Label: %s" (String.concat ", " locs) (String.concat ", " vals) (String.concat ", " labels)

let print_value_type vt =
  Printf.printf "  type: %Ld, unit: %Ld\n" vt.type_ vt.unit_

let print_label lbl =
  Printf.printf "  key: %Ld, str: %Ld, num: %Ld, num_unit: %Ld\n"
    lbl.key lbl.str lbl.num lbl.num_unit

let print_sample s =
  Printf.printf "  location_id: [%s], value: [%s]\n"
    (String.concat ", " (List.map Int64.to_string s.location_id))
    (String.concat ", " (List.map Int64.to_string s.value));
  List.iter print_label s.label

let print_mapping (m : mapping) =
  Printf.printf "  id: %Ld, memory_start: %Ld, memory_limit: %Ld, file_offset: %Ld, filename: %Ld, build_id: %Ld\n"
    m.id m.memory_start m.memory_limit m.file_offset m.filename m.build_id

let print_line l =
  Printf.printf "  function_id: %Ld, line: %Ld, column: %Ld\n" l.function_id l.line l.column

let print_location (loc : location) =
  Printf.printf "  id: %Ld, mapping_id: %Ld, address: %Ld, is_folded: %b\n"
    loc.id loc.mapping_id loc.address loc.is_folded;
  List.iter print_line loc.line

let print_function f =
  Printf.printf "  id: %Ld, name: %Ld, system_name: %Ld, filename: %Ld, start_line: %Ld\n"
    f.id f.name f.system_name f.filename f.start_line

let print_profile profile =
  Printf.printf "\n--- Profile ---\n";
  Printf.printf "Sample Types:\n";
  List.iter print_value_type profile.sample_type;
  Printf.printf "Samples:\n";
  List.iter print_sample profile.sample;
  Printf.printf "Mappings:\n";
  List.iter print_mapping profile.mapping;
  Printf.printf "Locations:\n";
  List.iter print_location profile.location;
  Printf.printf "Functions:\n";
  List.iter print_function profile.function_;
  Printf.printf "String Table:\n  [%s]\n"
    (String.concat ", " profile.string_table);
  Printf.printf "drop_frames: %Ld, keep_frames: %Ld, time_nanos: %Ld, duration_nanos: %Ld, period: %Ld, default_sample_type: %Ld, doc_url: %Ld\n"
    profile.drop_frames profile.keep_frames profile.time_nanos
    profile.duration_nanos profile.period profile.default_sample_type
    profile.doc_url

(* for now this is copied from dump trace*)
let print_ev ev reader = 
  match ev with 
    | Event.Alloc {obj_id; length; nsamples; source; backtrace_buffer; backtrace_length; common_prefix} ->
      let src =
        match source with
        | Minor -> "alloc"
        | Major -> "alloc_major"
        | External -> "alloc_ext" in
      Printf.printf "%010d %s %d len=%d % 4d:" (obj_id :> int) src nsamples length common_prefix;
      let print_location ppf loc =
        Printf.fprintf ppf "%s" (Location.to_string loc) in
      for i = 0 to backtrace_length - 1 do
        let s = backtrace_buffer.(i) in
        match Reader.lookup_location_code reader s with
        | [] -> Printf.printf " $%d" (s :> int)
        | ls -> ls |> List.iter (Printf.printf " %a" print_location)
      done;
      Printf.printf "\n%!"; ()
    | Event.Promote id ->
      Printf.printf "%010d promote\n" (id :> int); ()
    | Event.Collect id ->
      Printf.printf "%010d collect\n" (id :> int); ()

(* ------------------- *)

(* takes CTF location codes and creates locations *)
let update_locs reader buf functions locations string_table =
  let backtrace_buffer = Array.to_list buf in
      
  (* For each location code in the backtrace, 
    create location obj and add this to loc_map *)
  Array.iter (fun loc_code ->
    (* check if we have already seen this location code *)
    if Hashtbl.mem loc_map loc_code then
      ()
    else 
      (* create lines *)
      let lines = ref [] in
      let ctf_locs = Reader.lookup_location_code reader loc_code in
     
      (* each (ctf) location code can map to multiple (ctf) locations due to inlining) *)
      List.iter (fun (ctf_loc: Location.t) ->
        (* check if function entry exists *)
        let fn_id = ref Int64.zero in
        match get_or_add_fnid ctf_loc.defname fn_ids with  
          | (f, true) -> fn_id := f;
          | (f, false) -> fn_id := f;
            (* Create function entry *)
            functions := !functions @ [{
              id = !fn_id;
              name = get_or_add_string ctf_loc.defname string_table;
              system_name = 0L; (* No mangled names in CTF *)
              filename = get_or_add_string ctf_loc.filename string_table;
              start_line = Int64.of_int ctf_loc.line;
            }];
          
        (* Create line info *)
        let line_info = {
          function_id = !fn_id;
          line = Int64.of_int ctf_loc.line;
          column = Int64.of_int ctf_loc.start_char;
        } in

        lines := !lines @ [line_info];
      ) ctf_locs;
     
      (* Create and add location *)
      let loc = {
        id = Int64.of_int (loc_code :> int);
        mapping_id = 0L; (* not sure now *)
        address = 0L;    (* not sure now *)
        line = !lines;
        is_folded = false; (* not sure now *)
      } in
      (* add loc to location list and 
        loc_code / loc pair to loc_map *)
      locations := !locations @ [loc];
      Hashtbl.add loc_map loc_code loc;
    ) buf;
   (List.map loc_to_int backtrace_buffer)




let convert_events filename sample_rate time_end =
  let samples = ref [] in
  let string_table = ref [""; "source"; "minor"; "major"; "external"] in 
  let locations = ref [] in
  let functions = ref [] in (* a list of functions *) 
  (* the first value is the number of samples
  the second value is allocation size *)
  let sample_types = [
  { type_ = get_or_add_string "samples" string_table; unit_ = get_or_add_string "count" string_table };
  { type_ = get_or_add_string "length" string_table; unit_ = get_or_add_string "bytes" string_table } (* confirm unit *)
  ] in 
  let period_type = { type_ = get_or_add_string "space" string_table; unit_ = get_or_add_string "words" string_table } in
  let reader = Reader.open_ ~filename in
  let start_time = micro_to_nanoseconds (Timestamp.to_int64 ((Reader.info reader).start_time)) in 
  let end_time = micro_to_nanoseconds (Int64.of_float time_end) in
  let duration = Int64.sub start_time end_time in
  Printf.printf "Starting iterating events ...";

  Reader.iter reader (fun _ ev ->
    (*  not sure what to do with this time info for now 
    duration := Timedelta.to_int64 time; *)
    Printf.printf "Convering event: \n";
    print_ev ev reader;
    match ev with
    | Alloc { length; nsamples; source; backtrace_buffer; _ } -> 
      let loc_ids = update_locs reader backtrace_buffer functions locations string_table in
      let vals = [Int64.of_int nsamples; Int64.of_int length] in
      (* unsure where else to put this *)
      let str_val = match source with
        | Minor -> 2L
        | Major -> 3L
        | External -> 4L
      in
      let label = {
        key = 1L;
        str = str_val;
        num = 0L;
        num_unit = 0L
      } in 
      let new_sample = { location_id = loc_ids; value = vals; label = [label] } in
      samples := !samples @ [new_sample];
      Printf.printf "sample added to list: %s\n" (sample_to_string new_sample string_table);
    | Promote _ -> ()
    | Collect _ -> ()
    );
  Reader.close reader;
  {
    sample_type = sample_types;
    sample = !samples;
    mapping = [];  (* unsure *)
    location = !locations;
    function_ = !functions; 
    string_table = !string_table;
    drop_frames = 0L; (* unsure *)
    keep_frames = 0L; (* unsure *)
    time_nanos = start_time; (* unsure *)
    duration_nanos = duration; (* unsure *)
    period_type = Some period_type; 
    period = Int64.of_float (1.0 /. sample_rate); 
    comment = []; (* unsure *)
    default_sample_type = 0L; (* unsure *)
    doc_url = 0L (* unsure *)
  }

(* Main conversion function *)
let convert_file fd output_file sample_rate time_end =
  Printf.printf "Converting trace file ...";
  let profile = convert_events fd sample_rate time_end in
  (*let mapping = create_main_mapping profile in*)
  
  (* Write protobuf output *)
  let out_fd = Unix.openfile output_file [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o644 in
  let encoder = Pbrt.Encoder.create () in
  encode_pb_profile profile encoder;
  let bytes = Pbrt.Encoder.to_bytes encoder in
  Printf.printf "Encoded profile size: %d bytes\n" (Bytes.length bytes);
  for i = 0 to min 50 (Bytes.length bytes - 1) do
    Printf.printf "%02x " (Bytes.get_uint8 bytes i)
  done;
  Printf.printf "\n";
  let _ = Unix.write out_fd bytes 0 (Bytes.length bytes) in
  
  Unix.close out_fd;

  let bytes = 
    let ic = open_in output_file in 
    let len = in_channel_length ic in 
    let bytes = Bytes.create len in 
    really_input ic bytes 0 len; 
    close_in ic; 
    bytes 
  in 
  
  (* Decode the person and Pretty-print it *)
  print_profile (decode_pb_profile (Pbrt.Decoder.of_bytes bytes))


    (*let start_time = (Reader.info trace).start_time in
  Printf.printf "Start time: %Ld\n" (Timestamp.to_int64 start_time);*)
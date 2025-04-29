open Profile
open Trace


(* using dummy info for now, as we dont have mapping
 information. Addresses are non-zero so pprof does 
 not throw errors *)
let start_addr = 0x7F00000000L
let end_addr = 0x7F40000000L
let offset = 20L
let curr_addr = ref 0x7F0000000L
let get_next_addr () =
  curr_addr := Int64.add !curr_addr offset;
  !curr_addr

let loc_map = Hashtbl.create 100 (* maps location_ids to locations *)
let fn_map = Hashtbl.create 100 (* maps function names to functions *)
let next_fnid = ref 1L

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

let malformed_traces = ref 0
exception Malformed_trace of string

(* without this mapping, pprof cannot 
find the main binary/executable name. But we don't have actual mapping info so we use a dummy *)
let create_dummy_mapping reader string_table = {
  id = 1L;
  memory_start = start_addr;
  memory_limit = end_addr;
  file_offset = 0L; 
  filename = get_or_add_string ((Reader.info reader).executable_name) string_table;
  build_id = 0L;
  has_functions = false;
  has_filenames = false;
  has_line_numbers = false;
  has_inline_frames = false
}

let loc_to_int (loc_code : Location_code.t) = Int64.of_int (loc_code :> int)
let micro_to_nanoseconds s = Int64.mul s 1000L

(* takes CTF location codes and creates pprof locations *)
let update_locs reader buf len functions locations string_table =
  if len > Array.length buf then
    (malformed_traces := !malformed_traces + 1;
    raise (Malformed_trace "Backtrace buffer length exceeds buffer size"))
  else
    (* truncate the buffer to the length of the backtrace *)
    let truncated_buf = Array.sub buf 0 len in
    let backtrace_buffer = Array.to_list truncated_buf in

    (* For each location code in the backtrace,
      create location obj and add this to loc_map *)
    Array.iter (fun loc_code ->
      (* check if we have already seen this location code
          OR it is a reserved loc code *)
      if (Hashtbl.mem loc_map loc_code) || (loc_to_int loc_code = 0L) then
        ()
      else
        let lines = ref [] in
        (* each (ctf) location code can map to multiple (ctf) locations due to inlining) *)
        let ctf_locs = Reader.lookup_location_code reader loc_code in
        List.iter (fun (ctf_loc: Location.t) ->
          (* check if function entry exists *)
          let fn_id = ref Int64.zero in
          let fn_name = ctf_loc.defname ^ "_" ^ ctf_loc.filename in
          match Hashtbl.find_opt fn_map fn_name with
            | Some f -> fn_id := f
            | None -> (
              let f = !next_fnid in
              next_fnid := Int64.add f 1L;
              fn_id := f;
              functions := !functions @ [{
                id = !fn_id;
                name = get_or_add_string ctf_loc.defname string_table;
                system_name = 0L; (* No mangled names in CTF *)
                filename = get_or_add_string ctf_loc.filename string_table;
                start_line = Int64.of_int ctf_loc.line;
              }]);

          let line_info = {
            function_id = !fn_id;
            line = Int64.of_int ctf_loc.line;
            column = Int64.of_int ctf_loc.start_char;
          } in

          lines := !lines @ [line_info];
        ) ctf_locs;

        (* Create and add location 
          NOTE: Some (CTF) location codes map to an empty list, memtrace ignores them so we do too *)
        let loc = {
          id = Int64.of_int (loc_code :> int);
          mapping_id = 1L; (* dummy mapping for now *)
          address = get_next_addr (); (* dummy addr for now *)
          line = !lines;
          is_folded = (List.length !lines > 1);
        } in
        (* add loc to location list and
          loc_code / loc pair to loc_map *)
        locations := !locations @ [loc];
        Hashtbl.add loc_map loc_code loc;
      ) truncated_buf;
    List.rev (List.map loc_to_int backtrace_buffer)

let convert_events filename =
  let samples = ref [] in
  let string_table = ref [""; "source"; "minor"; "major"; "external"] in
  let locations = ref [] in
  let functions = ref [] in 
  let sample_types = [
  { type_ = get_or_add_string "num_samples" string_table; unit_ = get_or_add_string "count" string_table };
  { type_ = get_or_add_string "alloc_size" string_table; unit_ = get_or_add_string "bytes" string_table } (* confirm unit !! *)
  ] in
  let period_type = { type_ = get_or_add_string "space" string_table; unit_ = get_or_add_string "words" string_table } in
  let reader = Reader.open_ ~filename in
  let info = Reader.info reader in
  let word_size = info.word_size / 8 in
  let start_time = micro_to_nanoseconds (Timestamp.to_int64 info.start_time) in
  let time_end = ref 0L in
  Reader.iter reader (fun time_delta ev ->
    match ev with
    | Alloc { length; nsamples; source; backtrace_buffer; backtrace_length; _ } ->
      (try 
        let loc_ids = update_locs reader backtrace_buffer backtrace_length functions locations string_table in
        let size_in_bytes = length * word_size in
        let vals = [Int64.of_int nsamples; Int64.of_int size_in_bytes] in
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
        time_end := Timedelta.to_int64 time_delta;
        samples := !samples @ [new_sample]
      with
      | Malformed_trace _ ->
        time_end := Timedelta.to_int64 time_delta;
        ()
      | _ -> ())
    | Promote _ ->
      time_end := Timedelta.to_int64 time_delta;
      ()
    | Collect _ ->
      time_end := Timedelta.to_int64 time_delta;
      ()
    );

  let end_time = micro_to_nanoseconds (!time_end) in
  let duration = Int64.sub start_time end_time in
  let dummy_mapping = create_dummy_mapping reader string_table in
  Reader.close reader;
  {
    sample_type = sample_types;
    sample = !samples;
    mapping = [dummy_mapping]; 
    location = !locations;
    function_ = !functions;
    string_table = !string_table;
    (* Use these fields to specify function names we want to ignore from or keep in stack traces. Currently not used but could be useful in ignoring internal functions or functions related to the trace writer itself. *)
    drop_frames = 0L; 
    keep_frames = 0L; 
    time_nanos = start_time; 
    duration_nanos = duration; 
    period_type = Some period_type;
    period = Int64.of_float (1.0 /. info.sample_rate);
    comment = []; 
    default_sample_type = 0L; 
    doc_url = 0L;
  }

let convert_file fd output_file =
  let profile = convert_events fd in

  if !malformed_traces > 0 then
    Printf.printf "Warning: %d malformed samples skipped\n" !malformed_traces;

  (* Write the profile to a file *)

  let out_fd = Unix.openfile output_file [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o644 in
  let encoder = Pbrt.Encoder.create () in
  encode_pb_profile profile encoder;
  let bytes = Pbrt.Encoder.to_bytes encoder in
  let _ = Unix.write out_fd bytes 0 (Bytes.length bytes) in

  Unix.close out_fd

(* Summary:
    - pprof uses mappings to resolve memory addresses to symbol information. For pure OCaml code, mappings are generally not required, since memtrace already provides symbolic information.
    - However, mappings can be useful if the program interacts with native OCaml runtime components or uses FFI libraries. Unfortunately, memtrace doesn’t expose this mapping information directly, and supporting it would require emitting protobuf data in real time — https://github.com/grouptheoryiscool/memtrace/pull/2.
    - For now we use a dummy mapping, without which pprof cannot find the main binary name and produces an incomplete graph. Pprof also checks for non-zero addresses so we use random addresses using "get_next_addr ()".
    - Fields "keep_frames" and "drop_frames" are unused for now but may be useful later.
    - When viewing a flamegraph in pprof, you can right-click on a location and view it's source code: another feature that uses information from the mappings that are missing in our conversion tool. *)
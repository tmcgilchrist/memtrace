open Memtrace.Trace
open Memtrace.Profile
open QCheck2.Gen
open Unix

(* HELPERS *)

(* when shrinking, QCheck2 sometimes uses events generated from previous
 iterations, thus messing up the sequential order of obj_ids. Thus, we
 generate events without the id and fill this in later. *)
type event' =
  | Alloc of {
    length : int;
    nsamples : int;
    source : Allocation_source.t;
    backtrace_buffer : Location_code.t array;
    backtrace_length : int;
    common_prefix : int;
  }
  | Promote 
  | Collect 

let to_id : int -> Obj_id.t = Obj.magic

let to_event ev' counter = 
  match ev' with 
  | Alloc { length; nsamples; source; backtrace_buffer; backtrace_length; common_prefix } ->
    let obj_id = to_id !counter in
    let ev = Event.Alloc { obj_id; length; nsamples; source; backtrace_buffer; backtrace_length; common_prefix } in 
    incr counter; ev
  | Promote -> let obj_id = to_id (!counter-1) in Event.Promote obj_id
  | Collect -> let obj_id = to_id (!counter-1) in Event.Collect obj_id

let loc_code n : Location_code.t = Obj.magic (n + 1)

(* copied from test.ml *)
let mkloc filename line start_char end_char defname =
  Location.{ filename; line; start_char; end_char; defname }

let reasonable_locations =
  [ [mkloc "foo.ml" 42 100 120 "func"];
    [mkloc "filename.ml" 100 58 1023 "func"];
    [mkloc "filename.ml" 19 97 1023 "func"; mkloc "inline" 1 1 1 "fjkisda"];
    [mkloc "bar.ml" 21 90 105 "barfunc"; mkloc "inlined" 1 1 1 "inlinedcode"]]

let locations = Array.of_list reasonable_locations 

let decode_loc l = locations.((l : Location_code.t :> int) - 1)

(* Generators *)

let generate_fake_backtrace () =
  let len = Random.int 7 + 1 in
  Array.init len (fun _ -> loc_code (Random.int 4))

let gen_source =
  oneofl [ Allocation_source.Minor; Major; External ]

let gen_alloc_event =
  map2 (fun length source ->
    let buffer = generate_fake_backtrace () in
    let len = Array.length buffer in
    let backtrace_length = if len > 0 then 1 + Random.int len else 1 in
    let ev =
      Alloc {
        length;
        nsamples = 1;
        source;
        backtrace_buffer = buffer;
        backtrace_length;
        common_prefix = 0;  (* ignored in conversion *)
      }
    in 
    ev
  ) (int_bound 10_000) gen_source

let gen_collect_event = pure Collect 
let gen_promote_event = pure Promote 

let gen_event =
  frequency [
    4, gen_alloc_event;
    2, gen_collect_event;
    1, gen_promote_event;
  ]

let gen_events =
  list_size (int_range 100 300) gen_event

let dummy_info =
  {
    Info.sample_rate = 1.0;
    word_size = Sys.word_size;
    executable_name = "test";
    host_name = "localhost";
    ocaml_runtime_params = "";
    pid = Int64.of_int (Unix.getpid ());
    start_time = Timestamp.of_int64 23897423L;
    context = Some "fuzz";
  }

let write_ctf_file filename events =
  let fd = Unix.openfile filename [O_RDWR] 0o600 in
  try 
    (let writer = Writer.create fd dummy_info in
    let obj_id_counter = ref 0 in

    List.iteri (fun i ev' ->
      let ts = Int64.add (Timestamp.to_int64 dummy_info.start_time) (Int64.of_int (i * 1_000_000)) in
      let ev = to_event ev' obj_id_counter in
      Writer.put_event writer
        ~decode_callstack_entry:decode_loc
        (Timestamp.of_int64 ts) ev
    ) events;

    Writer.flush writer;
    Unix.close fd)
  with 
    | e -> Unix.close fd; raise e

let decode_profile_from_file fd = 
  let ic = open_in_bin fd in
  let len = in_channel_length ic in
  let buf = really_input_string ic len in close_in ic;
  let decoder = Pbrt.Decoder.of_bytes (Bytes.unsafe_of_string buf) in
  decode_pb_profile decoder

(** TESTS *)

let check_num_samples samples events =
  (* Get expected number of Alloc events *)
  let expected_allocs =
    List.fold_left (fun acc ev ->
      match ev with
      | Alloc _ -> acc + 1
      | _ -> acc
    ) 0 events
  in
  if not (List.length samples = expected_allocs) then 
    failwith (Printf.sprintf "Incorrect number of samples, expected: %d, got: %d" expected_allocs (List.length samples))

let take_first_n_backtraces events n =
  let rec aux acc n = function
    | [] -> List.rev acc
    | (Alloc { backtrace_buffer; backtrace_length; _ }) :: rest when n > 0 ->
      let backtrace = Array.to_list (Array.sub backtrace_buffer 0 backtrace_length) in
      aux (backtrace :: acc) (n - 1) rest
    | _ :: rest -> aux acc n rest
  in
  aux [] n events

let check_backtrace samples events n =
  let exp_backtraces = take_first_n_backtraces events n in 
  List.for_all2 (fun sample exp_backtrace -> 
    (* Pprof traces use the opposite order compared to CTF traces *)
    let protolocs = List.rev (List.map Int64.to_int sample.location_id) in 
    let ctflocs : int list = List.map (fun (l : Location_code.t) -> (l :> int)) exp_backtrace in
    if not (protolocs = ctflocs) then 
      failwith "Backtrace mismatch"
    else true
  ) samples exp_backtraces


let compare_loc_list ctflocs (protolocs : line list) (profile : profile) : bool =
  let functions = Array.of_list profile.function_ in
  let string_table = Array.of_list profile.string_table in
  List.for_all2 (fun (loc1 : Location.t) (loc2 : line)  ->
    let fn_id = Int64.to_int loc2.function_id in
    (* IDs are 1-based, so index is fn_id - 1 *)
    if fn_id <= 0 || fn_id > Array.length functions then
      failwith "Invalid function ID found"
    else
      let fn = functions.(fn_id - 1) in
      if not (fn.id = Int64.of_int fn_id) then false else
        let ctf_fnname = loc1.defname in
        let fname = string_table.(Int64.to_int fn.name) in
        if not (ctf_fnname = fname) then failwith "Function name mismatch";
        let ctf_filename  = loc1.filename in
        let filename = string_table.(Int64.to_int fn.filename) in 
        if not (ctf_filename = filename) then failwith "Filename mismatch";
        if not (loc1.line = (Int64.to_int loc2.line)) then failwith "Line number mismatch";
        if not (loc1.start_char = (Int64.to_int loc2.column)) then failwith "Start char mismatch";
        true
  ) ctflocs protolocs

let check_valid_backtraces (profile : profile) (location_by_id : location option array) (events: event' list) = 
  let n = 10 in 
  let samples_to_check = List.take n profile.sample in
  let check_backtrace_locs (sample : sample) =
  List.for_all (fun loc_id ->
    let loc_id = (Int64.to_int loc_id)-1 in
    let ctfloc = locations.(loc_id) in
    let protoloc = 
      match location_by_id.(loc_id) with
      | None -> failwith (Printf.sprintf "Location %d not found" loc_id)
      | Some loc -> loc.line 
    in 
    if List.length ctfloc <> List.length protoloc then
      failwith (Printf.sprintf "Location %d does not match" loc_id)
    else
      compare_loc_list ctfloc protoloc profile
  ) sample.location_id in
  (* check that the backtraces are correct *)
  check_backtrace samples_to_check events n &&
  (* check every location in the backtrace for every sample *)
  List.for_all check_backtrace_locs samples_to_check

let check_profile ~profile ~events =
  (* Order locations by ID, we have only generated 4 fake locations *)
  let location_by_id : location option array = Array.make 4 None in
  List.iter (fun (loc : location) ->
    let index = Int64.to_int loc.id in
    if index <= 0 || index > 4 then
      failwith "Invalid location ID found"
    else 
      location_by_id.(index - 1) <- Some loc
  ) profile.location;
  let samples = profile.sample in
  check_num_samples samples events;
  check_valid_backtraces profile location_by_id events

let conversion_test =
  QCheck2.Test.make ~count:1 gen_events @@ fun events -> 
  (* the first event cannot be promotion or collection *)
  let events = match events with
    | (Collect | Promote ) :: rest -> rest
    | _ -> events
  in
  let ctf_file = Filename.temp_file "memtrace" "ctf" in
  let pb_file = Filename.temp_file "memprof" "pb" in
  Fun.protect ~finally:(fun () -> 
    if Sys.file_exists ctf_file then Unix.unlink ctf_file;
    if Sys.file_exists pb_file then Unix.unlink pb_file)
  (fun () ->
    write_ctf_file ctf_file events;
    let convert_exe_path = Filename.concat (Sys.getenv "PWD") "_build/default/bin/convert.exe" in 
    let status = Sys.command (Printf.sprintf "%s %s %s" convert_exe_path ctf_file pb_file) in
    if status <> 0 then 
      failwith 
        (Printf.sprintf "Conversion failed with status %d" status)
    else
      (* decode profile and run tests *)
      let profile = decode_profile_from_file pb_file in 
      (* delete trace and pb files *)
      Unix.unlink ctf_file; Unix.unlink pb_file;
      check_profile ~profile ~events)

let () =
  QCheck_base_runner.run_tests_main [
    conversion_test
  ]

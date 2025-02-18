(* Generating Protobuf files *)

open Profile

(* Convert memtrace events into pprof samples *)
let convert_events reader =
  Printf.printf "hi";
  (* let samples = ref [] in
  let string_table = ref [""] in (* Index 0 is always empty string *)
  let get_or_add_string s =
    match List.find_index ((=) s) !string_table with
    | Some idx -> Int64.of_int idx
    | None ->
        string_table := !string_table @ [s];
        Int64.of_int (List.length !string_table - 1)
  in
  
  (* the first value is the number of samples,
    the second value is the size of the alloc *)
  let sample_types = [
    { type_ = get_or_add_string "nsamples"; unit_ = get_or_add_string "count" };
    { type_ = get_or_add_string "length"; unit_ = get_or_add_string "bytes" };
  ] in

  (* Track live allocations *)
  let live_allocs = Hashtbl.create 1024 in
  let alloc_sizes = Hashtbl.create 1024 in
  
  let process_event time info =
    match info with
    | Alloc {obj_id; length; nsamples; source; backtrace_buffer; backtrace_length; common_prefix} ->
        let location_ids = List.map Int64.of_int backtrace in
        (* Store size for later *)
        Hashtbl.add alloc_sizes time size;
        (* Create sample *)
        let sample = {
          location_id = location_ids;
          value = [Int64.of_int size; Int64.of_int 1L];
          label = []
        } in
        samples := sample :: !samples
        
    | `Collect {alloc_id} ->
        (* Look up original size *)
        begin match Hashtbl.find_opt alloc_sizes alloc_id with
        | Some size ->
            (* Create negative sample to cancel out allocation *)
            let sample = {
              location_id = [];  (* Could store dealloc trace if available *)
              value = [Int64.of_int (-size); Int64.of_int (-1)];
              label = []
            } in
            samples := sample :: !samples;
            Hashtbl.remove alloc_sizes alloc_id
        | None -> ()
        end
    
    | `Promote {alloc_id} ->
        (* Promotion events could be tracked separately if desired *)
        ()
  in

  (* Read all events *)
  Reader.iter reader process_event;

  (* Convert locations from reader into pprof format *)
  let locations = 
    Location_code.Tbl.fold (fun id loc acc ->
      let lines = match loc with
      | {Location. defname; filename; line; _} ->
          [{
            function_id = get_or_add_string defname;
            line = Int64.of_int line;
            column = 0L
          }]
      in
      {
        id = Int64.of_int id;
        mapping_id = 0L; (* Could populate mapping info if available *)
        address = 0L;
        line = lines;
        is_folded = false
      } :: acc
    ) reader.loc_table []
  in

  (* Build final profile *)
  {
    sample_type = sample_types;
    sample = !samples;
    mapping = [];  (* Could populate from debug info *)
    location = locations;
    function_ = [];  (* Could deduplicate function names *)
    string_table = !string_table;
    drop_frames = 0L;
    keep_frames = 0L;
    time_nanos = reader.info.start_time;
    duration_nanos = Int64.sub reader.info.end_time reader.info.start_time;
    period_type = None;
    period = 0L;
    comment = [];
    default_sample_type = 0L;
    doc_url = 0L
  }

(* Main conversion function *)
let convert_file filename =
  let fd = Unix.openfile filename [Unix.O_RDONLY] 0 in
  let reader = Reader.make_reader fd in
  let profile = convert_events reader in
  
  (* Write protobuf output *)
  let output_file = filename ^ ".pb" in
  let out_fd = Unix.openfile output_file [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o644 in
  let encoder = Pbrt.Encoder.create () in
  encode_pb_profile profile encoder;
  let bytes = Pbrt.Encoder.to_bytes encoder in
  let _ = Unix.write out_fd bytes 0 (Bytes.length bytes) in
  
  Unix.close fd;
  Unix.close out_fd*)


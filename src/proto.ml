open Buf

module RawBacktraceEntryTable = Hashtbl.Make(struct
  type t = Printexc.raw_backtrace_entry
  let equal (x: t) (y: t) = x = y
  let hash (x: t) = Hashtbl.hash x
end)

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

(* TODO: using dummy info for now, as we dont have mapping information.
  Add mapping info  *)
let start_addr = 0x7F00000000L
let end_addr = 0x7F40000000L
let offset = 16L
let curr_addr = ref 0x7F0000000L
let buffer_size = 65536 (* this will also change once we start streaming pprof data *)

let get_next_addr () =
  curr_addr := Int64.add !curr_addr offset;
  !curr_addr

(* TODO: make sure these numbers make sense, for now they are copied from Backtrace_codec, Location_codec and Trace *)
let max_loc = 4096
let max_backtrace = 4096
let max_ev = 100
let max_packet_size = 1 lsl 15
let max_strs = 4096

(* Strings need to be in order, so they are encoded only when flushed *)
module StringTable 

type writer = {
  dest : Unix.file_descr;
  pid : int64; 
  getpid : unit -> int64;
  sample_rate : float;
  start_time : int64;
  mutable next_alloc_id : int;

  (* I don't know if this is the most efficient way, works for now  *)
  new_strs : string Stack.t;
  mutable new_strs_len : int;
  new_strs_buf : Bytes.t;
  mutable strings : (string, unit) Hashtbl.t; 

  (* Function and location tracking *)
  mutable functions : function_ list ref;  
  mutable function_ids : (string, int64) Hashtbl.t;  (* Maps function name to ID *)
  mutable next_function_id : int64;
  
  mutable locations : location list ref;
  mutable loc_table : unit RawBacktraceEntryTable.t; (* track which locations we have seen before *)

  mutable packet : Write.t;
}

module Writer = struct
  type t = writer
  exception Pid_changed

  (* these strings are (probably) only accessed once *)
  let init_strtbl t =
    let s = t.new_strs in
    Stack.push "" s; 
    Stack.push "num_samples" s;
    Stack.push "count" s;
    Stack.push "alloc_size" s;
    Stack.push "bytes" s;
    Stack.push "source" s;
    Stack.push "minor" s;
    Stack.push "major" s;
    Stack.push "external" s;
    Stack.push "space" s;
    Stack.push "words" s;

  let[@inline] encode_nested f v t = 
    let old_start = Write.get_pos t.packet in
    f v t;
    let new_start = Write.get_pos t.packet in
    let size =  old_start - new_start in
    Write.int_as_varint size t.packet
    
  (* TODO: check - is this the write way to do it???? *)
  (* encode the "number of samples" and "count" sample type *)
  let encode_sample_type1 () t =
    Write.write_varint 1L t.packet;
    Write.key 1 Writer.Varint t.packet; 
    Write.write_varint 2L t.packet;
    Write.key 2 Writer.Varint t.packet; 

  (* encode the "alloc size" and "bytes" sample type *)
  let encode_sample_type1 () t =
    Write.write_varint 3L t.packet;
    Write.key 1 Writer.Varint t.packet; 
    Write.write_varint 4L t.packet;
    Write.key 2 Writer.Varint t.packet; 

  let flush t =
    if s.pid <> s.getpid () then raise Pid_changed;
    let open Write in
    (* Flush newly seen strings *)
    while not (Stack.is_empty t.new_strs) do
      let b = Write.of_bytes s.new_strs_buf in
      (* write until either we run out of buffer space or we finish writing all strings *)
      (* TODO: CHECK MAX_STRS SIZE*)
      while ((not (Stack.is_empty t.new_strs)) &&
      b.get_pos > max_strs) do
        let str = Stack.pop t.new_strs in
        string str b;
        key 6 Bytes b;
      done;
      write_fd t.dest b
    done;
    (* TODO: add location flushing *)
    (* Flush actual events *)
    write_fd t.dest t.packet;
    (* reset string, location and main buffers *)
    s.new_strs_len <- 0;
    s.packet <- Write.of_bytes s.packet.buf;
    
  (* Create a new proto writer *)
  let create dest ?getpid (info : Writer_helper.Info.t) = 
    (* TODO: check if this is correct *)
    let getpid = match getpid with
      | Some getpid -> getpid
      | None -> fun () -> info.pid in
    let pid = getpid () in

    let writer = {
      dest; pid; get_pid; 
      sample_rate = info.sample_rate; 
      start_time = (Int64.of_float info.start_time); 
      next_alloc_id = 0; 
      new_strs = Stack.create ();
      new_strs_len = 11; (* after adding initial strings *)
      new_strs_buf = Bytes.make max_packet_size '\042';
      strings = Hashtbl.create 100;
      functions = ref [];
      function_ids =  Hashtbl.create 100; 
      next_function_id = 1L; 
      locations = ref []; 
      loc_table = RawBacktraceEntryTable.create 100;
      packet = Write.of_bytes_proto (Bytes.make buffer_size '\042'); 
    } in 
    init_strtbl writer;
    encode_nested (encode_sample_type1) () writer;
    encode_nested (encode_sample_type2) () writer;
    writer
    

  (* convert raw backtraces to int64s *)
  (*let extract_pc_addresses (bt : Printexc.raw_backtrace) : int64 array =
    let n = Printexc.raw_backtrace_length bt in
    Array.init n (fun i -> Int64.of_int (Printexc.raw_backtrace_slot bt i))*)

  (* register a string and return its ID *)
  (*let register_string t s =
    match Hashtbl.find_opt t.strings s with
    | Some id -> Int64.of_int id
    | None ->
        let id = t.next_string_id in
        Hashtbl.add t.strings s id;
        t.next_string_id <- id + 1;
        Int64.of_int id

  (* Register a function and return its ID *)
  let register_function t name filename =
    let fn_id = t.next_function_id in
    t.next_function_id <- Int64.add fn_id 1L;
    functions := !functions @ [{
      id = fn_id;
      name = register_string t name;
      system_name = 0L; (* TODO: check *)
      filename = register_string t filename;
      start_line = 0L; (* TODO: check *)
    }];
    fn_id*)

  (* Extract location info from a raw backtrace and add it to the location array *)
  let update_locs (bt: raw_backtrace_entry) t = 
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
      Int64.of_int (bt :> int)

  (*let encode_period_type () encoder =
    Pbrt.Encoder.int64_as_varint 9L encoder;
    Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
    Pbrt.Encoder.int64_as_varint 10L encoder;
    Pbrt.Encoder.key 2 Pbrt.Varint encoder; 

  let encode_sample_types t =
    Pbrt.Encoder.nested encode_sample_type1 () t.encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; (* Field 1: Sample Type *)
    Pbrt.Encoder.nested encode_sample_type2 () t.encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; (* Field 1: Sample Type*)*)

  let encode_values t n_samples size =
    Pbrt.Encoder.int64_as_varint (Int64.of_int n_samples) t.encoder;
    Pbrt.Encoder.int64_as_varint (Int64.of_int size) t.encoder;

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

  let put_alloc_with_raw_backtrace t now ~length ~nsamples ~source ~callstack =
    let id = t.next_alloc_id in
    t.next_alloc_id <- id + 1;
    Pbrt.Encoder.nested (fun lst encoder ->
      Array.iter (fun x encoder -> 
        let entry = update_locs x t in
        Pbrt.Encoder.int64_as_varint entry encoder;
      ) lst;
    ) (Printexc.raw_backtrace_entries callstack) t.encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes t.encoder; (* Field 1 of sample: location ids *)
    Pbrt.Encoder.nested encode_values t.encoder nsamples length; 
    Pbrt.Encoder.key 2 Pbrt.Bytes t.encoder; (* Field 2 of sample: values *)
    Pbrt.Encoder.nested encode_label source t.encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes t.encoder; (* Field 3 of sample: labels *)
    id

  (*let encode_line (line : line) encoder = 
    Pbrt.Encoder.int64_as_varint line.function_id encoder;
    Pbrt.Encoder.key 1 Pbrt.Varint encoder; 
    Pbrt.Encoder.int64_as_varint line.line encoder;
    Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
    Pbrt.Encoder.int64_as_varint line.column encoder;
    Pbrt.Encoder.key 3 Pbrt.Varint encoder; 

  let encode_loc loc encoder =
    Pbrt.Encoder.int64_as_varint loc.id encoder;
    Pbrt.Encoder.key 1 Pbrt.Varint encoder; 
    Pbrt.Encoder.int64_as_varint loc.mapping_id encoder;
    Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
    Pbrt.Encoder.int64_as_varint loc.address encoder;
    Pbrt.Encoder.key 3 Pbrt.Varint encoder; 
    Pbrt.List_util.rev_iter_with (fun x encoder -> 
      Pbrt.Encoder.nested encode_line x encoder;
      Pbrt.Encoder.key 4 Pbrt.Bytes encoder; 
    ) loc.line encoder;
    Pbrt.Encoder.bool loc.is_folded encoder;
    Pbrt.Encoder.key 5 Pbrt.Varint encoder; 

  let encode_locations t =
    List.iter (fun x encoder ->
      Pbrt.Encoder.nested encode_loc x encoder;
      Pbrt.Encoder.key 4 Pbrt.Bytes encoder; (* Field 4 of Profile = locations *)
    ) t.locations t.encoder;

  let encode_function (f : function_) encoder = 
    Pbrt.Encoder.int64_as_varint f.id encoder;
    Pbrt.Encoder.key 1 Pbrt.Varint encoder; 
    Pbrt.Encoder.int64_as_varint f.name encoder;
    Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
    Pbrt.Encoder.int64_as_varint f.system_name encoder;
    Pbrt.Encoder.key 3 Pbrt.Varint encoder; 
    Pbrt.Encoder.int64_as_varint f.filename encoder;
    Pbrt.Encoder.key 4 Pbrt.Varint encoder; 
    Pbrt.Encoder.int64_as_varint f.start_line encoder;
    Pbrt.Encoder.key 5 Pbrt.Varint encoder; 

  let encode_functions t =
    List.iter (fun f encoder -> 
      Pbrt.Encoder.nested encode_function f encoder;
      Pbrt.Encoder.key 5 Pbrt.Bytes encoder; (* Field 5 of Profile = functions *)
    ) t.functions t.encoder;

  let encode_string_table t =
    Pbrt.List_util.rev_iter_with (fun x encoder -> 
      Pbrt.Encoder.string x encoder;
      Pbrt.Encoder.key 6 Pbrt.Bytes encoder; (* Field 6 of Profile = string table *)
    ) v.string_table encoder;

  (* writes all fields 9 onwards *)
  let encode_metadata t end_time =
    Pbrt.Encoder.int64_as_varint t.encoder 9 t.start_time;
    let duration = Int64.sub end_time t.start_time in
    Pbrt.Encoder.int64_as_varint t.encoder 10 duration;
    Pbrt.Encoder.nested encode_period_type () encoder;
    Pbrt.Encoder.key 11 Pbrt.Bytes encoder; 
    let period = Int64.of_float (1.0 /. t.sample_rate) in 
    Pbrt.Encoder.int64_as_varint period encoder;
    Pbrt.Encoder.key 12 Pbrt.Varint encoder; *)

  let put_collect t time id = Printf.printf "TODO: write collect events\n"; 
  let put_promote t time id = Printf.printf "TODO: write promote events\n"; 
  let encode_alloc_ext t id = Printf.printf "TODO: write external alloc events"; 

  let flush t = 
    if s.pid <> s.getpid () then raise Pid_changed;
    (* TODO: flush newly seen locations *)

  let close t =
    let end_time = Int64.of_float (Unix.gettimeofday () *. 1_000_000_000.) in
    
    (* Write the complete profile *)
    write_profile_header t;
    write_samples t;
    write_locations t;
    write_functions t;
    write_string_table t;
    write_metadata t end_time;
    
    (* Write the encoded data to file *)
    let data = Pbrt.Encoder.to_bytes t.encoder in
    let out = open_out_bin t.filename in
    output_bytes out data;
    close_out out;
end
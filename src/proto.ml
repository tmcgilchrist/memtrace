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
  line : line Stack.t;
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
(*let start_addr = 0x7F00000000L
let end_addr = 0x7F40000000L
let offset = 16L
let curr_addr = ref 0x7F0000000L*)
let buffer_size = 65536 (* this will also change once we start streaming pprof data *)

(*let get_next_addr () =
  curr_addr := Int64.add !curr_addr offset;
  !curr_addr*)

(* TODO: make sure these numbers make sense, for now they are copied from Backtrace_codec, Location_codec and Trace *)
let max_loc_size = 4096
(*let max_backtrace = 4096
let max_ev = 100*)
let max_packet_size = 1 lsl 15
let max_strs = 4096

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
  mutable strings : (string, int64) Hashtbl.t; 

  (* TODO: improve: we may not need to maintain function and location lists, but it's not possible to write them as soon as we see them either. *)

  (* Function tracking *)
  functions : function_ list ref;  
  mutable function_ids : (string, int64) Hashtbl.t;  (* Maps function name to ID *)
  mutable next_function_id : int64;
  
  (* Location tracking *)
  locations : location list ref;
  mutable new_locs_len : int;
  new_locs_buf : Bytes.t;
  mutable loc_table : unit RawBacktraceEntryTable.t; (* track which locations we have seen before *)

  mutable encoder : Write.t;
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
    Stack.push "words" s

  let[@inline] encode_nested f v t = 
    let old_start = Write.get_pos t.encoder in
    f v t;
    let new_start = Write.get_pos t.encoder in
    let size =  old_start - new_start in
    Write.int_as_varint size t.encoder;
    
  (* TODO: check - is this the right way to do it???? *)
  (* encode the "number of samples" and "count" sample type *)
  let encode_sample_type1 () t =
    Write.write_varint 1L t.encoder;
    Write.key 1 Writer.Varint t.encoder; 
    Write.write_varint 2L t.encoder;
    Write.key 2 Writer.Varint t.encoder; 

  (* encode the "alloc size" and "bytes" sample type *)
  let encode_sample_type2 () t =
    Write.write_varint 3L t.encoder;
    Write.key 1 Write.Varint t.encoder; 
    Write.write_varint 4L t.encoder;
    Write.key 2 Write.Varint t.encoder; 

  let encode_line (line : line) encoder = 
    Write.int64_as_varint line.function_id encoder;
    Write.key 1 Write.Varint encoder; 
    Write.int64_as_varint line.line encoder;
    Write.key 2 Write.Varint encoder; 
    Write.int64_as_varint line.column encoder;
    Write.key 3 Write.Varint encoder;

  let encode_loc loc encoder =
    let old_start = encoder.get_pos in
    Write.write_varint loc.id encoder;
    Write.key 1 Write.Varint encoder; 
    Write.write_varint loc.mapping_id encoder;
    Write.key 2 Write.Varint encoder; 
    Write.write_varint loc.address encoder;
    Write.key 3 Write.Varint encoder; 
    while not (Stack.is_empty loc.line) do
      (* every line is a nested field *)
      let start_bfr_line = encoder.get_pos in
      let line = Stack.pop loc.line in
      encode_line line encoder;
      let start_afr_line = encoder.get_pos in
      let size = start_bfr_line - start_afr_line in
      Write.write_varint size encoder;
      Write.key 4 Write.Bytes encoder; 
    done;
    Write.bool loc.is_folded encoder;
    Write.key 5 Write.Varint encoder; 
    let new_start = encoder.get_pos in
    let size = old_start - new_start in
    Write.write_varint size encoder;

  let encode_function (f : function_) encoder = 
    Write.write_varint f.id encoder;
    Write.key 1 Write.Varint encoder; 
    Write.write_varint f.name encoder;
    Write.key 2 Write.Varint encoder; 
    Write.write_varint f.system_name encoder;
    Write.key 3 Write.Varint encoder; 
    Write.write_varint f.filename encoder;
    Write.key 4 Write.Varint encoder; 
    Write.write_varint f.start_line encoder;
    Write.key 5 Write.Varint encoder; 

  let encode_functions t =
    List.iter (fun f encoder -> 
      nested encode_function f encoder;
      Write.key 5 Write.Bytes encoder; (* Field 5 of Profile = functions *)
    ) !t.functions t.encoder;

  let flush t =
    if t.pid <> t.getpid () then raise Pid_changed;
    let open Write in
    (* Flush newly seen strings *)
    while not (Stack.is_empty t.new_strs) do
      let b = Write.of_bytes_proto t.new_strs_buf in
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
    (* Flush new locations *)
    let i = ref 0 in
    while !i < t.new_locs_len do
      let b = Write.of_bytes_proto t.new_locs_buf in
      while !i < t.new_locs_len && b.get_pos > max_loc_size do
        encode_loc (List.nth new_locs !i) b;
        key 4 Bytes b;
        incr i;
      done;
      write_fd t.dest b;
    done;
    (* Flush new functions *)
    encode_functions t;
    (* Flush actual events *)
    write_fd t.dest t.encoder;
    (* reset string, location and main buffers *)
    t.new_strs_len <- 0;
    t.new_locs_len <- 0;
    t.locations <- ref [];
    t.functions <- ref [];
    t.encoder <- Write.of_bytes_proto t.encoder.buf;

  let encode_period_and_type t rate = 
    nested (fun (x, y) e -> 
      Write.write_varint x e;
      Write.key 1 Write.Varint e;
      Write.write_varint y e;
      Write.key 2 Write.Varint e;
    ) (9L, 10L) t.encoder;
    Pbrt.Encoder.key 11 Pbrt.Bytes t.encoder; (* Field 11 of Profile = Period Type *)
    let period = Int64.of_float (1.0 /. info.sample_rate) in
    Write.write_varint period t.encoder;
    Write.key 12 Pbrt.Varint encoder; (* Field 12 of Profile = Period *)
    
  (* Create a new proto writer *)
  let create dest ?getpid (info : Writer_helper.Info.t) = 
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
      encoder = Write.of_bytes_proto (Bytes.make buffer_size '\042'); 
    } in 
    init_strtbl writer;
    encode_nested (encode_sample_type1) () writer;
    encode_nested (encode_sample_type2) () writer;
    encode_period_and_type writer info.sample_rate;
    writer

  (* register a string and return its ID *)
  let register_string t s =
    match Hashtbl.find_opt t.strings s with
    | Some id -> Int64.of_int id
    | None ->
        let id = t.new_strs_len in
        Hashtbl.add t.strings s id;
        t.new_strs_len <- t.new_strs_len + 1;
        Stack.push s t.new_strs;
        Int64.of_int id

  (* Register a function and return its ID *)
  let register_function t name filename =
    let fn_id = t.next_function_id in
    t.next_function_id <- Int64.add fn_id 1L;
    t.functions := !t.functions @ [{
      id = fn_id;
      name = register_string t name;
      system_name = 0L; (* TODO: check *)
      filename = register_string t filename;
      start_line = 0L; (* TODO: check *)
    }];
    fn_id

  (* Extract location info from a raw backtrace and add it to the location array *)
  let update_locs (bt: raw_backtrace_entry) t = 
    let open Printexc in
    (* first check if we have already seen this entry *)
    if not (RawBacktraceEntryTable.mem t.loc_table bt) then 
      (* iterate over every "slot" in a frame
        One backtrace_entry (i.e. location) can correspond to
        multiple slots (i.e. lines) due to inlining  *)
      let is_folded = ref false in
      let lines = Stack.create () in
      let slots = backtrace_slots_of_raw_entry bt in
      Array.iter (fun slot ->
        match Slot.location slot with
        | None -> ()
        | Some { filename; line_number; start_char; _ } ->
          (* this may or may not be correct *)
          is_folded :=  is_inline slot;
          let function_name = match Slot.name slot with Some n -> n | _ -> "??" in
          (* check if we have seen this function *)
          let function_id = match Hashtbl.find_opt t.function_ids function_name with
            | Some fn_id -> fn_id
            | None -> register_function t function_name filename 
          in let new_line = { function_id; line = Int64.of_int line_number; column = Int64.of_int start_char } in
          Stack.push new_line lines;
      ) slots;
      RawBacktraceEntryTable.add t.loc_table bt ();
      (* add the location to the location list *)
      let entry_as_int = Int64.of_int (bt :> int) in
      t.locations := { id = entry_as_int; mapping_id = 1L; address = get_next_addr (); line = !lines; is_folded; } :: !(t.locations);
      entry_as_int
    else
      Int64.of_int (bt :> int)

  let encode_label src encoder = 
    Write.write_varint 5L encoder;
    Write.key 1 Pbrt.Varint encoder;
    let src = match src with
      | Major -> 6L
      | Minor -> 7L
      | External -> 8L
      | _ -> 0L
    in  
    Write.write_varint src encoder;
    Write.key 2 Write.Varint encoder;

  let put_alloc_with_raw_backtrace t now ~length ~nsamples ~source ~callstack =
    let id = t.next_alloc_id in
    t.next_alloc_id <- id + 1;
    (* writing the nested sample field *)
    let old_start = Write.get_pos t.encoder in
    nested (fun lst e ->
      Array.iter (fun x e -> 
        let entry = update_locs x t in
        Write.write_varint entry e;
      ) lst e;
    ) (Printexc.raw_backtrace_entries callstack) t.encoder;
    Write.key 1 Write.Bytes t.encoder; (* Field 1 of  Sample: Location IDs *)
    nested (fun (a, b) e ->
      Write.int_as_varint a e;
      Write.int_as_varint b e;
    ) (nsamples, length) t.encoder; 
    Write.key 2 Write.Bytes t.encoder; (* Field 2 of Sample: Values *)
    nested encode_label source t.encoder;
    Write.key 3 Write.Bytes t.encoder; (* Field 3 of Sample: Labels *)
    let new_start = Write.get_pos t.encoder in
    let size =  old_start - new_start in
    Write.int_as_varint size t.encoder;
    id 

  let put_collect t time id = Printf.printf "TODO: write collect events\n"; 
  let put_promote t time id = Printf.printf "TODO: write promote events\n"; 
  let encode_alloc_ext t id = Printf.printf "TODO: write external alloc events"; 

  let write_duration t end_time = 
    let duration = Int64.sub end_time t.start_time in
    Write.varint (Int64.mul 1000L duration) t.encoder;
    Write.key 10 Write.Varint t.encoder;

  let close t =
    let end_time = Int64.of_float (Unix.gettimeofday () *. 1_000_000_000.) in
    flush t; 
    write_duration t end_time;
    Unix.close t.dest
end
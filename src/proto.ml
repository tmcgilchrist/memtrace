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

let loc_to_string (loc : location) =
  let lines = Stack.fold (fun acc line -> acc ^ Printf.sprintf "\n%Ld: %Ld-%Ld" line.function_id line.line line.column) "" loc.line in
  Printf.sprintf "id: %Ld, mapping_id: %Ld, address: %Ld, is_folded: %b lines: %s" loc.id loc.mapping_id loc.address loc.is_folded lines

(* TODO: using dummy info for now, as we dont have mapping information.
  Add mapping info  *)
(*let start_addr = 0x7F00000000L
let end_addr = 0x7F40000000L*)
let offset = 16L
let curr_addr = ref 0x7F0000000L
let buffer_size = 65536 (* this will also change once we start streaming pprof data *)

let get_next_addr () =
  curr_addr := Int64.add !curr_addr offset;
  !curr_addr

(* TODO: make sure these numbers make sense, for now they are copied from Backtrace_codec, Location_codec and Trace
and add proper checks for buffer size and flushes  *)
let max_loc_size = 4096
let max_backtrace = 4096
(*let max_ev = 100*)
let max_packet_size = 1 lsl 15
let max_str_size = 4096

type writer = {
  dest : Unix.file_descr;
  pid : int64; 
  getpid : unit -> int64;
  sample_rate : float;
  start_time : int64;
  mutable next_alloc_id : int;

  (* I don't know if this is the most efficient way, works for now  *)
  new_strs : string Stack.t;
  mutable strs_len : int64;
  new_strs_buf : Bytes.t;
  mutable strings : (string, int64) Hashtbl.t; 

  (* TODO: improve: we may not need to maintain function and location lists, but it's not possible to write them as soon as we see them either. *)

  (* Function tracking *)
  functions : function_ Stack.t;
  mutable function_ids : (string, int64) Hashtbl.t;  (* Maps function name to ID *)
  mutable next_function_id : int64;
  
  (* Location tracking *)
  mutable locations : location list ref;
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

  let[@inline] encode_nested f v e = 
    let old_start = Write.get_pos e in
    f v e;
    let new_start = Write.get_pos e in
    let size =  old_start - new_start in
    Write.int_as_varint size e
    
  (* TODO: check - is this the right way to do it???? *)
  (* encode the "number of samples" and "count" sample type *)
  let encode_sample_type1 () e =
    Write.write_varint 1L e;
    Write.key 1 Write.Varint e; 
    Write.write_varint 2L e;
    Write.key 2 Write.Varint e

  (* encode the "alloc size" and "bytes" sample type *)
  let encode_sample_type2 () e =
    Write.write_varint 3L e;
    Write.key 1 Write.Varint e; 
    Write.write_varint 4L e;
    Write.key 2 Write.Varint e

  let encode_line (line : line) encoder = 
    Write.write_varint line.function_id encoder;
    Write.key 1 Write.Varint encoder; 
    Write.write_varint line.line encoder;
    Write.key 2 Write.Varint encoder; 
    Write.write_varint line.column encoder;
    Write.key 3 Write.Varint encoder

  let encode_loc (loc : location) encoder =
    Printf.printf "Encoding location: %s" (loc_to_string loc);
    let old_start = Write.get_pos encoder in
    Write.write_varint loc.id encoder;
    Write.key 1 Write.Varint encoder; 
    Write.write_varint loc.mapping_id encoder;
    Write.key 2 Write.Varint encoder; 
    Write.write_varint loc.address encoder;
    Write.key 3 Write.Varint encoder; 
    while not (Stack.is_empty loc.line) do
      (* every line is a nested field *)
      let start_bfr_line = Write.get_pos encoder in
      let line = Stack.pop loc.line in
      encode_line line encoder;
      let start_afr_line = Write.get_pos encoder in
      let size = start_bfr_line - start_afr_line in
      Write.int_as_varint size encoder;
      Write.key 4 Write.Bytes encoder; 
    done;
    Write.bool loc.is_folded encoder;
    Write.key 5 Write.Varint encoder; 
    let new_start = Write.get_pos encoder in
    let size = old_start - new_start in
    Write.int_as_varint size encoder

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
    Write.key 5 Write.Varint encoder

  let encode_functions t =
    while not (Stack.is_empty t.functions) do
      let f = Stack.pop t.functions in
      encode_nested encode_function f t.encoder; 
      Write.key 5 Write.Bytes t.encoder (* Field 5 of Profile = functions *)
    done

  let flush t =
    if t.pid <> t.getpid () then raise Pid_changed;
    let open Write in
    (* Flush newly seen strings *)
    while not (Stack.is_empty t.new_strs) do
      Printf.printf "initialising new string buffer\n";
      let b = Write.of_bytes_proto t.new_strs_buf in
      (* write until either we run out of buffer space or we finish writing all strings *)
      (* TODO: CHECK MAX_STRS_SIZE*)
      while ((not (Stack.is_empty t.new_strs)) &&
      Write.get_pos b > max_str_size) do
        let str = Stack.pop t.new_strs in
        write_string str b;
        key 6 Bytes b;
        Printf.printf "[flush] writing string: %s\n" str;
        Printf.printf "[flush] raw bytes written so far: \"%s\":\n" str;
        for i = get_pos b to (get_end b)-1 do
          Printf.printf "%02X " (Char.code (Bytes.get b.buf i))
        done;
        print_newline ();
      done;
      write_fd_proto t.dest b
    done;
    (* Flush new locations *)
    let i = ref 0 in
    while !i < t.new_locs_len do
      let b_loc = of_bytes_proto t.new_locs_buf in
      while ((!i < t.new_locs_len) && (get_pos b_loc > max_loc_size)) do
        encode_loc (List.nth !(t.locations) !i) b_loc;
        key 4 Bytes b_loc;
        incr i;
        Printf.printf "[flush] writing location %d: %s\n" (!i) (loc_to_string (List.nth !(t.locations) !i));
      done;
      write_fd_proto t.dest b_loc;
    done;
    (* Flush actual events *)
    write_fd_proto t.dest t.encoder;
    (* reset location and main buffers *)
    t.new_locs_len <- 0;
    t.locations <- ref [];
    t.encoder <- Write.of_bytes_proto t.encoder.buf

  let encode_period_and_type t rate = 
    encode_nested (fun (x, y) e -> 
      Write.write_varint x e;
      Write.key 1 Write.Varint e;
      Write.write_varint y e;
      Write.key 2 Write.Varint e;
    ) (9L, 10L) t.encoder;
    Write.key 11 Write.Bytes t.encoder; (* Field 11 of Profile = Period Type *)
    let period = Int64.of_float (1.0 /. rate) in
    Write.write_varint period t.encoder;
    Write.key 12 Write.Varint t.encoder (* Field 12 of Profile = Period *)
    
  (* Create a new proto writer *)
  let create dest ?getpid (info : Writer_helper.Info.t) = 
    let getpid = match getpid with
      | Some getpid -> getpid
      | None -> fun () -> info.pid in
    let pid = getpid () in

    let writer = {
      dest; pid; getpid; 
      sample_rate = info.sample_rate; 
      start_time = info.start_time; 
      next_alloc_id = 0; 
      new_strs = Stack.create ();
      strs_len = 11L; (* after adding initial strings *)
      new_strs_buf = Bytes.make max_packet_size '\042';
      strings = Hashtbl.create 100;
      functions = Stack.create ();
      function_ids =  Hashtbl.create 100; 
      next_function_id = 1L; 
      locations = ref []; 
      new_locs_len = 0;
      new_locs_buf = Bytes.make max_backtrace '\042';
      loc_table = RawBacktraceEntryTable.create 100;
      encoder = Write.of_bytes_proto (Bytes.make buffer_size '\042'); 
    } in 
    init_strtbl writer;
    encode_nested (encode_sample_type1) () writer.encoder;
    encode_nested (encode_sample_type2) () writer.encoder;
    encode_period_and_type writer info.sample_rate;
    writer

  (* register a string and return its ID *)
  let register_string t s =
    match Hashtbl.find_opt t.strings s with
    | Some id -> id
    | None ->
        let id = t.strs_len in
        Hashtbl.add t.strings s id;
        t.strs_len <- Int64.add t.strs_len 1L;
        Stack.push s t.new_strs;
        Printf.printf "[register_string] ID %Ld → %s\n" id s;
        id

  (* Register a function and return its ID *)
  let register_function t name filename =
    let fn_id = t.next_function_id in
    t.next_function_id <- Int64.add fn_id 1L;
    Stack.push {
      id = fn_id;
      name = register_string t name;
      system_name = 0L; (* TODO: check *)
      filename = register_string t filename;
      start_line = 0L; (* TODO: check *)
    } t.functions; 
    fn_id

  (* Extract location info from a raw backtrace and add it to the location array *)
  let update_locs (bt: Printexc.raw_backtrace_entry) t = 
    let open Printexc in
    (* first check if we have already seen this entry *)
    if not (RawBacktraceEntryTable.mem t.loc_table bt) then begin
      (* iterate over every "slot" in a frame
        One backtrace_entry (i.e. location) can correspond to
        multiple slots (i.e. lines) due to inlining  *)
      let is_folded = ref false in
      let lines = Stack.create () in
      let slots = match backtrace_slots_of_raw_entry bt with 
      | Some slots' -> slots'
      | None -> [||] in
      Array.iter (fun slot ->
        match Slot.location slot with
        | None -> ()
        | Some { filename; line_number; start_char; _ } ->
          (* TODO: check if this is correct *)
          is_folded :=  Slot.is_inline slot;
          let function_name = match Slot.name slot with Some n -> n | _ -> "??" in
          (* check if we have seen this function *)
          let function_id = match Hashtbl.find_opt t.function_ids function_name with
            | Some fn_id -> fn_id
            | None -> register_function t function_name filename 
          in
          let new_line = { function_id; line = Int64.of_int line_number; column = Int64.of_int start_char; } in
          Stack.push new_line lines
      ) slots;
      RawBacktraceEntryTable.add t.loc_table bt ();
      (*t.new_locs_len <- t.new_locs_len + 1;*)
      (* add the location to the location list *)
      let entry_as_int = Int64.of_int (bt :> int) in
      t.locations := { id = entry_as_int; mapping_id = 1L; address = get_next_addr (); line=lines; is_folded = !is_folded; } :: !(t.locations);
      entry_as_int
      end
    else
      Int64.of_int (bt :> int)

  let encode_label src encoder = 
    Write.write_varint 5L encoder;
    Write.key 1 Write.Varint encoder;
    let src = match src with
      | Writer_helper.Allocation_source.Major -> 6L
      |  Writer_helper.Allocation_source.Minor -> 7L
      |  Writer_helper.Allocation_source.External -> 8L
    in  
    Write.write_varint src encoder;
    Write.key 2 Write.Varint encoder

  let put_alloc_with_raw_backtrace t _ ~length ~nsamples ~source ~callstack =
    let id = t.next_alloc_id in
    t.next_alloc_id <- id + 1;
    (* writing the nested sample field *)
    let old_start = Write.get_pos t.encoder in
    encode_nested (fun lst e ->
      Array.iter (fun x -> 
        let entry = update_locs x t in
        Write.write_varint entry e;
      ) lst;
    ) (Printexc.raw_backtrace_entries callstack) t.encoder;
    Write.key 1 Write.Bytes t.encoder; (* Field 1 of  Sample: Location IDs *)
    encode_nested (fun (a, b) e ->
      Write.int_as_varint a e;
      Write.int_as_varint b e;
    ) (nsamples, length) t.encoder; 
    Write.key 2 Write.Bytes t.encoder; (* Field 2 of Sample: Values *)
    encode_nested encode_label source t.encoder;
    Write.key 3 Write.Bytes t.encoder; (* Field 3 of Sample: Labels *)
    let new_start = Write.get_pos t.encoder in
    let size =  old_start - new_start in
    Write.int_as_varint size t.encoder;
    (* now write all the functions we saw in this sample *)
    encode_functions t;
    id 

  let put_collect _ _ _ = ()
  let put_promote _ _ _ = ()
  (*let encode_alloc_ext _ _ = Printf.printf "TODO: write external alloc events"*)

  let write_duration t end_time = 
    let duration = Int64.sub end_time t.start_time in
    Write.write_varint (Int64.mul 1000L duration) t.encoder;
    Write.key 10 Write.Varint t.encoder

  let close t =
    let end_time = Int64.of_float (Unix.gettimeofday () *. 1_000_000_000.) in
    write_duration t end_time;
    flush t; 
    Unix.close t.dest
end
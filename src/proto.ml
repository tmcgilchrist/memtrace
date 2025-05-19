open Buf

(** Types of allocation *)
module Allocation_source = struct
  type t = Minor | Major | External
  end

module Info = struct
  type t = {
    sample_rate : float;
    word_size : int;
    executable_name : string;
    host_name : string;
    ocaml_runtime_params : string;
    pid : Int64.t;
    start_time : int64;
    context : string option;
    }
  end

(* Time since the epoch *)
module Timestamp = struct
  type t = int64

  let of_int64 t = t
  let to_int64 t = t

  let to_float t =
    (Int64.to_float t) /. 1_000_000.

  let of_float f =
    f *. 1_000_000. |> Int64.of_float

  let now () = of_float (Unix.gettimeofday ())
  end

(* Time since the start of the trace *)
module Timedelta = struct
  type t = int64

  let to_int64 t = t
  let offset = Int64.add
  end

module Location = struct
  type t = {
    filename : string;
    line : int;
    start_char : int;
    end_char : int;
    defname : string;
    }

  let to_string { filename; line; start_char; end_char; defname } =
    Printf.sprintf "%s@%s:%d:%d-%d" defname filename line start_char end_char

  let unknown =
    { filename = "<unknown>";
      line = 1;
      start_char = 1;
      end_char = 1;
      defname = "??"  }
  end

module IntTbl = Hashtbl.MakeSeeded (struct
  type t = int

  let hash _seed (id : t) =
    let h = id * 189696287 in
    h lxor (h lsr 23)

  (* Required for OCaml >= 5.0.0, but causes errors for older compilers
     because it is an unused value declaration. *)
  let [@warning "-32"] seeded_hash = hash

  let equal (a : t) (b : t) = a = b
  end)

module Int_pair = struct
  type t = int * int
  let hash (a, b) = a lxor (b * 65599)
  let equal (a1, b1) (a2, b2) = a1 = a2 && b1 = b2
end

module IntPairTbl = Hashtbl.Make(Int_pair)


module Obj_id = struct
  type t = int
  module Tbl = IntTbl
end

module Location_code = struct
  type t = int
  module Tbl = IntTbl
end

module Event = struct
  type t =
    | Alloc of {
        obj_id : Obj_id.t;
        length : int;
        nsamples : int;
        source : Allocation_source.t;
        backtrace_buffer : Location_code.t array;
        backtrace_length : int;
        common_prefix : int;
      }
    | Promote of Obj_id.t
    | Collect of Obj_id.t

  let to_string decode_loc = function
    | Alloc {obj_id; length; nsamples; source;
             backtrace_buffer; backtrace_length; common_prefix} ->
      let backtrace =
        List.init backtrace_length (fun i ->
            let s = backtrace_buffer.(i) in
            match decode_loc s with
            | [] -> Printf.sprintf "$%d" (s :> int)
            | ls -> String.concat " nextloc " (List.map Location.to_string ls))
        |> String.concat " nextentryinbb " in
      let alloc_src =
        match source with
        | Minor -> "alloc"
        | Major -> "alloc_major"
        | External -> "alloc_ext" in
      Printf.sprintf "%010d %s %d len=%d % 4d: %s"
        (obj_id :> int) alloc_src
        nsamples length common_prefix
        backtrace;
    | Promote id ->
      Printf.sprintf "%010d promote" (id :> int)
    | Collect id ->
      Printf.sprintf "%010d collect" (id :> int)
end

module List_util = struct
  let rev_iter_with f l st =
    let rec iter_ f l st =
      match l with
      | [] -> ()
      | x :: tl ->
        f x st;
        iter_ f tl st
    in
    let rec direct i f l st =
      match l with
      | [] -> ()
      | [ x ] -> f x st
      | [ x; y ] ->
        f y st;
        f x st
      | _ when i = 0 -> iter_ f (List.rev l) st
      | x :: y :: tl ->
        direct (i - 1) f tl st;
        f y st;
        f x st
    in

    match l with
    | [] -> ()
    | [ x ] -> f x st
    | [ x; y ] ->
      f y st;
      f x st
    | x :: y :: tl ->
      direct 200 f tl st;
      f y st;
      f x st
end

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
  filename : int64;
}

let loc_to_string (loc : location) =
  let lines = Stack.fold (fun acc line -> acc ^ Printf.sprintf "\n%Ld: %Ld-%Ld" line.function_id line.line line.column) "" loc.line in
  Printf.sprintf "id: %Ld, mapping_id: %Ld, address: %Ld, is_folded: %b lines: %s" loc.id loc.mapping_id loc.address loc.is_folded lines

let function_to_string (f : function_) =
  Printf.sprintf "id: %Ld, name: %Ld, filname: %Ld\n" f.id f.name f.filename

(* TODO: using dummy info for now, as we dont have mapping information.
  Add mapping info  *)
(*let start_addr = 0x7F00000000L
let end_addr = 0x7F40000000L*)
let offset = 16L
let curr_addr = ref 0x7F0000000L
let buffer_size = 1 lsl 15 (* this will also change once we start streaming pprof data *)

let get_next_addr () =
  curr_addr := Int64.add !curr_addr offset;
  !curr_addr

(* TODO: make sure these numbers make sense, for now they are copied from Backtrace_codec, Location_codec and Trace
and add proper checks for buffer size and flushes  *)
let max_loc_size = 4096
let max_backtrace = 4096
let max_ev = 100 + max_backtrace
let max_packet_size = 1 lsl 15
let max_str_size = 4096
let max_func_size = 33

type writer = {
  dest : Unix.file_descr;
  pid : int64;
  getpid : unit -> int64;
  sample_rate : float;
  start_time : int64;
  mutable next_alloc_id : int;

  mutable new_strs : string array;
  mutable strs_len : int64;
  mutable new_strs_len : int;
  new_strs_buf : Bytes.t;
  mutable strings : (string, int64) Hashtbl.t;

  (* Function tracking *)
  mutable functions : function_ array;
  mutable function_ids : int64 IntPairTbl.t; (* Maps (fn name, filename) to ID *)
  mutable next_function_id : int64;
  mutable new_funcs_len : int;

  (* Location tracking *)
  mutable locations : location list ref;
  mutable new_locs_len : int;
  (*new_locs_buf : Bytes.t;*)
  mutable loc_encoder : Write.t;
  mutable loc_table : unit RawBacktraceEntryTable.t; (* track which locations we have seen before *)

  mutable encoder : Write.t;
}

module Writer = struct
  type t = writer
  exception Pid_changed

  (* these strings are only accessed once *)
  let write_strtbl t name =
    let b = Write.of_bytes_proto t.new_strs_buf in
    Write.write_string name b;
    Write.key 6 Bytes b;
    Write.write_string "words" b;
    Write.key 6 Bytes b;
    Write.write_string "space" b;
    Write.key 6 Bytes b;
    Write.write_string "external" b;
    Write.key 6 Bytes b;
    Write.write_string "major" b;
    Write.key 6 Bytes b;
    Write.write_string "minor" b;
    Write.key 6 Bytes b;
    Write.write_string "source" b;
    Write.key 6 Bytes b;
    Write.write_string "bytes" b;
    Write.key 6 Bytes b;
    Write.write_string "alloc_size" b;
    Write.key 6 Bytes b;
    Write.write_string "count" b;
    Write.key 6 Bytes b;
    Write.write_string "num_samples" b;
    Write.key 6 Bytes b;
    Write.write_string "" b;
    Write.key 6 Bytes b;
    Printf.printf "[writing strings] raw bytes written so far: \n%!";
    for i = Write.get_pos b to (Write.get_end b)-1 do
      Printf.printf "%02X " (Char.code (Bytes.get b.buf i))
    done; 
    Write.write_fd_proto t.dest b;
    t.encoder <- Write.of_bytes_proto t.encoder.buf

  let[@inline] encode_nested f v e =
    let old_start = Write.get_pos e in
    f v e;
    let new_start = Write.get_pos e in
    let size =  old_start - new_start in
    Write.int_as_varint size e

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

  let flush t =
    Printf.printf "[flush] entry\n%!";
    if t.pid <> t.getpid () then raise Pid_changed;
    let open Write in
    (* Flush newly seen strings *)
    let i = ref t.new_strs_len in
    while (!i > 0) do
      let b = Write.of_bytes_proto t.new_strs_buf in
      (* write until either we run out of buffer space or we finish writing all strings *)
      (* TODO: CHECK MAX_STRS_SIZE*)
      while (!i > 0 && Write.get_pos b > max_str_size) do
        let str = t.new_strs.(!i-1) in
        write_string str b;
        key 6 Bytes b;
        decr i;
        Printf.printf "[flush] writing string: %s\n%!" str;
        Printf.printf "[flush] raw bytes written so far: \"%s\":\n%!" str;
        for i = get_pos b to (get_end b)-1 do
          Printf.printf "%02X " (Char.code (Bytes.get b.buf i))
        done; 
        print_newline ();
      done;
      Printf.printf "finished printing raw bytes, calling write\n%!";
      write_fd_proto t.dest b
    done;
    t.new_strs_len <- 0;
    (* Flush new locations *)
    (*let i = ref 0 in
    while !i < t.new_locs_len do
      (*Printf.printf "initialising new location buffer to write %d new locs\n%!" t.new_locs_len;*)
      let b_loc = of_bytes_proto t.new_locs_buf in
      while ((!i < t.new_locs_len) && (get_pos b_loc > max_loc_size)) do
        encode_loc (List.nth !(t.locations) !i) b_loc;
        key 4 Bytes b_loc;
        Printf.printf "[flush] writing location %d: %s\n%!" (!i) (loc_to_string (List.nth !(t.locations) !i));
        incr i;
      done;
      Printf.printf "[flush] after_locs raw bytes written so far: \n%!";
      for i = get_pos b_loc to (get_end b_loc)-1 do
        Printf.printf "%02X " (Char.code (Bytes.get b_loc.buf i))
      done;
      
      write_fd_proto t.dest b_loc;
    done;*)
    (* Flush actual events *)
    Printf.printf "[flush] writing actual events. bytes: \n%!";
    for i = get_pos t.encoder to (get_end t.encoder)-1 do
      Printf.printf "%02X " (Char.code (Bytes.get t.encoder.buf i))
    done;
    print_newline ();
    write_fd_proto t.dest t.encoder;
    (* reset location and main buffers *)
    (*t.new_locs_len <- 0;
    t.locations <- ref [];*)
    t.encoder <- Write.of_bytes_proto t.encoder.buf

  (*let encode_loc t id lines is_folded =
    if not (Write.get_pos t.loc_encoder > max_loc_size) then flush t;
    let old_start = Write.get_pos t.encoder in
    Write.write_varint id t.encoder;
    Write.key 1 Write.Varint t.encoder;
    Write.write_varint 1L t.encoder; (* only one mapping for now *)
    Write.key 2 Write.Varint t.encoder;
    Write.write_varint (get_next_addr ()) t.encoder;
    Write.key 3 Write.Varint t.encoder;
    while not (Stack.is_empty lines) do
      (* every line is a nested field *)
      let start_bfr_line = Write.get_pos t.encoder in
      let line = Stack.pop lines in
      encode_line line t.encoder;
      let start_afr_line = Write.get_pos t.encoder in
      let size = start_bfr_line - start_afr_line in
      Write.int_as_varint size t.encoder;
      Write.key 4 Write.Bytes t.encoder;
    done;
    Write.bool is_folded t.encoder;
    Write.key 5 Write.Varint t.encoder;
    let new_start = Write.get_pos t.encoder in
    let size = old_start - new_start in
    Write.int_as_varint size t.encoder;
    Write.key 4 Bytes t.encoder*)

  let encode_function (f : function_) encoder =
    Write.write_varint f.id encoder;
    Write.key 1 Write.Varint encoder;
    Write.write_varint f.name encoder;
    Write.key 2 Write.Varint encoder;
    Write.write_varint f.filename encoder;
    Write.key 4 Write.Varint encoder

  let encode_functions t =
    let open Write in
    let i = ref 0 in
    while !i < t.new_funcs_len do
      while ((!i < t.new_funcs_len) && (get_pos t.encoder > max_func_size)) do
        let f = t.functions.(!i) in
        encode_nested encode_function f t.encoder;
        key 5 Bytes t.encoder;  (* Field 5 of Profile = functions *)
        incr i;
        Printf.printf "[encode_functions] writing func: %s\n%!" (function_to_string f);
        Printf.printf "[encode_functions] raw bytes written so far:\n%!";
        for j = get_pos t.encoder to (get_end t.encoder) - 1 do
          Printf.printf "%02X " (Char.code (Bytes.get t.encoder.buf j))
        done;
        print_newline ();
      done;
      if get_pos t.encoder > max_func_size then begin
        write_fd_proto t.dest t.encoder;
        t.encoder <- Write.of_bytes_proto t.encoder.buf
      end;
    done;
    t.new_funcs_len <- 0
    
  let encode_mapping () e = 
    Write.write_varint 1L e;
    Write.key 1 Write.Varint e;
    Write.write_varint 11L e;
    Write.key 5 Write.Varint e


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
  let create dest ?getpid (info : Info.t) =
    let getpid = match getpid with
      | Some getpid -> getpid
      | None -> fun () -> info.pid in
    let pid = getpid () in

    let writer = {
      dest; pid; getpid;
      sample_rate = info.sample_rate;
      start_time = info.start_time;
      next_alloc_id = 0;
      new_strs = [| |];
      strs_len = 12L; (* after adding initial strings *)
      new_strs_buf = Bytes.make max_packet_size '\042';
      new_strs_len = 0;
      (*todo: see how ctf handles this *)
      strings = Hashtbl.create 64;
      functions = [| |];
      function_ids =  IntPairTbl.create 64;
      next_function_id = 1L;
      new_funcs_len = 0;
      locations = ref [];
      new_locs_len = 0;
      loc_encoder = Write.of_bytes_proto (Bytes.make max_packet_size '\042');
      loc_table = RawBacktraceEntryTable.create 100;
      encoder = Write.of_bytes_proto (Bytes.make buffer_size '\042');
    } in
    write_strtbl writer info.executable_name;
    let old_start = Write.get_pos writer.encoder in
    encode_nested (encode_mapping) () writer.encoder;
    Write.key 3 Write.Bytes writer.encoder; (* Field 3 of Profile = Mapping *)
    encode_nested (encode_sample_type1) () writer.encoder;
    Write.key 1 Write.Bytes writer.encoder; (* Field 1 of Profile = Sample Type *)
    encode_nested (encode_sample_type2) () writer.encoder;
    Write.key 1 Write.Bytes writer.encoder; (* Field 1 of Profile = Sample Type *)
    encode_period_and_type writer info.sample_rate;
    let new_start = Write.get_pos writer.encoder in
    Printf.printf "[create] raw bytes written so far: \n%!";
    for i = new_start to old_start - 1 do
      Printf.printf "%02X " (Char.code (Bytes.get writer.encoder.buf i))
    done;
    print_newline ();
    writer

  (* register a string and return its ID *)
  let register_string t s =
    Printf.printf "entering [register_string] %s\n" s;
    match Hashtbl.find_opt t.strings s with
    | Some id -> id
    | None ->
        let id = t.strs_len in
        Hashtbl.add t.strings s id;
        t.strs_len <- Int64.add t.strs_len 1L;
        let alen = Array.length t.new_strs in
        if t.new_strs_len >= alen then begin 
          let new_len = if alen = 0 then 32 else alen * 2 in
          let strs = Array.make new_len s in
          Array.blit t.new_strs 0 strs 0 alen;
          t.new_strs <- strs;
          t.new_strs_len <- alen + 1;
        end else begin
          t.new_strs.(t.new_strs_len) <- s;
          t.new_strs_len <- t.new_strs_len + 1
        end;
        Printf.printf "[register_string] ID %Ld → %s\n" id s;
        id

  (* Register a function and return its ID *)
  (*let register_function t name filename =
    let fn_id = t.next_function_id in
    t.next_function_id <- Int64.add fn_id 1L;
    Hashtbl.add t.function_ids (name ^ "_" ^ filename) fn_id;
    Stack.push {
      id = fn_id;
      name = register_string t name;
      system_name = 0L; (* TODO: check *)
      filename = register_string t filename;
      start_line = 0L; (* TODO: check *)
    } t.functions;
    fn_id*)

  let register_function t fn_idx file_idx = 
    Printf.printf "[register_function] %Ld %Ld\n" fn_idx file_idx;
    let fn_id = t.next_function_id in
    t.next_function_id <- Int64.add fn_id 1L;
    IntPairTbl.add t.function_ids (Int64.to_int fn_idx, Int64.to_int file_idx) fn_id;
    (* todo: change this to a tuple *)
    let new_func = {
      id = fn_id;
      name = fn_idx;
      filename = file_idx
    } in
    let alen = Array.length t.functions in
    if t.new_funcs_len >= alen then begin 
      let new_len = if alen = 0 then 16 else alen * 2 in
      let funcs = Array.make new_len new_func in
      Array.blit t.functions 0 funcs 0 alen;
      t.functions <- funcs;
      t.new_funcs_len <- alen + 1;
    end else begin
      t.functions.(t.new_funcs_len) <- new_func;
      t.new_funcs_len <- t.new_funcs_len + 1
    end;
    Printf.printf "[register_func] ID %Ld → %s\n" fn_id (function_to_string new_func);
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
          is_folded :=  Slot.is_inline slot;
          let function_name = match Slot.name slot with Some n -> n | _ -> "??" in
          (*let fn_name = function_name ^ "_" ^ filename in*)
          let fn_idx = register_string t function_name in
          let file_idx = register_string t filename in
          (* check if we have seen this function *)
          (*let function_id = match Hashtbl.find_opt t.function_ids fn_name with*)
          let function_id = match IntPairTbl.find_opt t.function_ids (Int64.to_int fn_idx, Int64.to_int file_idx) with
            | Some fn_id -> fn_id
            | None -> (*register_function t function_name filename*) register_function t fn_idx file_idx
          in
          let new_line = { function_id; line = Int64.of_int line_number; column = Int64.of_int start_char; } in
          Stack.push new_line lines
      ) slots;
      RawBacktraceEntryTable.add t.loc_table bt ();
      t.new_locs_len <- t.new_locs_len + 1;
      (* add the location to the location list *)
      let entry_as_int = Int64.of_int (bt :> int) in
      t.locations :=  { id = entry_as_int; mapping_id = 1L; address = get_next_addr (); line=lines; is_folded = !is_folded; } :: !(t.locations);
      (*let old_start = Write.get_pos t.encoder in
      Printf.printf "[flush] writing location: %s\n%!" (loc_to_string newloc);
      encode_loc t entry_as_int lines !is_folded;
      let new_start = Write.get_pos t.encoder in
      Printf.printf "[encode_loc] raw bytes written so far:\n%!";
        for i = new_start to (old_start) do
          Printf.printf "%02X " (Char.code (Bytes.get t.encoder.buf i))
      done;*)

      Printf.printf "[update_locs] added new loc to list. new list len = %d\n" (List.length !(t.locations));
      (* check if we need to flush the buffer *)
      entry_as_int
    end
    else
      Int64.of_int (bt :> int)

  let encode_label src encoder =
    Write.write_varint 5L encoder;
    Write.key 1 Write.Varint encoder;
    let src = match src with
      | Allocation_source.Minor -> 6L
      | Allocation_source.Major -> 7L
      | Allocation_source.External -> 8L
    in
    Write.write_varint src encoder;
    Write.key 2 Write.Varint encoder

  let put_alloc_with_raw_backtrace t _ ~length ~nsamples ~source ~callstack =
    let src = match source with
      | Allocation_source.Major -> "major"
      | Allocation_source.Minor -> "minor"
      | Allocation_source.External -> "ext"
    in
    Printf.printf "[put_alloc] entry: SOURCE: %s\n%!" src;
    if Write.get_pos t.encoder < max_ev then flush t;
    let id = t.next_alloc_id in
    t.next_alloc_id <- id + 1;
    (* writing the nested sample field *)
    let old_start = Write.get_pos t.encoder in
    (*Printf.printf "[put_alloc] OLD START = %d\n%!" old_start;*)
    encode_nested (fun lst e ->
      for i = (Array.length lst)-1 downto 0 do
        let entry = lst.(i) in
        let entry = update_locs entry t in
        Write.write_varint entry e
      done
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
    (*Printf.printf "[put_alloc] size of sample: %d = %d - %d\n" size old_start new_start;*)
    Write.int_as_varint size t.encoder;
    Write.key 2 Write.Bytes t.encoder; (* Field 2 of Profile = Sample *)
    (*Printf.printf "[put_alloc] writing bytes: ";
    for i = Write.get_pos t.encoder to (old_start-1) do
      Printf.printf "%02X " (Char.code (Bytes.get t.encoder.buf i))
    done;
    print_newline ();*)
    (* now write all the functions we saw in this sample *)
    encode_functions t;
    (*if not (Stack.is_empty t.functions) then begin
      (*(Printf.printf "[put_alloc] writing functions: ";*)
      (*let bfr_f = Write.get_pos t.encoder in*)
      encode_functions t
      (*let aftr_f = Write.get_pos t.encoder in
      for i = aftr_f to bfr_f do
        Printf.printf "%02X " (Char.code (Bytes.get t.encoder.buf i))
      done; print_newline ()*)
    end;*)
    let i = ref 0 in
    while !i < t.new_locs_len do
      Printf.printf "initialising new location buffer to write %d new locs\n%!" t.new_locs_len;
      while ((!i < t.new_locs_len) && (Write.get_pos t.loc_encoder > max_loc_size)) do
        encode_loc (List.nth !(t.locations) !i) t.loc_encoder;
        Write.key 4 Write.Bytes t.loc_encoder;
        Printf.printf "[flush] writing location %d: %s\n%!" (!i) (loc_to_string (List.nth !(t.locations) !i));
        incr i;
      done;
      Printf.printf "[flush] after_locs raw bytes written so far: \n%!";
      for i = Write.get_pos t.loc_encoder to (Write.get_end t.loc_encoder)-1 do
        Printf.printf "%02X " (Char.code (Bytes.get t.loc_encoder.buf i))
      done;
      if (Write.get_pos t.loc_encoder > max_loc_size) then begin
        Write.write_fd_proto t.dest t.loc_encoder;
        t.loc_encoder <- Write.of_bytes_proto t.loc_encoder.buf
      end;
    done;
    t.new_locs_len <- 0;
    t.locations := [];
    t.loc_encoder <- Write.of_bytes_proto t.loc_encoder.buf;
    id

  let put_alloc _t _ ~length:_ ~nsamples:_ ~source:_ ~callstack:_ ~decode_callstack_entry:_ = 0
  let put_event _t ~decode_callstack_entry:_ _ _ = ()
  let put_collect _ _ _ = ()
  let put_promote _ _ _ = ()

  let write_duration t end_time =
    let duration = Int64.sub end_time t.start_time in
    Write.write_varint (Int64.mul 1000L duration) t.encoder;
    Write.key 10 Write.Varint t.encoder

  let close t =
    let end_time = Int64.of_float (Unix.gettimeofday () *. 1_000_000_000.) in
    write_duration t end_time;
    flush t;
    (*Printf.printf "functions seen: \n%!";
    Hashtbl.iter (fun (key1,key2) value ->
    Printf.printf "(%Ld, %Ld) → %Ld\n" key1 key2 value
    ) t.function_ids;*)
    Unix.close t.dest
  end


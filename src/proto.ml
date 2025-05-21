(* This implements the encoder for memtrace format based on `profile.proto` protobuf format.
   The format is loosely based on the Go implementation.

   TODO Describe the specific Sample types used and how they map to OCaml GC events.
*)
open Buf

module Timestamp = Trace_s.Timestamp
module Timedelta = Trace_s.Timedelta
module Location = Location
module Location_code = Trace_s.Location_code

module Info = Trace_s.Info
module Allocation_source = Trace_s.Allocation_source
module Obj_id = Trace_s.Obj_id
module Event = Trace_s.Event

module Int_pair = struct
  type t = int * int
  let hash (a, b) = a lxor (b * 65599)
  let equal (a1, b1) (a2, b2) = a1 = a2 && b1 = b2
end

module IntPairTbl = Hashtbl.Make(Int_pair)

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
  line : line Stack.t;
  is_folded : bool;
}

type function_ = {
  id : int64;
  name : int64;
  filename : int64;
}

let buffer_size = 1 lsl 15

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
  mutable new_locs : location array;
  mutable new_locs_len : int;
  new_locs_buf : Bytes.t;
  mutable loc_table : unit RawBacktraceEntryTable.t; (* track which locations we have seen before *)

  mutable encoder : Write.t;
}

module Writer : Trace_s.Writer = struct
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
    if t.pid <> t.getpid () then raise Pid_changed;
    let open Write in
    (* Flush newly seen strings *)
    let i = ref t.new_strs_len in
    while (!i > 0) do
      let b = Write.of_bytes_proto t.new_strs_buf in
      (* write until either we run out of buffer space or we finish writing all strings *)
      while (!i > 0 && Write.get_pos b > max_str_size) do
        let str = t.new_strs.(!i-1) in
        write_string str b;
        key 6 Bytes b;
        decr i;
      done;
      write_fd_proto t.dest b
    done;
    t.new_strs_len <- 0;
    (* Flush new locations *)
    let i = ref 0 in
    while !i < t.new_locs_len do
      let b_loc = of_bytes_proto t.new_locs_buf in
      while ((!i < t.new_locs_len) && (get_pos b_loc > max_loc_size)) do
        encode_loc (t.new_locs.(!i)) b_loc;
        key 4 Bytes b_loc;
        incr i;
      done;
      write_fd_proto t.dest b_loc;
    done;
    (* Flush actual events *)
    write_fd_proto t.dest t.encoder;
    (* reset location and main buffers *)
    t.new_locs_len <- 0;
    t.encoder <- Write.of_bytes_proto t.encoder.buf

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
      strings = Hashtbl.create 64;
      functions = [| |];
      function_ids =  IntPairTbl.create 64;
      next_function_id = 1L;
      new_funcs_len = 0;
      new_locs = [| |];
      new_locs_len = 0;
      new_locs_buf = Bytes.make max_packet_size '\042';
      loc_table = RawBacktraceEntryTable.create 100;
      encoder = Write.of_bytes_proto (Bytes.make buffer_size '\042');
    } in
    write_strtbl writer info.executable_name;
    encode_nested (encode_mapping) () writer.encoder;
    Write.key 3 Write.Bytes writer.encoder; (* Field 3 of Profile = Mapping *)
    encode_nested (encode_sample_type1) () writer.encoder;
    Write.key 1 Write.Bytes writer.encoder; (* Field 1 of Profile = Sample Type *)
    encode_nested (encode_sample_type2) () writer.encoder;
    Write.key 1 Write.Bytes writer.encoder; (* Field 1 of Profile = Sample Type *)
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
        id

  let add_location t newloc =
    let alen = Array.length t.new_locs in
    if t.new_locs_len >= alen then begin
      let new_len = if alen = 0 then 32 else alen * 2 in
      let locs = Array.make new_len newloc in
      Array.blit t.new_locs 0 locs 0 alen;
      t.new_locs <- locs;
      t.new_locs_len <- alen + 1;
    end else begin
      t.new_locs.(t.new_locs_len) <- newloc;
      t.new_locs_len <- t.new_locs_len + 1
    end

  let register_function t fn_idx file_idx =
    let fn_id = t.next_function_id in
    t.next_function_id <- Int64.add fn_id 1L;
    IntPairTbl.add t.function_ids (Int64.to_int fn_idx, Int64.to_int file_idx) fn_id;
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
          let fn_idx = register_string t function_name in
          let file_idx = register_string t filename in
          (* check if we have seen this function *)
          let function_id = match IntPairTbl.find_opt t.function_ids (Int64.to_int fn_idx, Int64.to_int file_idx) with
            | Some fn_id -> fn_id
            | None -> register_function t fn_idx file_idx
          in
          let new_line = { function_id; line = Int64.of_int line_number; column = Int64.of_int start_char; } in
          Stack.push new_line lines
      ) slots;
      RawBacktraceEntryTable.add t.loc_table bt ();
      let entry_as_int = Int64.of_int (bt :> int) in
      (* add the location to the location array *)
      let newloc = { id = entry_as_int; mapping_id = 1L; line=lines; is_folded = !is_folded; } in
      add_location t newloc;
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
    if Write.get_pos t.encoder < max_ev then flush t;
    let id = t.next_alloc_id in
    t.next_alloc_id <- id + 1;
    (* writing the nested sample field *)
    let old_start = Write.get_pos t.encoder in
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
    Write.int_as_varint size t.encoder;
    Write.key 2 Write.Bytes t.encoder; (* Field 2 of Profile = Sample *)
    (* now write all the functions we saw in this sample *)
    encode_functions t;
    id

  let put_alloc _t _now ~length:_ ~nsamples:_ ~source:_
    ~callstack:_ ~decode_callstack_entry:_ = -1

  let put_event _ ~decode_callstack_entry:_ _ _ = ()
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
    Unix.close t.dest
  end

(* module Reader = struct *)

(* end *)
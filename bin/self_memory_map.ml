(* Light-weight version of man  vmmap(1) from macOS.

   Prints out simplified region memory map using Mach 3.0 system calls.
 *)

open PosixTypes

let format_display_size (size : int64) =
  let scale = [|'B'; 'K'; 'M'; 'G'; 'T'; 'P'; 'E'|] in
  let display_size = ref (Int64.to_int size |> Float.of_int) in
  let scale_index = ref 0 in
  while (!display_size >= 999.5 ) do
    display_size := !display_size /. 1024.0;
    scale_index := !scale_index + 1
  done;
  let precision = if (!display_size < 9.95 && (!display_size -. !display_size) > 0.0) then 1 else 0 in
  Printf.sprintf "%.*f%c" precision !display_size (Array.get scale (!scale_index))

let () =
  let open Memtrace in
  let depth = 2048 in
  let pid = Unix.getpid () |> Pid.of_int in

  Printf.printf "Virtal Memory Map (depth=%u) for PID %d\n" depth (Pid.to_int pid);
  Printf.printf "          START - END             [ VSIZE ] PRT FILE\n";
  let maps = Memory_map.get_process_memory_maps  pid in
  List.iter (fun (m : Memory_map.memory_map) ->
    Printf.printf "%09Lx-%09Lx [ %s ] %c%c%c%c %6s\n"
         m.address_start
         m.address_end
         (Int64.sub m.address_end m.address_start |> format_display_size)
         (if m.perm_read then 'r' else '-')
         (if m.perm_write then 'w' else '-')
         (if m.perm_execute then 'x' else '-')
         (if m.perm_shared then 's' else '-')
         m.pathname) (List.rev maps)


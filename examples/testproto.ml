(* Enable memory tracing *)
let () = Memtrace.trace_if_requested ()

let rec allocate_list1 n acc =
  if n = 0 then acc
  else allocate_list1 (n - 1) ((n, string_of_int n) :: acc)

let rec allocate_list2 n acc =
  if n = 0 then acc
  else allocate_list2 (n - 1) ((n, string_of_int n) :: acc)

(*let measure_time f =
  let t1 = Sys.time () in
  let result = f () in
  let t2 = Sys.time () in
  Printf.printf "Execution time: %fs\n" (t2 -. t1);
  result*)

let print_memory_stats () =
  let stat = Gc.stat () in
  Printf.printf "Heap size: %d words\n" stat.Gc.heap_words;
  Printf.printf "Live blocks: %d\n" stat.Gc.live_blocks;
  Printf.printf "Free blocks: %d\n" stat.Gc.free_blocks;
  Printf.printf "Major collections: %d\n" stat.Gc.major_collections;
  Printf.printf "Minor collections: %d\n" stat.Gc.minor_collections

let () =
  let _ = allocate_list1 500_000 [] in
  Gc.full_major ();
  (* let _ = measure_time (fun () -> allocate_list 500_000 []) in
  print_memory_stats (); *)

  let _ =  allocate_list2 300_000 [] in 
  (* Another allocation to see the effect *)
  (* let _ = measure_time (fun () -> allocate_list 300_000 []) in *) 
  Gc.full_major ();

  print_memory_stats (); 
  print_endline "Done allocating and collecting!"

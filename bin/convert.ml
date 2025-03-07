let convert input_file output_file =
  Memtrace.Ctf_to_proto.convert_file input_file output_file

let () =
  if Array.length Sys.argv <> 3 then
    Printf.fprintf stderr "Usage: %s <trace file> <protobuf file>\n" Sys.executable_name
    else
      convert Sys.argv.(1) Sys.argv.(2)


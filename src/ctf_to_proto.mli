(** [convert_file input output] reads a CTF trace from the [input] file,
    converts it to protobuf format, and writes it to the [output] file. *)
val convert_file : string -> string -> unit


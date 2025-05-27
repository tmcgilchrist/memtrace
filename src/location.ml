(** Source locations *)
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

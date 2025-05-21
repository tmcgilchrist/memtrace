(** Source locations in the traced program *)
type t = {
  filename : string;
  line : int;
  start_char : int;
  end_char : int;
  defname : string;
  }

val to_string : t -> string
val unknown : t

(** Source locations in the traced program *)
type t = {
  filename : string;
  line : int;
  start_char : int;
  end_char : int;
  defname : string;
  }

(** [to_string t] creates a string representation of [t]
     with the format "function@filename:line:start-end".  *)
val to_string : t -> string

(** Create an unknown source location *)
val unknown : t

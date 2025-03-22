type line 
type location 
type function_ 

type writer 

module Writer : sig
  include Writer_helper.Writer_interface 

  val register_string : t -> string -> int64 
  val register_function : t -> string -> string -> int64 
  val update_locs : Printexc.raw_backtrace_entry -> t -> int64
end 
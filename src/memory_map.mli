open PosixTypes

type memory_map = {
  address_start: int64;
  address_end: int64;
  perm_read: bool;
  perm_write: bool;
  perm_execute: bool;
  perm_shared: bool;
  offset: int64;
  device_major: int;
  device_minor: int;
  inode: int64;
  pathname: string;
  }

val get_process_memory_maps : Pid.t -> memory_map list
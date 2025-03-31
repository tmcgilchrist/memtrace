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

let mk_entry
  address_start address_end pr pw px ps offset
  device_major device_minor inode pathname =
  { address_start; address_end; offset;
    device_major; device_minor; inode;
    pathname = String.trim pathname;
    perm_read    = pr = 'r';
    perm_write   = pw = 'w';
    perm_execute = px = 'x';
    perm_shared  = ps = 's';
    }

let get_process_memory_maps (pid : Pid.t) =
  []
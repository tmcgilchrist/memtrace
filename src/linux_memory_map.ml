open PosixTypes
(** Read process [pid] memory maps.

Opens /proc/<pid>/maps and reads each row in the file.
Each row has the following fields:

address           perms offset  dev   inode   pathname
08048000-08056000 r-xp 00000000 03:0c 64593   /usr/sbin/GM

where
  - address: the starting and ending address of the region in the process's address space
  - permissions: This describes how pages in the region can be accessed. There are four different permissions: read, write, execute, and shared. If read/write/execute are disabled, a - will appear instead of the r/w/x. If a region is not shared, it is private, so a p will appear instead of an s. If the process attempts to access memory in a way that is not permitted, a segmentation fault is generated. Permissions can be changed using the mprotect system call.
  - offset: If the region was mapped from a file (using mmap), this is the offset in the file where the mapping begins. If the memory was not mapped from a file, it's just 0.
  - device: the major and minor device number (in hex) if this region was mapped from a file.
  - inode: the file number, if this region was mapped from a file.
  - pathname the name of the file, if this region was mapped from a file. There are also special regions with names like [heap], [stack], or [vdso] virtual dynamic shared object.

See https://stackoverflow.com/questions/1401359/understanding-linux-proc-pid-maps-or-proc-self-maps
 *)

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

let scan_line ic =
  Scanf.bscanf ic "%Lx-%Lx %c%c%c%c %Lx %x:%x %Ld%s@\n" mk_entry

let rec scan_lines ic =
  match scan_line ic with
  | entry -> entry :: scan_lines ic
  | exception End_of_file -> []

let get_process_memory_maps (pid : Pid.t) =
  let file = Printf.sprintf "/proc/%d/maps" (Pid.to_int pid) in
  let ic = Scanf.Scanning.from_file file in
  let lines = scan_lines ic in
  Scanf.Scanning.close_in ic;
  lines
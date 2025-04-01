open Ctypes
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

let get_memory_protection (prot : int32) =
  let open Mach in
  let r = if (not (Int32.equal (Int32.logand prot vm_prot_read) 0l)) then 'r' else '-' in
  let w = if (not (Int32.equal (Int32.logand prot vm_prot_write) 0l)) then 'w' else '-' in
  let x = if (not (Int32.equal (Int32.logand prot vm_prot_execute) 0l)) then 'x' else '-' in
  (r, w, x)

let vm_region_submap_info_count_64 =
  let open Mach in
  sizeof (vm_region_submap_info_data_64_t) / sizeof (natural_t)

let vmmap task start_ end_ (depth : int) =
  let open Mach in
  let pid = allocate pid_t (PosixTypes.Pid.of_int 0) in
  let _ = Mach.pid_for_task (!@task) pid in
  let result = ref [] in
  let start_ = ref start_ in
  let break = ref false in
  while (!break == false) do
    let address = allocate mach_vm_address_t !start_ in
    let size = allocate mach_vm_size_t Unsigned.UInt64.zero in
    let depth0 = allocate vm_region_recurse_info_t (Int32.of_int depth) in
    let info = allocate vm_region_submap_info_data_64_t (make vm_region_submap_info_data_64_t) in
    let count = allocate mach_msg_type_number_t (Int32.of_int vm_region_submap_info_count_64) in
    let kr = Mach.mach_vm_region_recurse (!@task) address size depth0
      (to_voidp info |> from_voidp vm_region_recurse_info_t) count in

    if (not (Int32.equal kr kern_success) || (!@address) > end_) then begin
        (* No break statement in OCaml, we can make one with boolean ref  *)
        break := true
      end
    else begin
      let address_start = !@address in
      let address_end = Unsigned.UInt64.add address_start !@size in
      (* macOS has current permissions and max permissions as
         info.protection and info.max_protection.
         Here we simplify that to current permissions.
       *)
      let (pr, pw, px) = get_memory_protection (getf (!@info) protection) in
      let ps = '-' in
      let pathname = CArray.make char (4 + 4096) in
      let _kr = Mach.proc_regionfilename !@pid !@address (to_voidp (CArray.start pathname)) (Unsigned.UInt32.of_int (4 + 4096)) in
      let pathname_str = CArray.to_list pathname |> List.to_seq |> String.of_seq in
      let offset = Unsigned.UInt64.zero in
      let device_major = 0 in
      let device_minor = 0 in
      let inode = Unsigned.UInt64.zero in
      let entry = mk_entry (Unsigned.UInt64.to_int64 address_start) (Unsigned.UInt64.to_int64 address_end) pr pw px ps (Unsigned.UInt64.to_int64 offset)
        device_major device_minor (Unsigned.UInt64.to_int64 inode) (pathname_str) in
      result := entry :: (!result);
      start_ := Unsigned.UInt64.add !@address !@size
    end
  done;
  !result

let get_process_memory_maps (pid : Pid.t) =
  let open Mach in
  let depth = 2048 in
  let self = mach_task_self () in
  let task = allocate uint64_t Unsigned.UInt64.zero in
  let kr = task_for_pid self pid task in
  if (not (Int32.equal kr kern_success)) then (
     Printf.printf "task_for_pid(%d) failed: %s\n" (Pid.to_int pid) (Mach.mach_error_string kr);
     exit 1);
  vmmap task (Unsigned.UInt64.zero) (Unsigned.UInt64.max_int) depth



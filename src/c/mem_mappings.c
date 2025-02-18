#include <mach/mach.h>
#include <mach/mach_vm.h>
#include <mach-o/dyld.h>
#include <stdio.h>

void print_memory_mappings() {
    mach_port_t task = mach_task_self();
    mach_vm_address_t address = 1;
    mach_vm_size_t size = 0;
    natural_t depth = VM_REGION_BASIC_INFO_COUNT_64;
    vm_region_basic_info_data_64_t info;
    mach_msg_type_number_t info_count = depth;
    memory_object_name_t object;

    while (mach_vm_region(task, &address, &size, VM_REGION_BASIC_INFO_64, 
                          (vm_region_info_t)&info, &info_count, &object) == KERN_SUCCESS) {
        printf("Region: %p - %p | Protection: %d\n",
               (void*)address, (void*)(address + size), info.protection);
        address += size;
    }
}

void list_shared_libraries() {
    uint32_t count = _dyld_image_count();
    for (uint32_t i = 0; i < count; i++) {
        printf("Shared Library: %s\n", _dyld_get_image_name(i));
    }
}

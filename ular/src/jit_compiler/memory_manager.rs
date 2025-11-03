use std::ffi::c_void;

use crate::{
    libunwind::{add_dynamic_eh_frame_section, remove_dynamic_eh_frame_section},
    mmtk::{
        runtime::mmtk_set_stack_map,
        stack_map::{IndexableStackMap, Parseable, StackMap},
    },
};
use inkwell::memory_manager::McjitMemoryManager;
use libc::{c_uint, uintptr_t};
use log::warn;

#[derive(Debug)]
struct StackMapSection {
    pointer: *mut u8,
    size: usize,
}

#[derive(Debug)]
pub struct UlarMemoryManager {
    capacity: usize,
    code_buffer: *mut u8,
    code_buffer_offset: usize,
    data_buffer: *mut u8,
    data_buffer_offset: usize,
    eh_frame_section: Option<*mut u8>,
    print_stack_map: bool,
    stack_map_section: Option<StackMapSection>,
}

impl UlarMemoryManager {
    /// # Safety
    ///
    /// After calling [UlarMemoryManager::destroy], none of the pointers returned by
    /// [UlarMemoryManager::allocate_code_section] or [UlarMemoryManager::allocate_data_section] must
    /// be read from or written to.
    pub unsafe fn new(print_stack_map: bool) -> Self {
        let capacity = 1024 * 128;

        // SAFETY: All of the requirements of calling `mmap` have been satisfied
        let code_buffer = unsafe {
            libc::mmap(
                std::ptr::null_mut(),
                capacity,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
                -1,
                0,
            )
        } as *mut u8;

        // SAFETY: All of the requirements of calling `mmap` have been satisfied
        let data_buffer = unsafe {
            libc::mmap(
                std::ptr::null_mut(),
                capacity,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
                -1,
                0,
            )
        } as *mut u8;

        Self {
            capacity,
            code_buffer,
            code_buffer_offset: 0,
            data_buffer,
            data_buffer_offset: 0,
            eh_frame_section: None,
            print_stack_map,
            stack_map_section: None,
        }
    }

    fn parse_stack_map(&mut self) {
        let section = if let Some(section) = &self.stack_map_section {
            section
        } else {
            panic!("LLVM never allocated a data section for the stack map.");
        };

        let section_slice =
            // Safety: That `section.pointer` is valid for reads for `section.size` bytes was verified
            // when it was allocated in `allocate_data_section`
            unsafe { std::slice::from_raw_parts_mut(section.pointer, section.size) };

        let (stack_map, _) = StackMap::parse(section_slice);
        let indexable_stack_map = IndexableStackMap::from_stack_map(&stack_map);

        if self.print_stack_map {
            warn!("Stack map:\n{:#?}", indexable_stack_map);
        }

        mmtk_set_stack_map(indexable_stack_map).unwrap();
    }
}

impl McjitMemoryManager for UlarMemoryManager {
    fn allocate_code_section(
        &mut self,
        size: uintptr_t,
        align: c_uint,
        _section_id: c_uint,
        _section_name: &str,
    ) -> *mut u8 {
        if self.code_buffer_offset + size > self.capacity {
            panic!("The code buffer's capacity has been exceeded.")
        }

        let cast_align = align as usize;
        let padding = (cast_align
            - ((self.code_buffer as usize + self.code_buffer_offset) % cast_align))
            % cast_align;

        if self.code_buffer_offset + padding + size > self.capacity {
            panic!("The code buffer's capacity has been exceeded.");
        }

        self.code_buffer_offset += padding;

        // SAFETY: `self.code_buffer` is valid for reads for `self.capacity`, and we verified that
        // `self.code_buffer_offset` is less than or equal to `self.capacity`
        let pointer = unsafe { self.code_buffer.add(self.code_buffer_offset) };

        self.code_buffer_offset += size;

        pointer
    }

    fn allocate_data_section(
        &mut self,
        size: uintptr_t,
        align: c_uint,
        _section_id: c_uint,
        section_name: &str,
        _is_read_only: bool,
    ) -> *mut u8 {
        if self.data_buffer_offset + size > self.capacity {
            panic!("The data buffer's capacity has been exceeded.")
        }

        let cast_align = align as usize;
        let padding = (cast_align
            - ((self.data_buffer as usize + self.data_buffer_offset) % cast_align))
            % cast_align;

        if self.data_buffer_offset + padding + size > self.capacity {
            panic!("The data buffer's capacity has been exceeded.");
        }

        self.data_buffer_offset += padding;

        // SAFETY: `self.data_buffer` is valid for reads for `self.capacity`, and we verified that
        // `self.data_buffer_offset` is less than or equal to `self.capacity`
        let pointer = unsafe { self.data_buffer.add(self.data_buffer_offset) };

        self.data_buffer_offset += size;

        if section_name == ".eh_frame" {
            self.eh_frame_section = Some(pointer);
        }

        // The names of the stack map section as described in
        // https://llvm.org/docs/StackMaps.html#stack-map-section
        if section_name == "__llvm_stackmaps" || section_name == ".llvm_stackmaps" {
            self.stack_map_section = Some(StackMapSection { pointer, size });
        }

        pointer
    }

    fn destroy(&mut self) {
        if let Some(eh_frame_section) = self.eh_frame_section {
            unsafe { remove_dynamic_eh_frame_section(eh_frame_section as usize) };
        }

        // SAFETY: As per the contract of `UlarMemoryManager::new`, neither `self.code_buffer` nor
        // `self.data_buffer` are in use
        unsafe {
            libc::munmap(self.code_buffer as *mut c_void, self.capacity);
            libc::munmap(self.data_buffer as *mut c_void, self.capacity);
        }
    }

    fn finalize_memory(&mut self) -> Result<(), String> {
        if let Some(eh_frame_section) = self.eh_frame_section {
            // Register the `.eh_frame` section, which contains information about call frames, with
            // libunwind so we can do unwinding. This is used for root scanning during
            // garbage collection, among other language features.
            //
            // For more information on this section, see:
            // https://refspecs.linuxfoundation.org/LSB_3.0.0/LSB-Core-generic/LSB-Core-generic/ehframechpt.html
            unsafe { add_dynamic_eh_frame_section(eh_frame_section as usize) };
        }

        self.parse_stack_map();

        // SAFETY: All of the requirements of calling `mprotect` have been satisfied
        unsafe {
            libc::mprotect(
                self.code_buffer as *mut c_void,
                self.capacity,
                libc::PROT_READ | libc::PROT_EXEC,
            );

            libc::mprotect(
                self.data_buffer as *mut c_void,
                self.capacity,
                libc::PROT_READ,
            );
        };

        Ok(())
    }
}

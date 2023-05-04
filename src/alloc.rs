use std::alloc::{Layout, alloc};

const BLOCK_SIZE: usize = 2048; // bytes
const ALIGNMENT: usize = std::mem::size_of::<usize>(); // ptr alignment
                                                       //
/// Leaks used blocks so all returned data can have static references
pub struct StaticBumpAllocator {
    block: *mut u8,
    bytes_left: usize,
}

impl StaticBumpAllocator {
    pub fn new() -> Self {
        const LAYOUT: Layout = match Layout::from_size_align(BLOCK_SIZE, ALIGNMENT) {
            Ok(ptr) => ptr,
            _ => panic!("Layout creation failure")
        };

        let block = unsafe { alloc(LAYOUT) };

        StaticBumpAllocator {
            block,
            bytes_left: BLOCK_SIZE,
        }
    }

    fn new_block(&mut self) {
        *self = StaticBumpAllocator::new();
    }

    pub fn alloc_str(&mut self, s: &str) -> &'static str {
        let source = s.as_bytes().as_ptr();
        let slen = s.len();
        assert!(slen < BLOCK_SIZE);

        if slen > self.bytes_left { self.new_block() }

        let static_s = unsafe {
            std::ptr::copy(source, self.block, slen);
            let static_bytes = std::slice::from_raw_parts(self.block, slen);
            std::str::from_utf8_unchecked(static_bytes)
        };

        // hacks to align to ptr
        self.bytes_left -= slen;
        let remainder = self.bytes_left & ALIGNMENT-1;
        self.bytes_left ^= remainder;
        self.block = unsafe { self.block.add(slen + remainder) };

        static_s
    }
}

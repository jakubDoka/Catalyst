use std::{
    alloc::Layout,
    cell::Cell,
    mem,
    ops::Range,
    ptr::{self, NonNull},
    slice,
};

pub type Allocator = AllocatorLow<false>;

pub struct ProtectedAllocator {
    protection: region::Protection,
    protected: usize,
    is_current_protected: bool,
    inner: Allocator,
}

impl ProtectedAllocator {
    pub fn new(protection: region::Protection) -> Self {
        Self {
            protection,
            protected: 0,
            is_current_protected: false,
            inner: Allocator::new(),
        }
    }

    pub fn with_chunk_size(chunk_size: usize, protection: region::Protection) -> Self {
        Self {
            protection,
            protected: 0,
            is_current_protected: false,
            inner: Allocator::with_chunk_size(chunk_size),
        }
    }

    pub fn seal(&mut self) {
        for chunk in self.inner.chunks.get_mut().iter_mut().skip(self.protected) {
            chunk.set_memory_protection(self.protection);
        }

        self.is_current_protected = true;
    }

    pub fn alloc(&mut self, layout: Layout) -> NonNull<[u8]> {
        if mem::take(&mut self.is_current_protected) {
            self.reset_current();
        }
        self.inner.alloc(layout)
    }

    #[cold]
    #[inline(never)]
    pub fn reset_current(&mut self) {
        // SAFETY: There is always at least one chunk.
        unsafe {
            self.inner
                .chunks
                .get_mut()
                .last_mut()
                .unwrap_unchecked()
                .set_memory_protection(region::Protection::READ_WRITE)
        };

        self.protected -= 1;
    }

    /// Frees all allocations. Protection is also released.
    /// # Safety
    /// Its only safe to call if the memory is no longer used.
    pub unsafe fn clear(&mut self) {
        for chunk in self
            .inner
            .chunks
            .get_mut()
            .iter_mut()
            .chain(self.inner.free.get_mut())
        {
            chunk.set_memory_protection(region::Protection::READ_WRITE);
        }
        self.protected = 0;
        self.is_current_protected = false;
        self.inner.clear();
    }
}

impl Drop for ProtectedAllocator {
    fn drop(&mut self) {
        unsafe {
            self.clear();
        }
    }
}

pub struct AllocatorLow<const WRITE_PADDING: bool> {
    free: Cell<Vec<Chunk>>,
    chunks: Cell<Vec<Chunk>>,
    current: Cell<*mut u8>,
    start: Cell<*mut u8>,
    chunk_size: usize,
}

impl<const WRITE_PADDING: bool> Default for AllocatorLow<WRITE_PADDING> {
    fn default() -> Self {
        Self::new()
    }
}

impl<const WRITE_PADDING: bool> AllocatorLow<WRITE_PADDING> {
    /// Since allocator is always reused small startup penalty is acceptable.
    const DEFAULT_CHUNK_SIZE: usize = 1024 * 1024 * 2;

    pub fn new() -> Self {
        Self::with_chunk_size(Self::DEFAULT_CHUNK_SIZE)
    }

    pub fn with_chunk_size(chunk_size: usize) -> Self {
        let mut chunk = Chunk::new(chunk_size);
        let range = chunk.range();
        Self {
            free: Cell::new(Vec::new()),
            chunks: Cell::new(vec![chunk]),
            current: Cell::new(range.end as *mut _),
            start: Cell::new(range.start as *mut _),
            chunk_size,
        }
    }

    pub fn alloc(&self, layout: Layout) -> NonNull<[u8]> {
        let current = self.current.get();
        let start = self.start.get();
        let padding = Self::compute_padding(current, layout);
        let size = layout.size() + padding;

        if current as usize - (start as usize) < size {
            self.alloc_new(layout)
        } else {
            // SAFETY: We just checked that the current chunk has enough space
            if WRITE_PADDING {
                Self::write_padding(current, padding);
            }
            let new = unsafe { current.sub(size) };
            self.current.set(new);
            // SAFETY: Allocation with address range range 0..n should never happen
            unsafe { NonNull::new_unchecked(slice::from_raw_parts_mut(new, size)) }
        }
    }

    fn write_padding(current: *mut u8, value: usize) {
        let addr = unsafe { current.sub(value) };
        match value {
            0 => {}
            1..=255 => unsafe { ptr::write(addr, value as u8) },
            _ => unsafe { ptr::write_unaligned(addr as *mut usize, value) },
        }
    }

    fn read_padding(current: *mut u8, layout: Layout) -> usize {
        let addr = unsafe { current.sub(layout.size()) };
        match addr as usize % layout.align() {
            0 => 0,
            1..=255 => unsafe { ptr::read(addr) as usize },
            _ => unsafe { ptr::read_unaligned(addr as *const usize) },
        }
    }

    fn compute_padding(current: *const u8, layout: Layout) -> usize {
        (current as usize - layout.size()) & (layout.align() - 1)
    }

    #[cold]
    #[inline(never)]
    fn alloc_new(&self, layout: Layout) -> NonNull<[u8]> {
        let mut chunks = self.chunks.take();
        let mut garbage = self.free.take();

        // we need a reserve in case the allocation is not aligned
        // for the layout
        let min_size = layout.pad_to_align().size() + layout.align();

        let reuse = garbage
            .last_mut()
            .map_or(false, |chunk| chunk.len() >= layout.size());
        let mut new_chunk = if reuse {
            // SAFETY: Branch implies that garbage contains something.
            unsafe { garbage.pop().unwrap_unchecked() }
        } else {
            Chunk::new(self.chunk_size.max(min_size))
        };

        // SAFETY: we are the unique owner of the chunk
        let Range { start, end } = new_chunk.range();
        let padding = Self::compute_padding(end, layout);
        Self::write_padding(end, padding);
        let size = layout.size() + padding;
        // SAFETY: former code ensures that there is enough space
        let new = unsafe { end.sub(size) };
        self.current.set(new);
        self.start.set(start);

        chunks.push(new_chunk);
        self.chunks.set(chunks);
        self.free.set(garbage);

        // SAFETY: Allocation with address range range 0..n should never happen
        unsafe { NonNull::new_unchecked(slice::from_raw_parts_mut(new, size)) }
    }

    /// Clears the allocator for reuse.
    /// # Safety
    /// Function can be called only if no references to allocated data exist.
    pub unsafe fn clear(&self) {
        let mut chunks = self.chunks.take();
        let mut free = self.free.take();

        free.extend(chunks.drain(1..));

        // SAFETY: Chunks are never empty
        let last = unsafe { chunks.last_mut().unwrap_unchecked() };
        let range = last.range();
        self.current.set(range.end as *mut _);
        self.start.set(range.start as *mut _);

        self.chunks.set(chunks);
        self.free.set(free);
    }
}

impl AllocatorLow<true> {
    /// # Safety
    /// `previous_size` must be the size of the allocation at `ptr`.
    pub unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        previous_layout: Layout,
        new_layout: Layout,
    ) -> NonNull<[u8]> {
        let reused = self.try_free(ptr, previous_layout);

        let mut new = self.alloc(new_layout);

        let copy_method = if reused {
            ptr::copy
        } else {
            ptr::copy_nonoverlapping
        };

        copy_method(
            ptr.as_ptr(),
            new.as_mut().as_mut_ptr(),
            previous_layout.size(),
        );

        new
    }

    /// try to free the memory at `ptr` if it is the last allocation in the current chunk
    /// # Safety
    /// `size` mush match the size of the allocation at `ptr`.
    pub unsafe fn try_free(&self, ptr: NonNull<u8>, layout: Layout) -> bool {
        let current = self.current.get();
        if current != ptr.as_ptr() {
            return false;
        }

        let padding = Self::read_padding(current, layout);
        let restored = current.add(layout.size() + padding);
        self.current.set(restored);

        true
    }
}

struct Chunk {
    data: region::Allocation,
}

impl Chunk {
    fn new(cap: usize) -> Self {
        // SAFETY: even if `cap` is 0, we still get a
        // dangling pointer which is not zero.
        Chunk {
            data: region::alloc(cap, region::Protection::READ_WRITE)
                .expect("Failed to allocate memory"),
        }
    }

    fn len(&self) -> usize {
        self.data.len()
    }

    fn range(&mut self) -> Range<*mut u8> {
        self.data.as_mut_ptr_range()
    }

    fn set_memory_protection(&mut self, protection: region::Protection) {
        let range = self.range();
        let size = range.end as usize - range.start as usize;
        unsafe {
            region::protect(range.start, size, protection)
                .expect("Failed to set memory protection");
        }
    }
}

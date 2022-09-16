use std::{
    cell::Cell,
    mem::{self, MaybeUninit},
    num::NonZeroUsize,
    ops::Range,
    ptr::{self, NonNull},
};

pub struct Allocator {
    garbage: Cell<Vec<Chunk>>,
    chunks: Cell<Vec<Chunk>>,
    current: Cell<*mut MaybeUninit<usize>>,
    start: Cell<*mut MaybeUninit<usize>>,
}

impl Default for Allocator {
    fn default() -> Self {
        Self::new()
    }
}

impl Allocator {
    const CHUNK_SIZE: usize = 1024;
    const MAX_CHUNK_SIZE: usize = 1024 * 1024 * 2;

    pub fn new() -> Self {
        let chunk = Chunk::new(Self::CHUNK_SIZE);
        let range = chunk.range();
        Self {
            garbage: Cell::new(Vec::new()),
            chunks: Cell::new(vec![chunk]),
            current: Cell::new(range.end as *mut _),
            start: Cell::new(range.start as *mut _),
        }
    }

    pub fn alloc(&self, size: NonZeroUsize) -> NonNull<MaybeUninit<usize>> {
        let current = self.current.get();
        let start = self.start.get();
        let size = size.get();

        if current as usize - (start as usize) < size * mem::size_of::<usize>() {
            self.alloc_new(size)
        } else {
            // SAFETY: We just checked that the current chunk has enough space
            let new = unsafe { current.sub(size) };
            self.current.set(new);
            // SAFETY: Allocation with address range range 0..n should never happen
            unsafe { NonNull::new_unchecked(new) }
        }
    }

    #[cold]
    #[inline(never)]
    fn alloc_new(&self, size: usize) -> NonNull<MaybeUninit<usize>> {
        let mut chunks = self.chunks.take();
        let mut garbage = self.garbage.take();

        let reuse = garbage
            .last_mut()
            .map_or(false, |chunk| chunk.len() >= size);
        let new = if reuse {
            // SAFETY: Branch implies that garbage contains something.
            unsafe { garbage.pop().unwrap_unchecked() }
        } else {
            let size =
                unsafe { chunks.last().unwrap_unchecked().len() * 2 }.min(Self::MAX_CHUNK_SIZE);
            Chunk::new(size)
        };

        let mut range = new.range();
        // SAFETY: former code ensures that there is enough space
        range.start = unsafe { range.end.sub(size) };
        self.current.set(range.end as *mut _);
        self.start.set(range.start as *mut _);

        chunks.push(new);
        self.chunks.set(chunks);
        self.garbage.set(garbage);

        // SAFETY: Allocation with address range range 0..n should never happen
        unsafe { NonNull::new_unchecked(range.start as *mut _) }
    }

    pub unsafe fn grow(
        &self,
        ptr: NonNull<MaybeUninit<usize>>,
        previous_size: NonZeroUsize,
        new_size: NonZeroUsize,
    ) -> NonNull<MaybeUninit<usize>> {
        let reused = self.try_free(ptr, previous_size);
        let new = self.alloc(new_size);

        let copy_method = if reused {
            ptr::copy
        } else {
            ptr::copy_nonoverlapping
        };

        copy_method(ptr.as_ptr(), new.as_ptr(), previous_size.get());

        new
    }

    pub unsafe fn try_free(&self, ptr: NonNull<MaybeUninit<usize>>, size: NonZeroUsize) -> bool {
        let current = self.current.get();
        if current != ptr.as_ptr() {
            return false;
        }

        let restored = current.add(size.get());
        self.current.set(restored);

        true
    }

    pub fn clear(&mut self) {
        self.garbage.get_mut().append(self.chunks.get_mut());
        self.garbage
            .get_mut()
            .sort_unstable_by_key(|check| check.len());
        // SAFETY: Garbage is never empty at this point, since appended chunks are never empty
        let last = unsafe { self.garbage.get_mut().pop().unwrap_unchecked() };
        let range = last.range();
        self.chunks.get_mut().push(last);
        self.current.set(range.end as *mut _);
        self.start.set(range.start as *mut _);
    }
}

struct Chunk {
    data: NonNull<[MaybeUninit<usize>]>,
}

impl Chunk {
    fn new(cap: usize) -> Self {
        // SAFETY: even if `cap` is 0, we still get a
        // dangling pointer which is not zero.
        unsafe {
            Chunk {
                data: NonNull::new_unchecked(Box::into_raw(Box::new_uninit_slice(cap))),
            }
        }
    }

    fn len(&self) -> usize {
        unsafe { self.data.as_ref().len() }
    }

    fn range(&self) -> Range<*const MaybeUninit<usize>> {
        unsafe { self.data.as_ref().as_ptr_range() }
    }
}

impl Drop for Chunk {
    fn drop(&mut self) {
        unsafe { Box::from_raw(self.data.as_ptr()) };
    }
}

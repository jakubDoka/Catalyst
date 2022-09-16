use core::slice;
use std::{
    alloc::Layout,
    cell::Cell,
    mem::{self, MaybeUninit},
    num::NonZeroUsize,
    ops::Range,
    ptr::{copy_nonoverlapping, NonNull},
};

#[derive(Default)]
pub struct Arena {
    allocator: Allocator,
}

impl Arena {
    pub fn new() -> Self {
        Self::from_allocator(Allocator::new())
    }

    pub fn from_allocator(allocator: Allocator) -> Self {
        Self { allocator }
    }

    pub fn alloc<T: Copy>(&self, value: T) -> &T {
        let size = Layout::new::<T>()
            .align_to(mem::align_of::<usize>())
            .unwrap()
            .pad_to_align()
            .size()
            / mem::size_of::<usize>();

        let Some(size) = NonZeroUsize::new(size) else {
            return unsafe { &mut *NonNull::dangling().as_ptr() };
        };

        let ptr = self.allocator.alloc(size);
        unsafe {
            (ptr.as_ptr() as *mut T).write(value);
            &*(ptr.as_ptr() as *const T)
        }
    }

    // pub fn alloc_iter<T: Copy, I: IntoIterator<Item = T>>(&self, iter: I) -> &[T] {

    // }

    pub fn alloc_slice<T: Copy>(&self, value: &[T]) -> &[T] {
        let size = Layout::array::<T>(value.len())
            .unwrap()
            .align_to(mem::align_of::<usize>())
            .unwrap()
            .pad_to_align()
            .size()
            / mem::size_of::<usize>();

        let Some(size) = NonZeroUsize::new(size) else {
            return &[];
        };

        let ptr = self.allocator.alloc(size);
        unsafe {
            copy_nonoverlapping(value.as_ptr(), ptr.as_ptr() as *mut _, value.len());
            slice::from_raw_parts(ptr.as_ptr() as *const _, value.len())
        }
    }

    pub fn clear(&mut self) {
        self.allocator.clear();
    }

    pub fn into_allocator(mut self) -> Allocator {
        self.clear();
        self.allocator
    }
}

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

    fn new() -> Self {
        let chunk = Chunk::new(Self::CHUNK_SIZE);
        let range = chunk.range();
        Self {
            garbage: Cell::new(Vec::new()),
            chunks: Cell::new(vec![chunk]),
            current: Cell::new(range.end as *mut _),
            start: Cell::new(range.start as *mut _),
        }
    }

    fn alloc(&self, size: NonZeroUsize) -> NonNull<MaybeUninit<usize>> {
        let current = self.current.get();
        let start = self.start.get();
        let size = size.get();

        if current as usize - (start as usize) < size {
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

    fn clear(&mut self) {
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_safety() {
        let arena = Arena::new();

        let a = arena.alloc(1);
        let b = arena.alloc(&*a);

        let other_arena = Arena::new();

        let c = other_arena.alloc(&*b);

        assert_eq!(***c, 1);
    }

    #[test]
    fn test_expand() {
        let arena = Arena::new();

        for i in 0usize..1024 + 1 {
            arena.alloc(i);
        }
    }

    #[test]
    fn clear() {
        let mut arena = Arena::new();

        for i in 0usize..1024 + 1 {
            arena.alloc(i);
        }

        arena.clear();

        for i in 0usize..1024 + 1 {
            arena.alloc(i);
        }
    }
}

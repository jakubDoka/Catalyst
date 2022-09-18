use crate::*;
use core::slice;
use std::{
    alloc::Layout,
    mem::{self},
    num::NonZeroUsize,
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_safety() {
        let arena = Arena::new();

        let a = arena.alloc(1);
        let b = arena.alloc(a);

        let other_arena = Arena::new();

        let c = other_arena.alloc(b);

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

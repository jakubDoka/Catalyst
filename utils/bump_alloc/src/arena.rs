use crate::*;
use core::slice;
use std::{
    alloc::Layout,
    mem::{self, MaybeUninit},
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

    pub fn alloc<T>(&self, value: T) -> &T {
        const { assert!(!mem::needs_drop::<T>()) };

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

    pub fn alloc_slice<T>(&self, value: &[T]) -> &[T] {
        const { assert!(!mem::needs_drop::<T>()) };

        let Some(size) = Self::array_size::<T>(value.len()) else {
            return &[];
        };

        let ptr = self.allocator.alloc(size);
        unsafe {
            copy_nonoverlapping(value.as_ptr(), ptr.as_ptr() as *mut _, value.len());
            slice::from_raw_parts(ptr.as_ptr() as *const _, value.len())
        }
    }

    pub fn alloc_iter<T, I: IntoIterator<Item = T>>(&self, iter: I) -> &[T] {
        const { assert!(!mem::needs_drop::<T>()) };

        let iter = iter.into_iter();

        if let (low, Some(high)) = iter.size_hint() && low == high {
            let Some(size) = Self::array_size::<T>(low) else {
                return &[];
            };

            let ptr = self.allocator.alloc(size);
            // SAFETY: ptr points to allocation with size >= low * size_of::<T>, memory is 
            // uninitialized
            let slice = unsafe {
                slice::from_raw_parts_mut(ptr.as_ptr() as *mut MaybeUninit<T>, low)
            };

            slice
                .iter_mut()
                .zip(iter)
                .for_each(|(slot, value)| { slot.write(value); });

            // SAFETY: slice was just initialized from iterator
            unsafe {
                return mem::transmute(slice);
            }
        }

        let data = iter.collect::<BumpVec<_>>();

        self.alloc_slice(&data)
    }

    fn array_size<T>(len: usize) -> Option<NonZeroUsize> {
        NonZeroUsize::new(
            Layout::array::<T>(len)
                .unwrap()
                .align_to(mem::align_of::<usize>())
                .unwrap()
                .pad_to_align()
                .size()
                / mem::size_of::<usize>(),
        )
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
        let b = arena.alloc(a as &i32);

        let other_arena = Arena::new();

        let c = other_arena.alloc(b as &&i32);

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

use crate::*;
use core::slice;
use std::{
    alloc::Layout,
    mem::{self, MaybeUninit},
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

        let layout = Layout::new::<T>();

        if layout.size() == 0 {
            return unsafe { &mut *NonNull::dangling().as_ptr() };
        };

        let ptr = self.allocator.alloc(layout);
        unsafe {
            (ptr.as_ptr() as *mut T).write(value);
            &*(ptr.as_ptr() as *const T)
        }
    }

    pub fn alloc_slice<T>(&self, value: &[T]) -> &[T] {
        const { assert!(!mem::needs_drop::<T>()) };

        // SAFETY: layout is of existing slice, must be valid
        let layout = unsafe { Layout::array::<T>(value.len()).unwrap_unchecked() };

        if layout.size() == 0 {
            return &[];
        };

        let ptr = self.allocator.alloc(layout);
        unsafe {
            copy_nonoverlapping(value.as_ptr(), ptr.as_ptr() as *mut _, value.len());
            slice::from_raw_parts(ptr.as_ptr() as *const _, value.len())
        }
    }

    pub fn alloc_iter<T, I>(&self, iter: I) -> &[T]
    where
        I: IntoIterator<Item = T>,
        I::IntoIter: ExactSizeIterator,
    {
        const { assert!(!mem::needs_drop::<T>()) };

        let iter = iter.into_iter();
        let len = iter.len();
        let layout = Layout::array::<T>(len).expect("layout of resulting allocation is invalid");

        let ptr = self.allocator.alloc(layout);
        // SAFETY: layout should represent valid slice
        let slice = unsafe { slice::from_raw_parts_mut(ptr.as_ptr() as *mut MaybeUninit<T>, len) };

        slice.iter_mut().zip(iter).for_each(|(slot, value)| {
            slot.write(value);
        });

        // SAFETY: slice was just initialized from iterator
        unsafe { mem::transmute(slice) }
    }

    pub fn clear(&mut self) {
        // unique access means there is no valid reference eto this
        unsafe { self.allocator.clear() };
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
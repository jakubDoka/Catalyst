use crate::*;
use core::slice;
use std::{
    alloc::Layout,
    cell::UnsafeCell,
    mem::{self, MaybeUninit},
    ops::Deref,
    ptr::{copy_nonoverlapping, NonNull},
};

/// Arena is safe wrapper around `Allocator` that allows to allocate values that dont need
/// dropping.
#[derive(Default)]
#[repr(transparent)]
pub struct Arena {
    allocator: UnsafeCell<Allocator>,
}

unsafe impl Send for Arena {}

impl Arena {
    /// Create new arena with given allocator.
    pub fn new(allocator: Allocator) -> Self {
        Self {
            allocator: allocator.into(),
        }
    }

    // THE FUNCTION IS NOT SOUND, USE WITH CAUTION
    fn allocator(&self) -> &mut Allocator {
        unsafe { &mut *self.allocator.get() }
    }

    /// Allocate value in arena. Value must not need dropping which will be asserted at compile
    /// time.
    #[inline]
    pub fn alloc<T>(&self, value: T) -> &T {
        const { assert!(!mem::needs_drop::<T>()) };

        let layout = Layout::new::<T>();

        if layout.size() == 0 {
            return unsafe { &mut *NonNull::dangling().as_ptr() };
        };

        let ptr = self.allocator().alloc(layout, false);
        unsafe {
            (ptr.as_ptr() as *mut T).write(value);
            &*(ptr.as_ptr() as *const T)
        }
    }

    /// Analogous to `alloc` but for slices.
    #[inline]
    pub fn alloc_slice<T>(&self, value: &[T]) -> &[T] {
        const { assert!(!mem::needs_drop::<T>()) };

        // SAFETY: layout is of existing slice, must be valid
        let layout = unsafe { Layout::array::<T>(value.len()).unwrap_unchecked() };

        if layout.size() == 0 {
            return &[];
        };

        let ptr = self.allocator().alloc(layout, false);
        unsafe {
            copy_nonoverlapping(value.as_ptr(), ptr.as_ptr() as *mut _, value.len());
            slice::from_raw_parts(ptr.as_ptr() as *const _, value.len())
        }
    }

    /// Analogous to `alloc_slice` but for iterators.
    pub fn alloc_iter<T, I>(&self, iter: I) -> &[T]
    where
        I: IntoIterator<Item = T>,
        I::IntoIter: ExactSizeIterator,
    {
        const { assert!(!mem::needs_drop::<T>()) };

        let iter = iter.into_iter();
        let len = iter.len();
        let layout = Layout::array::<T>(len).expect("layout of resulting allocation is invalid");

        let ptr = self.allocator().alloc(layout, false);
        // SAFETY: layout should represent valid slice
        let slice = unsafe { slice::from_raw_parts_mut(ptr.as_ptr() as *mut MaybeUninit<T>, len) };

        slice.iter_mut().zip(iter).for_each(|(slot, value)| {
            slot.write(value);
        });

        // SAFETY: slice was just initialized from iterator
        unsafe { mem::transmute(slice) }
    }

    /// More low level function that allocates sequence of arbitrary bytes compatible with passed
    /// layout.
    #[allow(clippy::mut_from_ref)]
    pub fn alloc_byte_layout(&self, layout: Layout) -> &mut [u8] {
        let mut ptr = self.allocator().alloc(layout, false);
        unsafe { ptr.as_mut() }
    }

    /// Clear arena, since all allocated values dont need dropping, we can just reset allocator.
    /// This function is safe since it requires mutable arena whitch implies all allocations are
    /// dropped.
    pub fn clear(&mut self) {
        unsafe {
            self.allocator.get_mut().clear();
        }
    }

    /// Get allocator from arena.
    pub fn into_allocator(mut self) -> Allocator {
        self.clear();
        self.allocator.into_inner()
    }

    /// Get frame from arena. This creates checkpoint that can be used to rollback arena to state
    /// before frame was created. Rollback is performed implicitly when frame is dropped.
    pub fn frame(&mut self) -> ArenaFrame {
        ArenaFrame {
            allocator: self.allocator.get_mut().frame(),
        }
    }
}

pub struct ArenaFrame<'a> {
    allocator: AllocatorFrame<'a>,
}

impl<'a> Deref for ArenaFrame<'a> {
    type Target = Arena;

    fn deref(&self) -> &Self::Target {
        unsafe { mem::transmute(&*self.allocator) }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_safety() {
        let arena = Arena::default();

        let a = arena.alloc(1);
        let b = arena.alloc(a as &i32);

        let other_arena = Arena::default();

        let c = other_arena.alloc(b as &&i32);

        assert_eq!(***c, 1);
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_expand() {
        let arena = Arena::default();

        for i in 0usize..1024 + 1 {
            arena.alloc(i);
        }
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn clear() {
        let mut arena = Arena::default();

        for i in 0usize..1024 + 1 {
            arena.alloc(i);
        }

        arena.clear();

        for i in 0usize..1024 + 1 {
            arena.alloc(i);
        }
    }
}

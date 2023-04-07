use crate::*;
use core::slice;
use std::{
    alloc::Layout,
    cell::UnsafeCell,
    mem::{self, ManuallyDrop, MaybeUninit},
    ptr::{copy_nonoverlapping, NonNull},
};

use super::AllocatorFrame;

#[macro_export]
macro_rules! proxy_arena {
    (let $arena:ident = $proxy:expr) => {
        let mut scope = $proxy.scope();
        #[allow(unused_mut)]
        let mut $arena = scope.proxy();
    };

    (let $arena:ident) => {
        let mut allocator = $crate::ALLOCATOR_POOL.get_or_default();
        let mut arena = Arena::new(&mut allocator);
        #[allow(unused_mut)]
        let mut $arena = arena.proxy();
    };
}

pub struct ProxyArena<'scope> {
    inner: ManuallyDrop<Arena<'scope>>,
}

impl<'scope> ProxyArena<'scope> {
    /// Allocate value in arena. Value must not need dropping which will be asserted at compile
    /// time.
    #[inline]
    pub fn alloc<T>(&self, value: T) -> &'scope T {
        unsafe { mem::transmute(self.inner.alloc(value)) }
    }

    /// Analogous to `alloc` but for slices.
    #[inline]
    pub fn alloc_slice<T>(&self, value: &[T]) -> &'scope [T] {
        unsafe { mem::transmute(self.inner.alloc_slice(value)) }
    }

    /// Analogous to `alloc_slice` but for iterators.
    pub fn alloc_iter<T, I>(&self, iter: I) -> &'scope [T]
    where
        I: IntoIterator<Item = T>,
        I::IntoIter: ExactSizeIterator,
    {
        unsafe { mem::transmute(self.inner.alloc_iter(iter)) }
    }

    /// More low level function that allocates sequence of arbitrary bytes compatible with passed
    /// layout.
    #[allow(clippy::mut_from_ref)]
    pub fn alloc_byte_layout(&self, layout: Layout) -> &'scope mut [u8] {
        unsafe { mem::transmute(self.inner.alloc_byte_layout(layout)) }
    }

    pub fn scope(&mut self) -> Arena {
        unsafe { self.inner.snapshot() }
    }
}

/// Arena is safe wrapper around `Allocator` that allows to allocate values that dont need
/// dropping.
pub struct Arena<'scope> {
    allocator: UnsafeCell<&'scope mut Allocator>,
    frame: AllocatorFrame,
}

unsafe impl Send for Arena<'_> {}

impl<'scope> Arena<'scope> {
    /// Create new arena with given allocator.
    pub fn new(allocator: &'scope mut Allocator) -> Self {
        Self {
            frame: AllocatorFrame::new(allocator),
            allocator: allocator.into(),
        }
    }

    unsafe fn allocator(&self) -> &mut Allocator {
        &mut *self.allocator.get()
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

        let ptr = unsafe { self.allocator() }.alloc(layout, false);
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

        let ptr = unsafe { self.allocator() }.alloc(layout, false);
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

        let ptr = unsafe { self.allocator() }.alloc(layout, false);
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
        let mut ptr = unsafe { self.allocator() }.alloc(layout, false);
        unsafe { ptr.as_mut() }
    }

    /// Creates proxy arena that can allocate for its lifetime.
    pub fn proxy(&mut self) -> ProxyArena {
        ProxyArena {
            inner: ManuallyDrop::new(unsafe { self.snapshot() }),
        }
    }

    unsafe fn snapshot(&mut self) -> Arena {
        Arena {
            frame: AllocatorFrame::new(self.allocator.get_mut()),
            allocator: (*self.allocator.get_mut()).into(),
        }
    }

    /// Clears arena and returns all allocated memory to allocator.
    pub fn clear(&mut self) {
        unsafe { self.frame.revert(self.allocator.get_mut()) };
    }
}

impl<'scope> Drop for Arena<'scope> {
    fn drop(&mut self) {
        self.clear();
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_safety() {
        let mut allocator = Allocator::default();
        let mut arena = Arena::new(&mut allocator);
        let mut arena = arena.proxy();

        let a = arena.alloc(1);
        let b = arena.alloc(a as &i32);

        let other_arena = arena.scope();

        let c = other_arena.alloc(b as &&i32);

        assert_eq!(***c, 1);
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_expand() {
        let mut allocator = Allocator::default();
        let arena = Arena::new(&mut allocator);

        for i in 0usize..1024 + 1 {
            arena.alloc(i);
        }
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn clear() {
        let mut allocator = Allocator::default();
        let mut arena = Arena::new(&mut allocator);

        let proxy = arena.proxy();
        for i in 0usize..1024 + 1 {
            proxy.alloc(i);
        }

        drop(proxy);

        for i in 0usize..1024 + 1 {
            arena.alloc(i);
        }
    }
}

use std::{
    alloc::AllocError,
    cell::{Cell, UnsafeCell},
    fmt::Debug,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

use crate::*;

thread_local! {
    /// Thread local instance of `BumpAlloc` used by `BumpVec`.
    pub static BUMP_ALLOC: BumpAlloc = BumpAlloc::default();
}

/// The allocator passed to `Vec` embedded in bumpved. It optimizes cases where tail allocation
/// tries to grow or deallocate.
pub struct BumpAllocRef(());

impl Drop for BumpAllocRef {
    #[inline]
    fn drop(&mut self) {
        BUMP_ALLOC.with(|alloc| alloc.drop_ref(self));
    }
}

impl !Send for BumpAllocRef {}
impl !Sync for BumpAllocRef {}

unsafe impl std::alloc::Allocator for BumpAllocRef {
    fn allocate(&self, layout: std::alloc::Layout) -> Result<NonNull<[u8]>, AllocError> {
        Ok(BUMP_ALLOC.with(|alloc| alloc.allocator().alloc(layout, true)))
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: std::alloc::Layout) {
        BUMP_ALLOC.with(|alloc| alloc.allocator().try_free(ptr, layout));
    }

    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: std::alloc::Layout,
        new_layout: std::alloc::Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        debug_assert!(
            new_layout.size() >= old_layout.size(),
            "`new_layout.size()` must be greater than or equal to `old_layout.size()`"
        );

        Ok(BUMP_ALLOC.with(|alloc| alloc.allocator().grow(ptr.cast(), old_layout, new_layout)))
    }
}

/// Thread local allocator used by `BumpVec`. It is very simple bump allocator
/// that frees memory when all `BumpAllocRef` are dropped.
#[derive(Default)]
pub struct BumpAlloc {
    refs: Cell<usize>,
    allocator: UnsafeCell<Allocator>,
}

impl BumpAlloc {
    fn allocator(&self) -> &mut Allocator {
        unsafe { &mut *self.allocator.get() }
    }

    #[inline]
    fn create_ref(&self) -> BumpAllocRef {
        self.refs.set(self.refs.get() + 1);
        BumpAllocRef(())
    }

    #[inline]
    fn drop_ref(&self, _: &mut BumpAllocRef) {
        if self.refs.replace(self.refs.get() - 1) == 1 {
            #[cold]
            #[inline(never)]
            fn clear(s: &BumpAlloc) {
                unsafe { s.allocator().clear() };
            }

            clear(self)
        }
    }
}

/// `Vec` wrapper that uses `BumpAlloc` as allocator. Great for short lived temporary
/// allocations, offers cache locality since allocations are placed next to each other and
/// are deallocated when located at the end of the bump allocator.
pub struct BumpVec<T> {
    inner: Vec<T, BumpAllocRef>,
}

impl<T: Debug> Debug for BumpVec<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.inner.iter()).finish()
    }
}

impl<T> From<BumpVec<T>> for Vec<T, BumpAllocRef> {
    fn from(bump_vec: BumpVec<T>) -> Self {
        bump_vec.inner
    }
}

impl<T> BumpVec<T> {
    /// Analogous to `Vec::new`.
    pub fn new() -> Self {
        Self {
            inner: Vec::new_in(BUMP_ALLOC.with(|b| b.create_ref())),
        }
    }

    /// Analogous to `Vec::with_capacity`.
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            inner: Vec::with_capacity_in(capacity, BUMP_ALLOC.with(|b| b.create_ref())),
        }
    }
}

impl<T> Default for BumpVec<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> IntoIterator for BumpVec<T> {
    type Item = T;

    type IntoIter = std::vec::IntoIter<T, BumpAllocRef>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

impl<T> FromIterator<T> for BumpVec<T> {
    #[inline]
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let iter = iter.into_iter();
        let (size_hint, _) = iter.size_hint();
        let mut s = Self::with_capacity(size_hint);
        s.extend(iter);
        s
    }
}

impl<T> Deref for BumpVec<T> {
    type Target = Vec<T, BumpAllocRef>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> DerefMut for BumpVec<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

pub trait ToBumpVec<T> {
    fn to_bumpvec(&self) -> BumpVec<T>;
}

impl<T: Clone> ToBumpVec<T> for [T] {
    #[inline]
    fn to_bumpvec(&self) -> BumpVec<T> {
        let mut vec = BumpVec::new();
        vec.extend_from_slice(self);
        vec
    }
}

/// Offers a convenient way to create `BumpVec`s.
/// ```
/// use catalyst_entities::bumpvec;
/// let enumerated = bumpvec![1, 2, 3];
/// let repeated = bumpvec![0; 10];
/// let with_capacity = bumpvec!(cap 10);
/// ```
#[macro_export]
macro_rules! bumpvec {
    () => {
        $crate::BumpVec::new()
    };
    ($($x:expr),+) => {
        {
            let mut vec = $crate::BumpVec::new();
            vec.extend([$($x),*]);
            vec
        }
    };
    ($expr:expr; $len:expr) => {
        {
            let mut v = $crate::BumpVec::new();
            v.resize($len, $expr);
            v
        }
    };
    (cap $cap:expr) => {
        $crate::BumpVec::with_capacity($cap)
    };
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    #[cfg_attr(miri, ignore)]
    fn burst_alloc_comparison() {
        bench(&[
            ("normal-vec", |timer| {
                timer.start();
                let mut ptr = 0;
                for _ in 0..1_000_000 {
                    let vec = Vec::<usize>::with_capacity(100);
                    ptr += vec.as_ptr() as usize;
                }
                timer.stop();
                println!("{ptr:?}");
            }),
            ("bump-vec", |timer| {
                let vec: BumpVec<usize> = bumpvec!(cap 100);
                timer.start();
                let mut ptr = 0;
                for _ in 0..1_000_000 {
                    let vec = BumpVec::<usize>::with_capacity(100);
                    ptr += vec.as_ptr() as usize;
                }
                timer.stop();
                drop(vec);
                println!("{ptr:?}");
            }),
            ("no-alloc", |timer| {
                let vec = vec![0; 100].as_ptr();
                timer.start();
                let mut ptr = 0;
                for _ in 0..1_000_000 {
                    ptr += vec as usize;
                    ptr /= 3;
                    ptr *= 2;
                }
                timer.stop();
                println!("{ptr:?}");
            }),
        ])
    }

    #[derive(Default)]
    struct Timer {
        from: Option<std::time::Instant>,
        to: Option<std::time::Instant>,
    }

    impl Timer {
        fn start(&mut self) {
            self.from = Some(std::time::Instant::now());
        }

        fn stop(&mut self) {
            self.to = Some(std::time::Instant::now());
        }

        fn summary(self, name: &str) {
            let start = self.from.unwrap();
            let end = self.to.unwrap_or_else(std::time::Instant::now);
            println!("{name} {:?}", end.duration_since(start));
        }
    }

    type BenchTasks<'a> = &'a [(&'a str, fn(&mut Timer))];

    fn bench(task: BenchTasks) {
        std::thread::scope(|s| {
            for (name, f) in task {
                s.spawn(|| {
                    let mut timer = Timer::default();
                    timer.start();
                    f(&mut timer);
                    timer.summary(name);
                });
            }
        });
    }
}

use arc_swap::ArcSwapAny;
use std::{
    cell::{Cell, UnsafeCell},
    sync::{
        atomic::{AtomicPtr, Ordering},
        Arc,
    },
};

use super::*;

pub struct SyncFragMap<T, A: Allocator = Global> {
    base: SyncFragBase<T, A>,
    locals: Box<[LocalFragView<T, A>]>,
    thread: u8,
}

impl<T: Clone, A: Allocator + Clone> SyncFragMap<T, A> {
    fn new(thread: u8, base: &SyncFragBase<T, A>) -> Self {
        Self {
            base: base.clone(),
            locals: base
                .views
                .iter()
                .map(SyncFragView::clone_to_local)
                .map(UnsafeCell::new)
                .collect(),
            thread,
        }
    }

    pub fn extend<I: IntoIterator<Item = T>>(&self, values: I) -> FragSlice<T>
    where
        I::IntoIter: ExactSizeIterator,
    {
        let (slice, reallocated) = self.base.views[self.thread as usize].extend(values.into_iter());
        if reallocated {}
        slice
    }
}

impl<T, A: Allocator> SyncFragMap<T, A> {
    fn get_thread(&self) -> &SyncFragView<T, A> {
        // thread is alwais in range
        unsafe { self.base.views.get_unchecked(self.thread as usize) }
    }

    fn get_local_thread(&self) -> &LocalFragView<T, A> {
        // thread is always in range
        unsafe { self.locals.get_unchecked(self.thread as usize) }
    }
}

impl<T, A: Allocator> Index<FragSlice<T>> for SyncFragMap<T, A> {
    type Output = [T];

    fn index(&self, index: FragSlice<T>) -> &Self::Output {
        let (index, thread, len) = index.parts();
        let range = index as usize..index as usize + len as usize;

        if let Some(slice) = unsafe { self.get_local(thread) }.get_slice(range.clone()) {
            return slice;
        }

        self.locals[thread as usize] = self.base.views[thread as usize].clone_to_local();

        self.locals[thread as usize]
            .get_slice(range)
            .expect("index out of bounds")
    }
}

pub struct SyncFragBase<T, A: Allocator = Global> {
    views: Arc<[SyncFragView<T, A>]>,
}

impl<T: Clone> SyncFragBase<T> {
    pub fn new(thread_count: u8) -> Self {
        Self::new_in(thread_count, Global)
    }
}

impl<T, A: Allocator> Clone for SyncFragBase<T, A> {
    fn clone(&self) -> Self {
        Self {
            views: self.views.clone(),
        }
    }
}

impl<T: Clone, A: Allocator + Clone> SyncFragBase<T, A> {
    pub fn new_in(thread_count: u8, allocator: A) -> Self {
        Self {
            views: (0..thread_count)
                .map(|thread| SyncFragView::new_in(thread, allocator.clone()))
                .collect(),
        }
    }

    pub fn split(&mut self) -> impl Iterator<Item = SyncFragMap<T, A>> + '_ {
        assert!(Arc::get_mut(&mut self.views).is_none());
        (0..self.views.len()).map(|t| SyncFragMap::new(t as u8, self))
    }
}

struct SyncFragView<T, A: Allocator = Global> {
    inner: ArcSwapAny<FragVecInner<T, A>>,
    thread: u8,
    len: AtomicUsize,
}

impl<T, A: Allocator> SyncFragView<T, A> {
    fn new_in(thread: u8, allocator: A) -> Self {
        Self {
            inner: FragVecInner::new(allocator).as_ptr().into(),
            thread,
            len: 0.into(),
        }
    }

    fn load_inner(&self) -> NonNull<FragVecInner<T, A>> {
        unsafe { NonNull::new_unchecked(self.inner.load(Ordering::Relaxed)) }
    }

    fn load_len(&self) -> usize {
        self.len.load(Ordering::Relaxed)
    }

    fn clone_to_local(&self) -> LocalFragView<T, A> {
        let local = LocalFragView {
            inner: self.load_inner(),
            thread: self.thread,
            len: self.load_len(),
        };

        unsafe {
            FragVecInner::inc(local.inner);
        }

        local
    }

    fn extend(&self, values: impl ExactSizeIterator<Item = T>) -> (FragSlice<T>, bool)
    where
        T: Clone,
        A: Clone,
    {
        let len = self.load_len();
        let Ok(values_len) = values.len().try_into() else {
            crate::terminate!("SyncFragView:extend: too may values");
        };
        let (.., possibly_new) = unsafe { FragVecInner::extend(self.load_inner(), len, values) };
        if let Some(new) = possibly_new {
            self.inner.store(new.as_ptr(), Ordering::Relaxed);
        }
        let slice = FragSlice::new(FragSliceAddr::new(len as u64, self.thread, values_len));
        let prev = self.load_len();
        self.len
            .store(prev + values_len as usize, Ordering::Relaxed);
        (slice, possibly_new.is_some())
    }
}

impl<T, A: Allocator> Drop for SyncFragView<T, A> {
    fn drop(&mut self) {
        unsafe {
            FragVecInner::dec(self.load_inner(), self.load_len());
        }
    }
}

struct LocalFragView<T, A: Allocator = Global> {
    inner: NonNull<FragVecInner<T, A>>,
    thread: u8,
    len: usize,
}

impl<T, A: Allocator> LocalFragView<T, A> {
    pub fn get(&self, index: usize) -> Option<&T> {
        unsafe { FragVecInner::data(self.inner, self.len).get(index) }
    }

    fn get_slice(&self, range: Range<usize>) -> Option<&[T]> {
        unsafe { FragVecInner::data(self.inner, self.len).get(range) }
    }
}

impl<T, A: Allocator> Drop for LocalFragView<T, A> {
    fn drop(&mut self) {
        unsafe {
            FragVecInner::dec(self.inner, self.len);
        }
    }
}

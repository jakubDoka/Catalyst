use arc_swap::ArcSwapAny;
use std::{
    cell::UnsafeCell,
    hash::Hash,
    sync::{atomic::Ordering, Arc},
};

use crate::{DynFragMap, Relocated};

use super::*;

pub struct SyncFragMap<T, A: Allocator = Global> {
    base: SyncFragBase<T, A>,
    locals: Box<[UnsafeCell<LocalFragView<T, A>>]>,
    thread: u8,
}

impl<T, A: Allocator> SyncFragMap<T, A> {
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

    pub fn extend<I: IntoIterator<Item = T>>(&mut self, values: I) -> FragSlice<T>
    where
        T: Clone,
        A: Clone,
        I::IntoIter: ExactSizeIterator,
    {
        let thread = self.get_thread();
        let (slice, reallocated) = thread.extend(values.into_iter());
        if reallocated {
            self.fallback(self.thread);
        }
        slice
    }

    /// # Safety
    /// Caller must ensure the slice is last one pushed
    pub unsafe fn unextend(&mut self, slice: FragSlice<T>) {
        let (index, thread, len) = slice.parts();
        self.base.views[thread as usize].unextend(index, len);
    }

    pub fn push(&mut self, value: T) -> FragRef<T>
    where
        T: Clone,
        A: Clone,
    {
        let (index, thread, ..) = self.extend(iter::once(value)).parts();
        FragRef::new(FragAddr::new(index, thread))
    }
}

impl<T, A: Allocator> SyncFragMap<T, A> {
    fn get_thread(&self) -> &SyncFragView<T, A> {
        // thread is alwais in range
        unsafe { self.base.views.get_unchecked(self.thread as usize) }
    }

    unsafe fn index(&self, addr: FragRef<T>) -> &T {
        let (index, thread) = addr.parts();
        // we do not implemend sync and threads are unique
        if (*self.locals[thread as usize].get())
            .get(index as usize)
            .is_none()
        {
            self.fallback(thread);
        }

        (*self.locals[thread as usize].get())
            .get(index as usize)
            .expect("index out of bounds")
    }

    #[cold]
    #[inline(never)]
    fn fallback(&self, thread: u8) {
        // SAFETY: the structure is not sync
        unsafe {
            *self.locals[thread as usize].get() = self.base.views[thread as usize].clone_to_local();
        }
    }

    unsafe fn slice(&self, slice: FragSlice<T>) -> &[T] {
        let (index, thread, len) = slice.parts();
        let range = index as usize..index as usize + len as usize;
        if (*self.locals[thread as usize].get())
            .slice(range.clone())
            .is_none()
        {
            self.fallback(thread);
        }

        (*self.locals[thread as usize].get())
            .slice(range)
            .expect("range out of bounds")
    }
}

impl<T, A: Allocator> Index<FragRef<T>> for SyncFragMap<T, A> {
    type Output = T;

    fn index(&self, index: FragRef<T>) -> &Self::Output {
        // SAFETY: the struct is not sync
        unsafe { self.index(index) }
    }
}

impl<T, A: Allocator> Index<FragSlice<T>> for SyncFragMap<T, A> {
    type Output = [T];

    fn index(&self, index: FragSlice<T>) -> &Self::Output {
        // SAFETY: the struct is not sync
        unsafe { self.slice(index) }
    }
}

pub struct SyncFragBase<T, A: Allocator = Global> {
    views: Arc<[SyncFragView<T, A>]>,
}

unsafe impl<T: Send + Sync, A: Allocator + Send + Sync> Sync for SyncFragBase<T, A> {}

impl<T> SyncFragBase<T> {
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

impl<T, A: Allocator> SyncFragBase<T, A> {
    pub fn new_in(thread_count: u8, allocator: A) -> Self
    where
        A: Clone,
    {
        Self {
            views: (0..thread_count)
                .map(|thread| SyncFragView::new_in(thread, allocator.clone()))
                .collect(),
        }
    }

    pub fn split(&self) -> impl Iterator<Item = SyncFragMap<T, A>> + '_ {
        assert!(self.is_unique());
        (0..self.views.len()).map(|t| SyncFragMap::new(t as u8, self))
    }

    pub fn is_unique(&self) -> bool {
        Arc::strong_count(&self.views) == 1
    }
}

impl<T: Send + Sync + Relocated, A: Allocator + Send + Sync> DynFragMap for SyncFragBase<T, A> {
    fn mark(&self, addr: FragAddr, marker: &mut crate::FragRelocMarker) {
        let (index, thread) = addr.parts();
        unsafe {
            FragVecInner::full_data(self.views[thread as usize].inner.load().0)[index as usize]
                .mark(marker);
        }
    }

    fn remap(&mut self, ctx: &crate::FragRelocMapping) {
        self.views
            .iter()
            .map(|v| unsafe { FragVecInner::full_data_mut(v.inner.load().0) })
            .flatten()
            .for_each(|i| i.remap(ctx))
    }

    fn filter(&mut self, marks: &mut crate::FragMarks, mapping: &mut crate::FragRelocMapping) {
        marks.filter_base(
            self.views
                .iter()
                .map(|v| (v.inner.load(), |len| v.len.store(len, Ordering::Relaxed))),
            mapping,
        )
    }

    fn is_unique(&self) -> bool {
        self.is_unique()
    }
}

struct SyncFragView<T, A: Allocator = Global> {
    inner: ArcSwapAny<FragVecArc<T, A>>,
    thread: u8,
    len: AtomicUsize,
}

unsafe impl<T: Sync + Send, A: Allocator + Sync + Send> Sync for SyncFragView<T, A> {}

impl<T, A: Allocator> SyncFragView<T, A> {
    fn new_in(thread: u8, allocator: A) -> Self {
        Self {
            inner: ArcSwapAny::new(FragVecArc(FragVecInner::new(allocator))),
            thread,
            len: 0.into(),
        }
    }

    fn load_len(&self) -> usize {
        self.len.load(Ordering::Relaxed)
    }

    fn clone_to_local(&self) -> LocalFragView<T, A> {
        LocalFragView {
            inner: self.inner.load_full(),
            len: self.load_len(),
        }
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
        let (.., possibly_new) = unsafe { FragVecInner::extend(self.inner.load().0, values) };
        if let Some(new) = possibly_new {
            self.inner.store(FragVecArc(new));
        }
        let slice = FragSlice::new(FragSliceAddr::new(len as u64, self.thread, values_len));
        let prev = self.load_len();
        self.len
            .store(prev + values_len as usize, Ordering::Relaxed);
        (slice, possibly_new.is_some())
    }

    unsafe fn unextend(&self, index: u64, len: u16) {
        FragVecInner::unextend(self.inner.load().0, index, len);
    }
}

struct LocalFragView<T, A: Allocator = Global> {
    inner: FragVecArc<T, A>,
    len: usize,
}

impl<T, A: Allocator> LocalFragView<T, A> {
    pub fn get(&self, index: usize) -> Option<&T> {
        unsafe { FragVecInner::data(self.inner.0, self.len).get(index) }
    }

    fn slice(&self, range: Range<usize>) -> Option<&[T]> {
        unsafe { FragVecInner::data(self.inner.0, self.len).get(range) }
    }
}

pub struct FragSliceKey<T, A: Allocator = Global> {
    view: *const SyncFragView<T, A>,
    slice: FragSlice<T>,
}

unsafe impl<T: Sync + Send, A: Allocator + Sync + Send> Send for FragSliceKey<T, A> {}
unsafe impl<T: Sync + Send, A: Allocator + Sync + Send> Sync for FragSliceKey<T, A> {}

impl<T, A: Allocator> FragSliceKey<T, A> {
    /// # Safety
    /// Caller must ensure that `Self` does not outlive `map` and `slice` is valid
    pub unsafe fn new(map: &SyncFragMap<T, A>, slice: FragSlice<T>) -> Self {
        let (_, thread, ..) = slice.parts();
        let map = &map.base.views[thread as usize];

        Self {
            view: map as _,
            slice,
        }
    }

    unsafe fn inner_slice(&self) -> &[T] {
        let map = &*self.view;
        let (index, .., len) = self.slice.parts();
        let range = index as usize..index as usize + len as usize;
        &FragVecInner::full_data(map.inner.load().0).get_unchecked(range)
    }
}

impl<T: Hash, A: Allocator> Hash for FragSliceKey<T, A> {
    fn hash<H: ~const std::hash::Hasher>(&self, state: &mut H) {
        unsafe {
            self.inner_slice().hash(state);
        }
    }
}

impl<T: Eq, A: Allocator> PartialEq for FragSliceKey<T, A> {
    fn eq(&self, other: &Self) -> bool {
        unsafe { self.inner_slice() == other.inner_slice() }
    }
}

impl<T: Eq, A: Allocator> Eq for FragSliceKey<T, A> {}

#[cfg(test)]
mod test {
    use std::{
        sync::atomic::{AtomicBool, AtomicUsize},
        thread,
        time::Duration,
    };

    use dashmap::DashMap;

    use crate::{FragRef, SyncFragBase};

    struct Tested {
        arbitrary: usize,
    }

    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    impl Tested {
        fn new(arbitrary: usize) -> Self {
            COUNTER.fetch_add(arbitrary, std::sync::atomic::Ordering::Relaxed);
            Self { arbitrary }
        }
    }

    impl Clone for Tested {
        fn clone(&self) -> Self {
            Self::new(self.arbitrary)
        }
    }

    impl Drop for Tested {
        fn drop(&mut self) {
            COUNTER.fetch_sub(self.arbitrary, std::sync::atomic::Ordering::Relaxed);
        }
    }

    #[test]
    fn test() {
        const THREAD_COUNT: usize = 4;
        let dash = &DashMap::<usize, FragRef<Tested>>::default();
        let run = &AtomicBool::new(true);
        let shared = SyncFragBase::new(THREAD_COUNT as u8);
        let counter = &AtomicUsize::new(0);

        thread::scope(|s| {
            for thread in shared.split() {
                s.spawn(move || {
                    let mut thread = thread;
                    while run.load(std::sync::atomic::Ordering::Relaxed) {
                        let id = counter.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                        if let Some(value) = {
                            let i = dash.iter().next().map(|a| a.value().to_owned());
                            i
                        } {
                            let _ = &thread[value];
                            dash.insert(id, value);
                        } else {
                            let t = thread.push(Tested::new(1));
                            dash.insert(id, t);
                        }
                    }
                });
            }

            thread::sleep(Duration::from_millis(10));
            run.store(false, std::sync::atomic::Ordering::Relaxed);
        });
    }
}

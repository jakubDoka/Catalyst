use arc_swap::ArcSwapAny;
use std::sync::{atomic::Ordering, Arc};

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
                .collect(),
            thread,
        }
    }

    pub fn extend<I: IntoIterator<Item = T>>(&mut self, values: I) -> FragSlice<T>
    where
        I::IntoIter: ExactSizeIterator,
    {
        let thread = self.get_thread();
        let (slice, reallocated) = thread.extend(values.into_iter());
        if reallocated {
            self.fallback(self.thread);
        }
        slice
    }

    pub fn push(&mut self, value: T) -> FragRef<T> {
        let (index, thread, ..) = self.extend(iter::once(value)).parts();
        FragRef::new(FragAddr::new(index, thread))
    }
}

impl<T, A: Allocator> SyncFragMap<T, A> {
    fn get_thread(&self) -> &SyncFragView<T, A> {
        // thread is alwais in range
        unsafe { self.base.views.get_unchecked(self.thread as usize) }
    }

    pub fn index(&mut self, addr: FragRef<T>) -> &T {
        let (index, thread) = addr.parts();
        if self.locals[thread as usize].get(index as usize).is_none() {
            self.fallback(thread);
        }

        self.locals[thread as usize]
            .get(index as usize)
            .expect("index out of bounds")
    }

    #[cold]
    #[inline(never)]
    fn fallback(&mut self, thread: u8) {
        self.locals[thread as usize] = self.base.views[thread as usize].clone_to_local();
    }

    pub fn slice(&mut self, slice: FragSlice<T>) -> &[T] {
        let (index, thread, len) = slice.parts();
        let range = index as usize..index as usize + len as usize;
        if self.locals[thread as usize].slice(range.clone()).is_none() {
            self.fallback(thread);
        }

        self.locals[thread as usize]
            .slice(range)
            .expect("range out of bounds")
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
        assert!(Arc::get_mut(&mut self.views).is_some());
        (0..self.views.len()).map(|t| SyncFragMap::new(t as u8, self))
    }
}

struct SyncFragView<T, A: Allocator = Global> {
    inner: ArcSwapAny<FragVecArc<T, A>>,
    thread: u8,
    len: AtomicUsize,
}

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

#[cfg(test)]
mod test {
    use std::{
        iter,
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
        let mut shared = SyncFragBase::new(THREAD_COUNT as u8);
        let counter = &AtomicUsize::new(0);

        thread::scope(|s| {
            for mut thread in shared.split() {
                s.spawn(move || {
                    while run.load(std::sync::atomic::Ordering::Relaxed) {
                        let id = counter.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                        if let Some(value) = {
                            let i = dash.iter().next().map(|a| a.value().to_owned());
                            i
                        } {
                            thread.index(value);
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

use arc_swap::ArcSwapAny;
use std::{
    any::TypeId,
    cell::UnsafeCell,
    default::default,
    hash::Hash,
    mem::ManuallyDrop,
    ops::Deref,
    sync::{
        atomic::{AtomicPtr, Ordering},
        Arc,
    },
};

use crate::{DynFragMap, FragMarks, Relocated};

use super::{relocator::FragMarkShard, *};

pub struct SyncFragMap<T, A: Allocator + Default = Global> {
    pub(crate) base: SyncFragBase<T, A>,
    pub(crate) locals: Box<[UnsafeCell<LocalFragView<T, A>>]>,
    pub(crate) thread: u8,
}

impl<T, A: Allocator + Default> SyncFragMap<T, A> {
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

    pub fn unextend(&mut self, slice: FragSlice<T>) -> impl Iterator<Item = T> + '_ {
        let FragSliceAddr { index, thread, len } = slice.0;
        self.base.views[thread as usize].unextend(index, len)
    }

    pub fn push(&mut self, value: T) -> FragRef<T>
    where
        A: Clone,
    {
        let FragSliceAddr { index, thread, .. } = self.extend(iter::once(value)).addr();
        FragRef::new(FragAddr::new(index, thread))
    }

    pub fn next(&self) -> FragRef<T> {
        let index = self.base.views[self.thread as usize]
            .len
            .load(Ordering::Relaxed);
        FragRef::new(FragAddr::new(index as u32, self.thread))
    }

    fn get_thread(&self) -> &SyncFragView<T, A> {
        // thread is alwais in range
        unsafe { self.base.views.get_unchecked(self.thread as usize) }
    }

    unsafe fn index(&self, addr: FragRef<T>) -> &T {
        let FragAddr { index, thread, .. } = addr.0;
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

    /// # Safety
    /// Ensure addr is vot out of bounds.
    pub unsafe fn index_unchecked(&self, addr: FragRef<T>) -> &T {
        let FragAddr { index, thread, .. } = addr.0;
        // we do not implemend sync and threads are unique
        if (*self.locals.get_unchecked(thread as usize).get())
            .get(index as usize)
            .is_none()
        {
            self.fallback(thread);
        }

        &*ArcVecInner::get_item(
            (*self.locals.get_unchecked(thread as usize).get()).inner.0,
            index as usize,
        )
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
        let FragSliceAddr { index, thread, len } = slice.addr();
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

    /// # Safety
    /// Ensure slice is not out of bounds.
    pub unsafe fn slice_unchecked(&self, slice: FragSlice<T>) -> &[T] {
        let FragSliceAddr { index, thread, len } = slice.addr();
        let range = index as usize..index as usize + len as usize;
        if (*self.locals.get_unchecked(thread as usize).get())
            .slice(range.clone())
            .is_none()
        {
            self.fallback(thread);
        }
        ArcVecInner::full_data((*self.locals.get_unchecked(thread as usize).get()).inner.0)
            .get_unchecked(range)
    }

    pub fn reference(&self, addr: FragRef<T>) -> SyncFragBorrow<T, A>
    where
        T: NoInteriorMutability,
    {
        SyncFragBorrow(&self[addr], unsafe {
            (*self.locals[addr.0.thread as usize].get()).inner.clone()
        })
    }
}

pub struct SyncFragBorrow<T, A: Allocator + Default>(*const T, ArcVec<T, A>);

impl<T, A: Allocator + Default> Deref for SyncFragBorrow<T, A> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.0 }
    }
}

impl<T: NoInteriorMutability, A: Allocator + Default> Index<FragRef<T>> for SyncFragMap<T, A> {
    type Output = T;

    fn index(&self, index: FragRef<T>) -> &Self::Output {
        // SAFETY: the struct is not sync
        // unsafe { self.index_unchecked(index) }
        unsafe { self.index(index) }
    }
}

impl<T: NoInteriorMutability, A: Allocator + Default> Index<FragSlice<T>> for SyncFragMap<T, A> {
    type Output = [T];

    fn index(&self, index: FragSlice<T>) -> &Self::Output {
        // SAFETY: the struct is not sync
        // unsafe { self.slice_unchecked(index) }
        unsafe { self.slice(index) }
    }
}

#[derive(Archive, Deserialize, Serialize)]

pub struct SyncFragBase<T, A: Allocator + Default = Global> {
    pub(crate) views: Arc<[SyncFragView<T, A>]>,
}

unsafe impl<T: Send + Sync, A: Allocator + Default + Send + Sync> Sync for SyncFragBase<T, A> {}

impl<T> SyncFragBase<T, Global> {
    pub fn new(thread_count: u8) -> Self {
        Self::new_in(thread_count, Global)
    }

    pub fn expand(&mut self, thread_count: u8) {
        self.expand_in(thread_count, Global)
    }
}

impl<T, A: Allocator + Default> Clone for SyncFragBase<T, A> {
    fn clone(&self) -> Self {
        Self {
            views: self.views.clone(),
        }
    }
}

impl<T, A: Allocator + Default> SyncFragBase<T, A> {
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

    pub fn expand_in(&mut self, thread_count: u8, allocator: A)
    where
        A: Clone,
    {
        let new = (self.views.len() as u8..thread_count)
            .map(|thread| SyncFragView::new_in(thread, allocator.clone()));
        let current = mem::replace(&mut self.views, Arc::new([]));
        unsafe {
            let mut current: Arc<[ManuallyDrop<SyncFragView<T, A>>]> = mem::transmute(current);
            let current = Arc::get_mut(&mut current)
                .expect("no other references")
                .iter_mut()
                .map(|v| (v as *mut ManuallyDrop<SyncFragView<T, A>>).read());

            self.views = current
                .map(ManuallyDrop::into_inner)
                .chain(new)
                .collect::<Vec<_>>()
                .into();
        };
    }

    pub fn split(&self) -> impl Iterator<Item = SyncFragMap<T, A>> + '_ {
        assert!(self.is_unique());
        (0..self.views.len()).map(|t| SyncFragMap::new(t as u8, self))
    }

    pub fn is_unique(&self) -> bool {
        Arc::strong_count(&self.views) == 1
    }
}

impl<T: Relocated + 'static, A: Allocator + Default + Send + Sync> DynFragMap
    for SyncFragBase<T, A>
{
    fn mark(
        &self,
        FragSliceAddr { index, thread, len }: FragSliceAddr,
        marker: &mut crate::FragRelocMarker,
    ) {
        let thread = &self.views[thread as usize];
        unsafe {
            ArcVecInner::data(thread.inner.load().0, thread.len.as_mut_ptr().read())
                [index as usize..index as usize + len as usize]
                .mark(marker);
        }
    }

    fn remap(&mut self, ctx: &FragMarks) {
        self.views
            .iter()
            .flat_map(|v| unsafe { ArcVecInner::full_data_mut(v.inner.load().0) })
            .for_each(|i| {
                i.remap(ctx);
            })
    }

    fn filter(&mut self, marks: &mut FragMarkShard) {
        marks.filter_base(
            self.views
                .iter()
                .map(|v| (v.inner.load(), |len| v.len.store(len, Ordering::Relaxed))),
        )
    }

    fn is_unique(&self) -> bool {
        self.is_unique()
    }

    fn item_id(&self) -> TypeId {
        TypeId::of::<T>()
    }
}

#[derive(Archive, Deserialize, Serialize)]

pub struct SyncFragView<T, A: Allocator + Default = Global> {
    #[with(ArcSwapArchiver)]
    pub(crate) inner: ArcSwapAny<ArcVec<T, A>>,
    pub(crate) thread: u8,
    pub(crate) len: AtomicUsize,
}

unsafe impl<T: Sync + Send, A: Allocator + Default + Sync + Send> Sync for SyncFragView<T, A> {}

impl<T, A: Allocator + Default> SyncFragView<T, A> {
    fn new_in(thread: u8, allocator: A) -> Self {
        Self {
            inner: ArcSwapAny::new(ArcVec(ArcVecInner::new(allocator))),
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
        A: Clone,
    {
        if values.len() == 0 {
            return (default(), false);
        }
        let len = self.load_len();
        let Ok(values_len) = values.len().try_into() else {
            crate::terminate!("SyncFragView:extend: too may values");
        };
        let (.., possibly_new) = unsafe { ArcVecInner::extend(self.inner.load().0, values) };
        if let Some(new) = possibly_new {
            self.inner.store(ArcVec(new));
        }
        let slice = FragSlice::new(FragSliceAddr::new(len as u32, self.thread, values_len));
        self.len.store(len + values_len as usize, Ordering::Relaxed);
        (slice, possibly_new.is_some())
    }

    fn unextend(&self, index: u32, len: u16) -> impl Iterator<Item = T> + '_ {
        self.len.store(
            self.len.load(Ordering::Relaxed) - len as usize,
            Ordering::Relaxed,
        );
        unsafe { ArcVecInner::unextend(self.inner.load().0, index, len) }
    }
}

pub(crate) struct LocalFragView<T, A: Allocator + Default = Global> {
    pub(crate) inner: ArcVec<T, A>,
    pub(crate) len: usize,
}

impl<T, A: Allocator + Default> LocalFragView<T, A> {
    pub fn get(&self, index: usize) -> Option<&T> {
        unsafe { ArcVecInner::data(self.inner.0, self.len).get(index) }
    }

    fn slice(&self, range: Range<usize>) -> Option<&[T]> {
        unsafe { ArcVecInner::data(self.inner.0, self.len).get(range) }
    }
}

pub struct FragSliceKey<T, A: Allocator + Default = Global> {
    view: *const SyncFragView<T, A>,
    cached_cap: AtomicUsize,
    cached: AtomicPtr<T>,
    slice: FragSlice<T>,
}

unsafe impl<T: Sync + Send, A: Allocator + Default + Sync + Send> Send for FragSliceKey<T, A> {}
unsafe impl<T: Sync + Send, A: Allocator + Default + Sync + Send> Sync for FragSliceKey<T, A> {}

impl<T, A: Allocator + Default> FragSliceKey<T, A> {
    /// # Safety
    /// Caller must ensure that `Self` does not outlive `map` and `slice` is valid
    pub unsafe fn new(map: &SyncFragMap<T, A>, slice: FragSlice<T>) -> Self {
        let map = &map.base.views[slice.0.thread as usize];
        Self {
            view: map as _,
            cached_cap: 0.into(),
            cached: default(),
            slice,
        }
    }

    /// # Safety
    /// Caller must ensure that `Self` does not outlive `map` and `slice` is valid
    pub unsafe fn from_base(map: &SyncFragBase<T, A>, slice: FragSlice<T>) -> Self {
        let map = &map.views[slice.0.thread as usize];

        Self {
            view: map as _,
            cached_cap: 0.into(),
            cached: default(),
            slice,
        }
    }

    unsafe fn inner_slice(&self) -> &[T] {
        let map = &*self.view;
        let cap = self.cached_cap.load(Ordering::Relaxed);
        if map.len.load(Ordering::Relaxed) <= cap {
            let FragSliceAddr { len, .. } = self.slice.addr();
            slice::from_raw_parts(self.cached.load(Ordering::Relaxed), len as usize)
        } else {
            self.reload()
        }
    }

    #[cold]
    #[inline(never)]
    unsafe fn reload(&self) -> &[T] {
        let map = &*self.view;
        let loaded = map.inner.load();
        let cap = ptr::addr_of!((*loaded.0.as_ptr()).cap).read();
        let FragSliceAddr { index, len, .. } = self.slice.addr();
        self.cached_cap.store(cap, Ordering::Relaxed);
        let ptr = ArcVecInner::get_item(loaded.0, index as usize);
        self.cached.store(ptr, Ordering::Relaxed);
        slice::from_raw_parts(ptr, len as usize)
    }
}

impl<T: Hash, A: Allocator + Default> Hash for FragSliceKey<T, A> {
    fn hash<H: ~const std::hash::Hasher>(&self, state: &mut H) {
        unsafe {
            self.inner_slice().hash(state);
        }
    }
}

impl<T: Eq, A: Allocator + Default> PartialEq for FragSliceKey<T, A> {
    fn eq(&self, other: &Self) -> bool {
        unsafe { self.inner_slice() == other.inner_slice() }
    }
}

impl<T: Eq, A: Allocator + Default> Eq for FragSliceKey<T, A> {}

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

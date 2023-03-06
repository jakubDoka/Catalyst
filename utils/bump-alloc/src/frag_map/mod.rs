use std::{
    alloc::{Allocator, Global, Layout},
    borrow::Borrow,
    cell::UnsafeCell,
    hash::{BuildHasher, Hash},
    iter, mem,
    ops::{Deref, DerefMut, Index, IndexMut, Range},
    process::abort,
    ptr::{self, NonNull},
    slice::{self, SliceIndex},
    sync::atomic::{self, AtomicUsize},
};

use crate::{FragAddr, FragRef, FragSlice, FragSliceAddr};
use arc_swap::{strategy::Strategy, ArcSwapAny, RefCnt};
use dashmap::{mapref::multiple::RefMulti, DashMap};
use rkyv::{
    out_field,
    ser::{ScratchSpace, Serializer},
    vec::{ArchivedVec, VecResolver},
    with::{ArchiveWith, DeserializeWith, SerializeWith},
    Archive, Archived, Deserialize, Fallible, Resolver, Serialize,
};
use smallvec::SmallVec;

pub mod addr;
pub mod interner;
// pub mod objects;
pub mod relocator;
pub mod shadow;
pub mod sync;

#[derive(Archive, Serialize, Deserialize)]
pub struct Cluster<T> {
    allocs: ArcVec<T>,
}

impl<T> Cluster<T> {
    pub fn new(thread_count: u8) -> Self
    where
        T: Default,
    {
        let new = ArcVecInner::with_capacity(thread_count as usize);
        unsafe {
            ArcVecInner::extend(new, (0..thread_count).map(|_| T::default()));
        }
        Self {
            allocs: ArcVec(new),
        }
    }

    pub fn split(&self) -> impl Iterator<Item = ClusterBorrow<T>> + '_ {
        unsafe {
            assert!(ArcVecInner::is_unique(self.allocs.0));
            ArcVecInner::full_data_mut(self.allocs.0)
                .iter_mut()
                .map(|borrowed| ClusterBorrow {
                    borrowed,
                    backing: self.allocs.clone(),
                })
        }
    }

    pub fn expand(&mut self, thread_count: u8)
    where
        T: Default,
    {
        assert!(self.allocs.is_unique());
        self.allocs.extend(
            (unsafe { crate::field!(self.allocs.0 => len) as u8 }..thread_count)
                .map(|_| T::default()),
        );
    }

    pub fn thread_count(&self) -> u8 {
        unsafe { crate::field!(self.allocs.0 => len) as u8 }
    }
}

unsafe impl<T: Send> Send for Cluster<T> {}
unsafe impl<T: Sync> Sync for Cluster<T> {}

pub struct ClusterBorrow<T> {
    borrowed: *mut T,
    backing: ArcVec<T>,
}

impl<T> ClusterBorrow<T> {
    pub fn thread(&self) -> u8 {
        let data_ptr = unsafe { crate::field!(self.backing.0 => ref data) };
        ((self.borrowed as usize - data_ptr as usize) / std::mem::size_of::<T>()) as u8
    }
}

unsafe impl<T: Send> Send for ClusterBorrow<T> {}
unsafe impl<T: Sync> Sync for ClusterBorrow<T> {}

impl<T> Deref for ClusterBorrow<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.borrowed }
    }
}

impl<T> DerefMut for ClusterBorrow<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.borrowed }
    }
}

/// # Safety
/// Trait should be only used and never derived.
pub unsafe auto trait NoInteriorMutability {}

impl<T: ?Sized> !NoInteriorMutability for UnsafeCell<T> {}

pub struct FragMap<T> {
    others: Box<[FragVec<T>]>,
    thread_local: FragVec<T>,
}

impl<T> FragMap<T> {
    fn new_in(others: Box<[FragVec<T>]>, thread: usize) -> Self {
        let thread_local = others[thread].clone();
        Self {
            others,
            thread_local,
        }
    }

    pub fn push(&mut self, value: T) -> FragRef<T> {
        let (id, reallocated) = self.thread_local.push(value);
        self.handle_reallocation(reallocated);
        id
    }

    pub fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) -> FragSlice<T>
    where
        I::IntoIter: ExactSizeIterator,
    {
        let (addr, reallocated) = self.thread_local.extend(iter.into_iter());
        self.handle_reallocation(reallocated);
        addr
    }

    pub fn commit(&mut self, base: &mut FragBase<T>) {
        self.thread_local.freeze();
        base.threads[self.thread_local.thread as usize] = self.thread_local.clone();
    }

    pub fn commit_unique(self, base: &mut FragBase<T>) {
        let thread = self.thread_local.thread as usize;
        base.threads[thread] = self.thread_local;
    }

    pub fn pull(&mut self, base: &FragBase<T>) {
        self.others.clone_from_slice(&base.threads);
    }

    fn handle_reallocation(&mut self, reallocated: bool) {
        if !reallocated {
            return;
        }

        self.others[self.thread_local.thread as usize] = self.thread_local.clone();
    }

    pub fn indexed(
        &self,
        slice: FragSlice<T>,
    ) -> impl Iterator<Item = (FragRef<T>, &T)> + ExactSizeIterator + DoubleEndedIterator
    where
        T: NoInteriorMutability,
    {
        slice.keys().zip(self[slice].iter())
    }

    pub fn next(&self) -> FragRef<T> {
        self.thread_local.next()
    }

    /// # Safety
    /// The caller must ensure reference is in range of valid memory.
    pub unsafe fn get_unchecked(&self, index: FragRef<T>) -> &T {
        let FragAddr { index, thread, .. } = index.0;
        let thread = &self.others.get_unchecked(thread as usize);
        &*ArcVecInner::get_item(thread.inner.0, index as usize)
    }

    /// # Safety
    /// Ensure slice is in range of valid memory.
    pub unsafe fn gen_unchecked_slice(&self, slice: FragSlice<T>) -> &[T] {
        let FragSliceAddr { index, thread, len } = slice.0;
        let thread = &self.others.get_unchecked(thread as usize);
        let ptr = ArcVecInner::get_item(thread.inner.0, index as usize);
        slice::from_raw_parts(ptr, len as usize)
    }

    /// # Safety
    /// Ensure slice is not out of bounds and does not capture frozen element.
    pub unsafe fn get_unchacked_mut(&mut self, index: FragRef<T>) -> &mut T {
        let FragAddr { index, .. } = index.0;
        &mut *ArcVecInner::get_item(self.thread_local.inner.0, index as usize)
    }

    /// # Safety
    /// Ensure slice is not out of bounds and does not capture frozen elements.
    pub unsafe fn gen_unchecked_slice_mut(&mut self, slice: FragSlice<T>) -> &mut [T] {
        let FragSliceAddr { index, len, .. } = slice.0;
        let ptr = ArcVecInner::get_item(self.thread_local.inner.0, index as usize);
        slice::from_raw_parts_mut(ptr, len as usize)
    }
}

impl<T: NoInteriorMutability> Index<FragRef<T>> for FragMap<T> {
    type Output = T;

    fn index(&self, index: FragRef<T>) -> &Self::Output {
        // unsafe { self.get_unchecked(index) }
        let FragAddr { index, thread, .. } = index.0;
        if thread == self.thread_local.thread {
            self.thread_local.thread_local_get(index as usize)
        } else {
            &self.others[thread as usize][index as usize]
        }
    }
}

impl<T: NoInteriorMutability> IndexMut<FragRef<T>> for FragMap<T> {
    fn index_mut(&mut self, index: FragRef<T>) -> &mut Self::Output {
        // unsafe { self.get_unchacked_mut(index) }
        let FragAddr { index, thread, .. } = index.0;
        assert!(self.thread_local.thread == thread);
        let index = (index as usize)
            .checked_sub(self.thread_local.frozen)
            .expect("accessing frozen elements mutably");
        &mut self.thread_local[index]
    }
}

impl<T: NoInteriorMutability> Index<FragSlice<T>> for FragMap<T> {
    type Output = [T];

    fn index(&self, index: FragSlice<T>) -> &Self::Output {
        // unsafe { self.gen_unchecked_slice(index) }
        let FragSliceAddr { index, thread, len } = index.0;
        let range = index as usize..index as usize + len as usize;
        if thread == self.thread_local.thread {
            self.thread_local.thread_local_get(range)
        } else {
            &self.others[thread as usize][range]
        }
    }
}

impl<T: NoInteriorMutability> IndexMut<FragSlice<T>> for FragMap<T> {
    fn index_mut(&mut self, index: FragSlice<T>) -> &mut Self::Output {
        // unsafe { self.gen_unchecked_slice_mut(index) }
        let FragSliceAddr { index, thread, len } = index.0;
        assert!(self.thread_local.thread == thread);
        let index = (index as usize)
            .checked_sub(self.thread_local.frozen)
            .expect("accessing frozen elements mutably");
        &mut self.thread_local[index..index + len as usize]
    }
}

#[derive(Archive, Deserialize, Serialize)]
pub struct FragBase<T> {
    threads: Box<[FragVec<T>]>,
}

impl<T> FragBase<T> {
    pub fn new(thread_count: u8) -> Self {
        let threads = (0..thread_count)
            .map(|thread| FragVec::new(thread))
            .collect::<Box<[_]>>();

        Self { threads }
    }

    pub fn expand(&mut self, thread_count: u8) {
        let threads = (self.threads.len() as u8..thread_count).map(|thread| FragVec::new(thread));
        let current = mem::take(&mut self.threads);
        self.threads = current.into_vec().into_iter().chain(threads).collect();
    }

    unsafe fn slice_unique(&self, addr: FragSliceAddr) -> &[T] {
        let FragSliceAddr { index, thread, len } = addr;
        let thread = &self.threads[thread as usize];
        &ArcVecInner::full_data(thread.inner.0)[index as usize..index as usize + len as usize]
    }

    pub fn split(&mut self) -> impl Iterator<Item = FragMap<T>> + '_ {
        assert!(self.is_unique());
        (0..self.threads.len()).map(|thread| FragMap::new_in(self.threads.clone(), thread))
    }

    pub fn is_unique(&mut self) -> bool {
        self.threads
            .iter()
            .all(|t| unsafe { ArcVecInner::is_unique(t.inner.0) })
    }

    // unsafe fn index_unique(&self, index: FragAddr) -> &T {
    //     let FragAddr { index, thread, .. } = index;
    //     let thread = &self.threads[thread as usize];
    //     &ArcVecInner::full_data(thread.inner.0)[index as usize]
    // }
}

impl<T: NoInteriorMutability> Index<FragRef<T>> for FragBase<T> {
    type Output = T;

    fn index(&self, index: FragRef<T>) -> &Self::Output {
        let FragAddr { index, thread, .. } = index.0;
        &self.threads[thread as usize][index as usize]
    }
}

impl<T: NoInteriorMutability> Index<FragSlice<T>> for FragBase<T> {
    type Output = [T];

    fn index(&self, index: FragSlice<T>) -> &Self::Output {
        let FragSliceAddr { index, thread, len } = index.0;
        &self.threads[thread as usize][index as usize..index as usize + len as usize]
    }
}

#[macro_export]
macro_rules! terminate {
    ($($tt:tt)*) => {
        {
            eprintln!($($tt)*);
            abort()
        }
    };
}

#[derive(Archive, Deserialize, Serialize)]
pub struct FragVec<T> {
    inner: ArcVec<T>,
    thread: u8,
    frozen: usize,
    len: usize,
}

impl<T> Clone for FragVec<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            thread: self.thread,
            frozen: self.frozen,
            len: self.len,
        }
    }
}

unsafe impl<T: Send + Sync> Send for FragVec<T> {}
unsafe impl<T: Send + Sync> Sync for FragVec<T> {}

impl<T> FragVec<T> {
    fn new(thread: u8) -> Self {
        Self {
            inner: ArcVec(ArcVecInner::new()),
            thread,
            frozen: 0,
            len: 0,
        }
    }

    fn unique_data(&mut self) -> &mut [T] {
        unsafe {
            assert!(ArcVecInner::is_unique(self.inner.0));
            ArcVecInner::full_data_mut(self.inner.0)
        }
    }

    fn push(&mut self, value: T) -> (FragRef<T>, bool) {
        let (slice, reallocated) = self.extend(iter::once(value));
        let FragSliceAddr { index, thread, .. } = slice.0;
        (FragRef::new(FragAddr::new(index, thread)), reallocated)
    }

    fn extend<I: ExactSizeIterator<Item = T>>(&mut self, values: I) -> (FragSlice<T>, bool) {
        let Ok(values_len) = values.len().try_into() else {
            terminate!("FragVec::extend: iterator too long");
        };

        let (.., possibly_new) = unsafe { ArcVecInner::extend(self.inner.0, values) };

        let slice = FragSlice::new(FragSliceAddr::new(self.len as u32, self.thread, values_len));

        if let Some(inner) = possibly_new {
            self.inner = ArcVec(inner);
        }
        self.len += values_len as usize;

        (slice, possibly_new.is_some())
    }

    fn freeze(&mut self) {
        self.frozen = self.len;
    }

    fn next(&self) -> FragRef<T> {
        FragRef::new(FragAddr::new(self.len as u32, self.thread))
    }

    fn thread_local_get<I: SliceIndex<[T]>>(&self, index: I) -> &I::Output {
        unsafe { index.index(ArcVecInner::data(self.inner.0, self.len)) }
    }
}

impl<T, I: SliceIndex<[T]>> IndexMut<I> for FragVec<T> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        unsafe { index.index_mut(ArcVecInner::data_mut(self.inner.0, self.frozen..self.len)) }
    }
}

impl<T, I: SliceIndex<[T]>> Index<I> for FragVec<T> {
    type Output = I::Output;

    fn index(&self, index: I) -> &Self::Output {
        unsafe { index.index(ArcVecInner::data(self.inner.0, self.frozen)) }
    }
}

#[macro_export]
macro_rules! field {
    ($s:expr => $field:ident) => {
        *std::ptr::addr_of!((*$s.as_ptr()).$field)
    };

    ($s:expr => mut $field:ident) => {
        *std::ptr::addr_of_mut!((*$s.as_ptr()).$field)
    };

    ($s:expr => ref $field:ident) => {
        std::ptr::addr_of!((*$s.as_ptr()).$field)
    };

    ($s:expr => ref mut $field:ident) => {
        std::ptr::addr_of_mut!((*$s.as_ptr()).$field)
    };
}

#[repr(C)]
pub struct ArcVecInner<T> {
    ref_count: AtomicUsize,
    owner: Option<NonNull<ArcVecInner<T>>>,
    cap: usize,
    len: usize,
    data: [T; 0],
}

impl<T> ArcVecInner<T> {
    unsafe fn is_unique(s: NonNull<Self>) -> bool {
        field!(s => ref_count).load(atomic::Ordering::Relaxed) == 1
    }

    unsafe fn inc(s: NonNull<Self>) {
        field!(s => ref_count).fetch_add(1, atomic::Ordering::Relaxed);
    }

    unsafe fn dec(s: NonNull<Self>) {
        if field!(s => ref_count).fetch_sub(1, atomic::Ordering::Relaxed) == 1 {
            Self::drop(s);
        }
    }

    unsafe fn data<'a>(s: NonNull<Self>, len: usize) -> &'a [T] {
        slice::from_raw_parts(Self::get_item(s, 0), len)
    }

    unsafe fn data_mut<'a>(s: NonNull<Self>, range: Range<usize>) -> &'a mut [T] {
        slice::from_raw_parts_mut(Self::get_item(s, range.start), range.end - range.start)
    }

    unsafe fn get_item(s: NonNull<Self>, index: usize) -> *mut T {
        let data = field!(s => ref mut data).cast::<T>();
        data.add(index)
    }

    unsafe fn drop(s: NonNull<Self>) {
        let len = field!(s => len);
        if let Some(owner) = field!(s => owner) {
            Self::dec(owner);
        } else {
            Self::data_mut(s, 0..len)
                .iter_mut()
                .for_each(|x| ptr::drop_in_place(x));
        }

        let cap = field!(s => cap);
        let layout = Self::layout(cap);

        let allocator = Global;
        allocator.deallocate(s.cast(), layout);
    }

    unsafe fn extend<I: ExactSizeIterator<Item = T>>(
        mut s: NonNull<Self>,
        i: I,
    ) -> (*mut T, Option<NonNull<Self>>) {
        let pushed_len = i.len();
        let cap = field!(s => cap);
        let len = field!(s => len);

        let overflows = len + pushed_len > cap;
        if overflows {
            s = Self::reallocate(s, len, pushed_len);
        }

        let start = Self::get_item(s, len);
        for (i, item) in i.enumerate() {
            ptr::write(start.add(i), item);
            field!(s => mut len) += 1; // in case iterator panicked
        }

        (start, overflows.then_some(s))
    }

    #[cold]
    #[inline(never)]
    unsafe fn reallocate(s: NonNull<Self>, len: usize, pushed_len: usize) -> NonNull<Self> {
        let cap = (len + pushed_len).next_power_of_two();
        let new_s = Self::with_capacity(cap);

        // note: by this we avoid cloning the contents, eventhough we
        // have two instances of the data it is immutable and only parrent will
        // call destructors
        ptr::copy_nonoverlapping(Self::get_item(s, 0), Self::get_item(new_s, 0), len);
        field!(s => mut owner) = Some(new_s);
        field!(new_s => mut ref_count) = AtomicUsize::new(2);
        field!(new_s => mut len) = len;
        new_s
    }

    fn with_capacity(cap: usize) -> NonNull<Self> {
        let layout = Self::layout(cap);
        let s = Global
            .allocate(layout)
            .unwrap_or_else(|err| terminate!("{}", err))
            .cast::<Self>();

        unsafe {
            s.as_ptr().write(Self {
                ref_count: AtomicUsize::new(1),
                owner: None,
                cap,
                len: 0,
                data: [],
            });
        }

        s
    }

    fn new() -> NonNull<Self> {
        let cap = ((1 << 10) / mem::size_of::<T>()).next_power_of_two();
        Self::with_capacity(cap)
    }

    fn layout(cap: usize) -> Layout {
        Layout::new::<Self>()
            .extend(Layout::array::<T>(cap).unwrap_or_else(|err| terminate!("{}", err)))
            .unwrap_or_else(|err| terminate!("{}", err))
            .0
    }

    unsafe fn full_data_mut<'a>(inner: NonNull<ArcVecInner<T>>) -> &'a mut [T] {
        let len = field!(inner => len);
        Self::data_mut(inner, 0..len)
    }

    pub(crate) unsafe fn unextend(
        load: NonNull<ArcVecInner<T>>,
        index: u32,
        len: u16,
    ) -> impl Iterator<Item = T> {
        assert_eq!(field!(load => len) - len as usize, index as usize);
        let base = Self::get_item(load, index as usize);
        field!(load => mut len) -= len as usize;

        struct Finalizer<I: Iterator>(I);

        impl<I: Iterator> Drop for Finalizer<I> {
            fn drop(&mut self) {
                self.0.by_ref().for_each(drop);
            }
        }

        impl<I: Iterator> Iterator for Finalizer<I> {
            type Item = I::Item;

            fn next(&mut self) -> Option<Self::Item> {
                self.0.next()
            }
        }

        Finalizer((0..len as usize).map(move |i| base.add(i).read()))
    }

    unsafe fn full_data<'a>(inner: NonNull<ArcVecInner<T>>) -> &'a [T] {
        let len = field!(inner => len);
        Self::data(inner, len)
    }
}

#[repr(transparent)]
pub struct ArcVec<T>(NonNull<ArcVecInner<T>>);

impl<T> ArcVec<T> {
    pub fn new() -> Self {
        ArcVec(ArcVecInner::new())
    }

    pub fn is_unique(&self) -> bool {
        unsafe { ArcVecInner::is_unique(self.0) }
    }

    pub fn extend<I: ExactSizeIterator<Item = T>>(&mut self, i: I) {
        unsafe {
            let (.., overflows) = ArcVecInner::extend(self.0, i);
            if let Some(overflows) = overflows {
                *self = ArcVec(overflows);
            }
        };
    }
}

impl<T> Default for ArcVec<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[repr(transparent)]
pub struct ArchivedArcVec<T: Archive> {
    data: ArchivedVec<T::Archived>,
}

impl<T> Archive for ArcVec<T>
where
    T: Archive,
{
    type Archived = ArchivedArcVec<T>;

    type Resolver = VecResolver;

    unsafe fn resolve(&self, pos: usize, resolver: Self::Resolver, out: *mut Self::Archived) {
        ArchivedVec::resolve_from_slice(
            ArcVecInner::full_data(self.0),
            pos,
            resolver,
            out as *mut _,
        );
    }
}

impl<T, S> Serialize<S> for ArcVec<T>
where
    T: Archive + Serialize<S>,
    S: ScratchSpace + ?Sized + Serializer,
{
    fn serialize(&self, serializer: &mut S) -> Result<Self::Resolver, <S as Fallible>::Error> {
        ArchivedVec::<T::Archived>::serialize_from_slice(
            unsafe { ArcVecInner::full_data(self.0) },
            serializer,
        )
    }
}

impl<T, D> Deserialize<ArcVec<T>, D> for ArchivedArcVec<T>
where
    T::Archived: Deserialize<T, D>,
    D: Fallible + ?Sized,
    T: Archive + Sized,
{
    fn deserialize(&self, deserializer: &mut D) -> Result<ArcVec<T>, <D as Fallible>::Error> {
        let new = ArcVec(ArcVecInner::<T>::with_capacity(self.data.len()));
        let base = unsafe { ArcVecInner::get_item(new.0, 0) };
        for (i, item) in self.data.iter().enumerate() {
            let des = item.deserialize(deserializer)?;
            unsafe {
                base.add(i).write(des);
                *ptr::addr_of_mut!((*new.0.as_ptr()).len) += 1;
            }
        }

        Ok(new)
    }
}

pub struct ArcSwapArchiver;

impl<T, S> ArchiveWith<ArcSwapAny<T, S>> for ArcSwapArchiver
where
    T: RefCnt + Archive,
    S: Strategy<T>,
{
    type Archived = Archived<T>;

    type Resolver = Resolver<T>;

    unsafe fn resolve_with(
        field: &ArcSwapAny<T, S>,
        pos: usize,
        resolver: Self::Resolver,
        out: *mut Self::Archived,
    ) {
        let arc = field.load_full();
        arc.resolve(pos, resolver, out)
    }
}

impl<T, ST, S> SerializeWith<ArcSwapAny<T, ST>, S> for ArcSwapArchiver
where
    T: RefCnt + Serialize<S>,
    ST: Strategy<T>,
    S: ScratchSpace + Serializer + ?Sized,
{
    fn serialize_with(
        field: &ArcSwapAny<T, ST>,
        serializer: &mut S,
    ) -> Result<Self::Resolver, <S as Fallible>::Error> {
        field.load_full().serialize(serializer)
    }
}

impl<T, S, D> DeserializeWith<Archived<T>, ArcSwapAny<T, S>, D> for ArcSwapArchiver
where
    T::Archived: Deserialize<T, D>,
    T: RefCnt + Archive,
    S: Strategy<T> + Default,
    D: Fallible + ?Sized,
{
    fn deserialize_with(
        field: &Archived<T>,
        deserializer: &mut D,
    ) -> Result<ArcSwapAny<T, S>, <D as Fallible>::Error> {
        field.deserialize(deserializer).map(|s| ArcSwapAny::new(s))
    }
}

pub struct DashMapArchiver;

impl<K, V, H> ArchiveWith<DashMap<K, V, H>> for DashMapArchiver
where
    K::Archived: Eq + Hash,
    K: Archive + Eq + Hash,
    V: Archive,
    H: BuildHasher + Clone,
{
    type Archived = (ArchivedVec<K::Archived>, ArchivedVec<V::Archived>);

    type Resolver = (VecResolver, VecResolver);

    unsafe fn resolve_with(
        field: &DashMap<K, V, H>,
        pos: usize,
        resolver: Self::Resolver,
        out: *mut Self::Archived,
    ) {
        let (off_a, a) = out_field!(out.0);
        ArchivedVec::resolve_from_len(field.len(), pos + off_a, resolver.0, a);
        let (off_a, a) = out_field!(out.1);
        ArchivedVec::resolve_from_len(field.len(), pos + off_a, resolver.1, a);
    }
}

impl<K, V, S, H> SerializeWith<DashMap<K, V, H>, S> for DashMapArchiver
where
    K::Archived: Eq + Hash,
    K: Archive + Eq + Hash + Serialize<S>,
    V: Archive + Serialize<S>,
    S: ScratchSpace + Serializer + ?Sized,
    H: BuildHasher + Clone,
{
    fn serialize_with(
        field: &DashMap<K, V, H>,
        serializer: &mut S,
    ) -> Result<Self::Resolver, <S as Fallible>::Error> {
        struct CustomSizeHint<I>(I, usize);

        impl<I: Iterator> Iterator for CustomSizeHint<I> {
            type Item = I::Item;

            fn next(&mut self) -> Option<Self::Item> {
                self.0.next()
            }
        }

        impl<I: Iterator> ExactSizeIterator for CustomSizeHint<I> {
            fn len(&self) -> usize {
                self.1
            }
        }

        struct KeyBorrow<'a, K, V, S>(RefMulti<'a, K, V, S>);

        impl<'a, K: Hash + Eq, V, S: BuildHasher> Borrow<K> for KeyBorrow<'a, K, V, S> {
            fn borrow(&self) -> &K {
                self.0.key()
            }
        }
        struct ValueBorrow<'a, K, V, S>(RefMulti<'a, K, V, S>);

        impl<'a, K: Hash + Eq, V, S: BuildHasher> Borrow<V> for ValueBorrow<'a, K, V, S> {
            fn borrow(&self) -> &V {
                self.0.value()
            }
        }

        Ok((
            ArchivedVec::serialize_from_iter::<K, _, _, _>(
                CustomSizeHint(field.iter().map(|entry| KeyBorrow(entry)), field.len()),
                serializer,
            )?,
            ArchivedVec::serialize_from_iter::<V, _, _, _>(
                CustomSizeHint(field.iter().map(|entry| ValueBorrow(entry)), field.len()),
                serializer,
            )?,
        ))
    }
}

impl<K, V, D, H>
    DeserializeWith<(ArchivedVec<K::Archived>, ArchivedVec<V::Archived>), DashMap<K, V, H>, D>
    for DashMapArchiver
where
    K::Archived: Eq + Hash + Deserialize<K, D>,
    K: Archive + Eq + Hash,
    V::Archived: Deserialize<V, D>,
    V: Archive,
    D: Fallible + ?Sized,
    H: BuildHasher + Default + Clone,
{
    fn deserialize_with(
        field: &(ArchivedVec<K::Archived>, ArchivedVec<V::Archived>),
        deserializer: &mut D,
    ) -> Result<DashMap<K, V, H>, <D as Fallible>::Error> {
        let map = DashMap::<K, V, H>::with_capacity_and_hasher(field.0.len(), H::default());
        for (k, v) in field.0.iter().zip(field.1.iter()) {
            map.insert(k.deserialize(deserializer)?, v.deserialize(deserializer)?);
        }

        Ok(map)
    }
}

pub struct SmallVecArchiver;

impl<T: Archive + Clone> ArchiveWith<SmallVec<[T; 4]>> for SmallVecArchiver {
    type Archived = Archived<Vec<T>>;

    type Resolver = Resolver<Vec<T>>;

    unsafe fn resolve_with(
        field: &SmallVec<[T; 4]>,
        pos: usize,
        resolver: Self::Resolver,
        out: *mut Self::Archived,
    ) {
        field.to_vec().resolve(pos, resolver, out)
    }
}

impl<T, S> SerializeWith<SmallVec<[T; 4]>, S> for SmallVecArchiver
where
    T: Archive + Clone + Serialize<S>,
    S: ScratchSpace + Serializer + ?Sized,
{
    fn serialize_with(
        field: &SmallVec<[T; 4]>,
        serializer: &mut S,
    ) -> Result<Self::Resolver, <S as Fallible>::Error> {
        field.to_vec().serialize(serializer)
    }
}

impl<T, D> DeserializeWith<Archived<Vec<T>>, SmallVec<[T; 4]>, D> for SmallVecArchiver
where
    <Vec<T> as Archive>::Archived: Deserialize<Vec<T>, D>,
    T: Archive,
    D: Fallible + ?Sized,
{
    fn deserialize_with(
        field: &Archived<Vec<T>>,
        deserializer: &mut D,
    ) -> Result<SmallVec<[T; 4]>, <D as Fallible>::Error> {
        field.deserialize(deserializer).map(|v| v.into())
    }
}

// unsafe impl<T: Sync + Send + Sync + Send> Sync for FragVecArc<T> {}
unsafe impl<T: Sync + Send + Sync + Send> Send for ArcVec<T> {}

impl<T> Clone for ArcVec<T> {
    fn clone(&self) -> Self {
        unsafe {
            ArcVecInner::inc(self.0);
        }

        Self(self.0)
    }
}

impl<T> Drop for ArcVec<T> {
    fn drop(&mut self) {
        unsafe {
            ArcVecInner::dec(self.0);
        }
    }
}

unsafe impl<T> RefCnt for ArcVec<T> {
    type Base = ArcVecInner<T>;

    fn into_ptr(me: Self) -> *mut Self::Base {
        let ptr = me.0.as_ptr();
        mem::forget(me);
        ptr
    }

    fn as_ptr(me: &Self) -> *mut Self::Base {
        me.0.as_ptr()
    }

    unsafe fn from_ptr(ptr: *const Self::Base) -> Self {
        Self(NonNull::new_unchecked(ptr as _))
    }
}

#[cfg(test)]
mod tests {
    use std::{sync::mpsc, thread};

    use super::*;

    #[test]
    fn test() {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);

        struct Dropped(usize);

        impl Drop for Dropped {
            fn drop(&mut self) {
                COUNTER.fetch_sub(1, atomic::Ordering::Relaxed);
            }
        }

        impl Dropped {
            fn new(i: usize) -> Self {
                COUNTER.fetch_add(1, atomic::Ordering::Relaxed);
                Self(i)
            }
        }

        impl Clone for Dropped {
            fn clone(&self) -> Self {
                Self::new(self.0)
            }
        }

        let mut base = FragBase::<Dropped>::new(4);
        let maps = base.split();
        let (sender, receiver) = mpsc::channel();

        let threads = maps
            .into_iter()
            .enumerate()
            .map(|(i, map)| {
                let (thread_sender, thread_receiver) = mpsc::channel();
                thread_sender
                    .send((map, Option::<FragSlice<Dropped>>::None))
                    .unwrap();
                let sender = sender.clone();
                (
                    thread::spawn(move || {
                        for _ in 0..10 {
                            let (mut map, slice) = thread_receiver.recv().unwrap();
                            let slice = if let Some(slice) = slice {
                                let vec = map[slice].to_owned();
                                map.extend(vec.into_iter())
                            } else {
                                map.extend((0..10).map(Dropped::new))
                            };
                            sender.send((map, i, slice)).unwrap();
                        }
                    }),
                    thread_sender,
                )
            })
            .collect::<Vec<_>>();

        let mut slices = vec![];
        for (mut map, i, slice) in receiver.iter().take(threads.len() * 10) {
            map.commit(&mut base);
            map.pull(&base);
            slices.push(slice);
            let _ = threads[i].1.send((map, slices.pop()));
        }

        drop(base);

        assert_eq!(COUNTER.load(atomic::Ordering::Relaxed), 0);
    }
}

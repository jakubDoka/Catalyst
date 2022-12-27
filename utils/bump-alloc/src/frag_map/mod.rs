/*
    Lets say we have separate allocation for each thread.
    Thread can reallocate everything, but it allocation is ref-counted.
    At each control join will thread update its local view and take views
    of other threads. The storage items must not implement drop.
*/

use std::{
    alloc::{Allocator, Global, Layout},
    iter, mem,
    ops::{Index, IndexMut, Range},
    process::abort,
    ptr::{self, NonNull},
    slice::{self, SliceIndex},
    sync::atomic::{self, AtomicUsize},
};

use crate::{FragAddr, FragRef, FragSlice, FragSliceAddr};

pub mod addr;

pub struct FragMap<T, A: Allocator = Global> {
    others: Box<[FragVecView<T, A>]>,
    thread_local: FragVec<T, A>,
}

impl<T, A: Allocator> FragMap<T, A> {
    fn new_in(others: Box<[FragVecView<T, A>]>, thread: usize) -> Self
    where
        A: Clone,
    {
        let thread_local = FragVec::new(others[thread].clone());
        Self {
            others,
            thread_local,
        }
    }

    pub fn push(&mut self, value: T) -> FragRef<T>
    where
        A: Clone,
    {
        let (id, reallocated) = self.thread_local.push(value);
        self.handle_reallocation(reallocated);
        id
    }

    pub fn extend<I: ExactSizeIterator<Item = T>>(&mut self, iter: I) -> FragSlice<T>
    where
        A: Clone,
    {
        let (addr, reallocated) = self.thread_local.extend(iter);
        self.handle_reallocation(reallocated);
        addr
    }

    pub fn update(&mut self, base: &mut FragBase<T, A>) {
        self.thread_local.freeze();
        base.threads[self.thread_local.view.thread] = self.thread_local.view.clone();
        self.others.clone_from_slice(&base.threads);
    }

    fn handle_reallocation(&mut self, reallocated: bool) {
        if !reallocated {
            return;
        }

        self.others[self.thread_local.view.thread] = self.thread_local.view.clone();
    }
}

impl<T, A: Allocator> Index<FragRef<T>> for FragMap<T, A> {
    type Output = T;

    fn index(&self, index: FragRef<T>) -> &Self::Output {
        let (index, thread, ..) = index.0.parts();
        &self.others[thread as usize][index as usize]
    }
}

impl<T, A: Allocator> IndexMut<FragRef<T>> for FragMap<T, A> {
    fn index_mut(&mut self, index: FragRef<T>) -> &mut Self::Output {
        let (index, thread, ..) = index.0.parts();
        assert!(self.thread_local.view.thread == thread as usize);
        &mut self.thread_local[index as usize]
    }
}

impl<T, A: Allocator> Index<FragSlice<T>> for FragMap<T, A> {
    type Output = [T];

    fn index(&self, index: FragSlice<T>) -> &Self::Output {
        let (index, thread, len) = index.0.parts();
        &self.others[thread as usize][index as usize..index as usize + len as usize]
    }
}

impl<T, A: Allocator> IndexMut<FragSlice<T>> for FragMap<T, A> {
    fn index_mut(&mut self, index: FragSlice<T>) -> &mut Self::Output {
        let (index, thread, len) = index.0.parts();
        assert!(self.thread_local.view.thread == thread as usize);
        &mut self.thread_local[index as usize..index as usize + len as usize]
    }
}

pub struct FragBase<T, A: Allocator = Global> {
    threads: Box<[FragVecView<T, A>]>,
}

impl<T> FragBase<T> {
    pub fn new(thread_count: usize) -> (Self, Vec<FragMap<T>>) {
        Self::new_in(thread_count, Global)
    }
}

impl<T, A: Allocator> FragBase<T, A> {
    pub fn new_in(thread_count: usize, allocator: A) -> (Self, Vec<FragMap<T, A>>)
    where
        A: Clone,
    {
        let threads = (0..thread_count)
            .map(|thread| FragVecView::new_in(thread, allocator.clone()))
            .collect::<Box<[_]>>();

        let maps = (0..thread_count)
            .map(|thread| FragMap::new_in(threads.clone(), thread))
            .collect();

        (Self { threads }, maps)
    }
}

impl<T, A: Allocator> Index<FragRef<T>> for FragBase<T, A> {
    type Output = T;

    fn index(&self, index: FragRef<T>) -> &Self::Output {
        let (index, thread, ..) = index.0.parts();
        &self.threads[thread as usize][index as usize]
    }
}

impl<T, A: Allocator> Index<FragSlice<T>> for FragBase<T, A> {
    type Output = [T];

    fn index(&self, index: FragSlice<T>) -> &Self::Output {
        let (index, thread, len) = index.0.parts();
        &self.threads[thread as usize][index as usize..index as usize + len as usize]
    }
}

macro_rules! terminate {
    ($($tt:tt)*) => {
        {
            eprintln!($($tt)*);
            abort()
        }
    };
}

struct FragVec<T, A: Allocator = Global> {
    view: FragVecView<T, A>,
    len: usize,
}

impl<T, A: Allocator> FragVec<T, A> {
    fn new(view: FragVecView<T, A>) -> Self {
        Self { view, len: 0 }
    }

    fn push(&mut self, value: T) -> (FragRef<T>, bool)
    where
        A: Clone,
    {
        let (slice, reallocated) = self.extend(iter::once(value));
        let (index, thread, ..) = slice.0.parts();
        (FragRef::new(FragAddr::new(index, thread)), reallocated)
    }

    fn extend<I: ExactSizeIterator<Item = T>>(&mut self, values: I) -> (FragSlice<T>, bool)
    where
        A: Clone,
    {
        let values_len = values.len();
        let (.., possibly_new) = unsafe { FragVecInner::extend(self.view.inner, self.len, values) };

        let slice = FragSlice::new(FragSliceAddr::new(
            self.len as u64,
            self.view.thread as u8,
            values_len as u16,
        ));

        if let Some(inner) = possibly_new {
            self.view = FragVecView { inner, ..self.view };
        }
        self.len += values_len;

        (slice, possibly_new.is_some())
    }

    fn freeze(&mut self) {
        self.view.frozen = self.len;
    }
}

impl<T, A: Allocator, I: SliceIndex<[T]>> Index<I> for FragVec<T, A> {
    type Output = I::Output;

    fn index(&self, index: I) -> &Self::Output {
        &self.view[index]
    }
}

impl<T, A: Allocator, I: SliceIndex<[T]>> IndexMut<I> for FragVec<T, A> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        unsafe {
            index.index_mut(FragVecInner::data_mut(
                self.view.inner,
                self.view.frozen..self.len,
            ))
        }
    }
}

struct FragVecView<T, A: Allocator = Global> {
    inner: NonNull<FragVecInner<T, A>>,
    thread: usize,
    frozen: usize,
}

unsafe impl<T: Send + Sync, A: Send + Sync + Allocator> Send for FragVecView<T, A> {}
unsafe impl<T: Send + Sync, A: Send + Sync + Allocator> Sync for FragVecView<T, A> {}

impl<T, A: Allocator> FragVecView<T, A> {
    const GOOD_CAP: usize = ((1 << 10) / mem::size_of::<T>()).next_power_of_two();

    fn new_in(thread: usize, allocator: A) -> Self {
        Self {
            inner: FragVecInner::new(Self::GOOD_CAP, allocator),
            thread,
            frozen: 0,
        }
    }
}

impl<T, A: Allocator, I: SliceIndex<[T]>> Index<I> for FragVecView<T, A> {
    type Output = I::Output;

    fn index(&self, index: I) -> &Self::Output {
        unsafe { index.index(FragVecInner::data(self.inner, self.frozen)) }
    }
}

impl<T, A: Allocator> Clone for FragVecView<T, A> {
    fn clone(&self) -> Self {
        unsafe {
            FragVecInner::inc(self.inner);
        }
        Self {
            inner: self.inner,
            frozen: self.frozen,
            thread: self.thread,
        }
    }
}

impl<T, A: Allocator> Drop for FragVecView<T, A> {
    fn drop(&mut self) {
        unsafe {
            FragVecInner::dec(self.inner);
        }
    }
}

struct FragVecInner<T, A: Allocator = Global> {
    ref_count: AtomicUsize,
    cap: usize,
    allocator: A,
    data: [T; 0],
}

impl<T, A: Allocator> FragVecInner<T, A> {
    unsafe fn inc(s: NonNull<Self>) {
        (*ptr::addr_of!((*s.as_ptr()).ref_count)).fetch_add(1, atomic::Ordering::Relaxed);
    }

    unsafe fn dec(s: NonNull<Self>) {
        if (*ptr::addr_of!((*s.as_ptr()).ref_count)).fetch_sub(1, atomic::Ordering::Relaxed) == 1 {
            Self::drop(s);
        }
    }

    unsafe fn data<'a>(s: NonNull<Self>, len: usize) -> &'a [T] {
        slice::from_raw_parts(Self::get_item(s, 0).as_ptr(), len)
    }

    unsafe fn data_mut<'a>(s: NonNull<Self>, range: Range<usize>) -> &'a mut [T] {
        slice::from_raw_parts_mut(
            Self::get_item(s, 0).as_ptr().add(range.start),
            range.end - range.start,
        )
    }

    unsafe fn get_item(s: NonNull<Self>, index: usize) -> NonNull<T> {
        debug_assert!(index < ptr::addr_of!((*s.as_ptr()).cap).read());
        let data = ptr::addr_of_mut!((*s.as_ptr()).data);
        NonNull::new_unchecked(data.add(index).cast())
    }

    unsafe fn drop(s: NonNull<Self>) {
        let allocator = ptr::addr_of!((*s.as_ptr()).allocator).read();
        let layout = Self::layout(ptr::addr_of!((*s.as_ptr()).cap).read());
        allocator.deallocate(s.cast(), layout);
    }

    unsafe fn extend<I: ExactSizeIterator<Item = T>>(
        mut s: NonNull<Self>,
        len: usize,
        i: I,
    ) -> (NonNull<T>, Option<NonNull<Self>>)
    where
        A: Clone,
    {
        let pushed_len = i.len();
        let mut cap = ptr::addr_of!((*s.as_ptr()).cap).read();

        let overflows = len + pushed_len > cap;
        if overflows {
            cap = (len + pushed_len).next_power_of_two();
            let allocator = (*ptr::addr_of!((*s.as_ptr()).allocator)).clone();
            let new_s = Self::new(cap, allocator);

            const { assert!(!mem::needs_drop::<T>()) }
            ptr::copy_nonoverlapping(
                Self::get_item(s, 0).as_ptr(),
                Self::get_item(new_s, 0).as_ptr(),
                len,
            );
            s = new_s;
        }

        let start = Self::get_item(s, len);
        i.enumerate()
            .for_each(|(i, v)| ptr::write(start.as_ptr().add(i), v));

        (start, overflows.then_some(s))
    }

    fn new(cap: usize, allocator: A) -> NonNull<Self> {
        let layout = Self::layout(cap);
        let s = allocator
            .allocate(layout)
            .unwrap_or_else(|err| terminate!("{}", err))
            .cast::<Self>();

        unsafe {
            s.as_ptr().write(Self {
                ref_count: AtomicUsize::new(1),
                cap,
                allocator,
                data: [],
            });
        }

        s
    }

    fn layout(cap: usize) -> Layout {
        Layout::new::<Self>()
            .extend(Layout::array::<T>(cap).unwrap_or_else(|err| terminate!("{}", err)))
            .unwrap_or_else(|err| terminate!("{}", err))
            .0
    }
}

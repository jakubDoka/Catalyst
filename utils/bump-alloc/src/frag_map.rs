use core::slice;
use std::{
    default::default,
    hint::unreachable_unchecked,
    marker::PhantomData,
    mem::{self, MaybeUninit},
    ops::Index,
    ptr::{self, addr_of, addr_of_mut, NonNull},
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::*;

impl NonMaxU16 {
    pub const fn get(self) -> u16 {
        self.0
    }
}

pub const MAX_FRAGMENT_SIZE: usize = (1 << 16) - 2;
const OUT_OF_SPACE: &str = "Out of space.";

pub struct FragRelocator<T> {
    seen: Set<FragRef<T>>,
    sort_temp: Vec<FragRef<T>>,
    mapping: Map<FragRef<T>, FragRef<T>>,
}

impl<T> Default for FragRelocator<T> {
    fn default() -> Self {
        Self {
            seen: default(),
            sort_temp: default(),
            mapping: default(),
        }
    }
}

impl<T: Clone> FragRelocator<T> {
    pub fn relocate<const SIZE: usize>(&mut self, map: &mut FragMap<T, SIZE>) {
        let mut allocator = Allocator::<SIZE>::default();

        self.sort_temp.clear();
        self.sort_temp.extend(self.seen.drain());
        self.sort_temp.sort();
        self.mapping.reserve(self.seen.len());

        let mut iter = self.sort_temp.iter().copied().peekable();
        let just_the_right_cap = 32;
        let mut buffer = bumpvec![cap just_the_right_cap];
        while let Some(key) = iter.next() {
            buffer.push(key);
            while let Some(&key_peek) = iter.peek()
                && key_peek.right_after(key)
            {
                buffer.push(key_peek);
                iter.next();
            }
            let slice = allocator
                .alloc(buffer.len() as u16, map)
                .expect(OUT_OF_SPACE);
            let slice_ref = FragSlice(slice, PhantomData);
            unsafe {
                let new_key = FragRef(slice_ref.0.addr, PhantomData);
                if key != new_key {
                    ptr::copy(map.ptr_to(key), map.ptr_to(new_key), buffer.len());
                }
            }
            buffer
                .drain(..)
                .zip(slice_ref.keys())
                .collect_into(&mut self.mapping);
        }

        let taken_blocks = allocator.taken_blocks();

        unsafe {
            map.set_len_and_drop_overflow(taken_blocks);
            let global = NonMaxU16(taken_blocks as u16 - 1);
            let last = map.base.inner.get_mut(global, false);
            last.set_len_and_drop_overflow(allocator.local as usize);
            map.current = last.clone();
            map.global = global;
        }

        map.base.inner.iter_mut().for_each(|x| x.freeze());
    }

    pub fn project(&self, entry: FragRef<T>) -> Option<FragRef<T>> {
        self.mapping.get(&entry).copied()
    }

    pub fn project_slice(&self, entry: FragSlice<T>) -> FragSlice<T> {
        let mut iter = entry.keys().filter_map(|key| self.project(key));

        let Some(first) = iter.next() else {
            return default();
        };

        let len = iter.count() + 1;
        let addr = FragSliceAddr {
            addr: first.0,
            len: unsafe { NonMaxU16(len as u16) },
        };
        FragSlice(addr, PhantomData)
    }

    pub fn mark(&mut self, entry: FragRef<T>) -> Option<()> {
        self.seen.insert(entry).then_some(())
    }

    pub fn mark_slice(&mut self, entry: FragSlice<T>) -> impl Iterator<Item = Option<()>> + '_ {
        entry.keys().map(move |key| self.mark(key))
    }

    pub fn mark_slice_summed(&mut self, entry: FragSlice<T>) -> Option<()> {
        self.mark_slice(entry).fold(None, |acc, x| acc.or(x))
    }

    pub fn clear(&mut self) {
        self.mapping.clear();
        self.seen.clear();
    }
}

#[derive(Default)]
struct Allocator<const SIZE: usize> {
    global: u16,
    local: u16,
}

impl<const SIZE: usize> Allocator<SIZE> {
    pub fn alloc<T>(&mut self, size: u16, map: &mut FragMap<T, SIZE>) -> Option<FragSliceAddr> {
        if self.local > SIZE as u16 - size {
            if size > SIZE as u16 {
                return None;
            }

            if self.global == MAX_FRAGMENT_SIZE as u16 {
                return None;
            }

            unsafe {
                map.base
                    .inner
                    .get_mut(NonMaxU16(self.global), false)
                    .set_len_and_drop_overflow(self.local as usize);
            }

            self.local = size;
            self.global += 1;
        } else {
            self.local += size;
        }

        Some(unsafe { FragSliceAddr::new(self.global, self.local - size, size) })
    }

    pub fn taken_blocks(&self) -> usize {
        (self.global as usize + (self.local != 0) as usize).max(1)
    }
}

pub struct FragBase<T, const SIZE: usize> {
    inner: Fragment<Fragment<T, SIZE>, MAX_FRAGMENT_SIZE>,
}

impl<T, const SIZE: usize> FragBase<T, SIZE> {
    pub fn new() -> Self {
        Self {
            inner: Fragment::new(),
        }
    }

    pub fn as_map(&self) -> FragMap<T, SIZE> {
        let current = Fragment::new();
        let mut base = self.clone();
        let global = base.inner.push(current.clone()).ok().expect(OUT_OF_SPACE);

        FragMap {
            base,
            current,
            global,
        }
    }
}

impl<T, const SIZE: usize> Default for FragBase<T, SIZE> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T, const SIZE: usize> Clone for FragBase<T, SIZE> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

pub struct FragMap<T, const SIZE: usize> {
    base: FragBase<T, SIZE>,
    current: Fragment<T, SIZE>,
    global: NonMaxU16,
}

impl<T, const SIZE: usize> FragMap<T, SIZE> {
    pub fn new() -> Self {
        FragBase::new().as_map()
    }

    pub fn split(&self) -> Self {
        self.base.as_map()
    }

    pub fn freeze(&mut self) {
        self.current.freeze();
        self.base.inner.freeze();
    }

    /// # Safety
    /// The returned reference is only valid after subsequent push or non empty extend.
    pub unsafe fn next(&mut self) -> FragRef<T> {
        match self.current.next() {
            Some(id) => FragRef(
                FragAddr {
                    global: self.global,
                    local: id,
                },
                PhantomData,
            ),
            None => self.grow_next(),
        }
    }

    #[cold]
    #[inline(never)]
    unsafe fn grow_next(&mut self) -> FragRef<T> {
        let current = Fragment::new();
        let Some(id) = current.next() else {
            unsafe { unreachable_unchecked() }
        };
        let global = self
            .base
            .inner
            .push(current.clone())
            .ok()
            .expect(OUT_OF_SPACE);
        self.current = current;
        self.global = global;
        FragRef(FragAddr { global, local: id }, PhantomData)
    }

    pub fn push(&mut self, value: T) -> FragRef<T> {
        match self.current.push(value) {
            Ok(local) => FragRef(
                FragAddr {
                    global: self.global,
                    local,
                },
                PhantomData,
            ),
            Err(value) => self.grow_push(value),
        }
    }

    #[cold]
    #[inline(never)]
    pub fn grow_push(&mut self, value: T) -> FragRef<T> {
        let mut current = Fragment::new();
        let Ok(local) = current.push(value) else {
            unsafe { unreachable_unchecked() };
        };

        let global = self
            .base
            .inner
            .push(current.clone())
            .ok()
            .expect(OUT_OF_SPACE);

        self.current = current;
        self.global = global;

        FragRef(
            FragAddr {
                global: self.global,
                local,
            },
            PhantomData,
        )
    }

    pub fn extend<I>(&mut self, values: I) -> FragSlice<T>
    where
        I: IntoIterator<Item = T>,
        I::IntoIter: ExactSizeIterator,
    {
        let iter = values.into_iter();
        match self.current.extend(iter) {
            Ok((local, len)) => FragSlice(
                FragSliceAddr {
                    addr: FragAddr {
                        global: self.global,
                        local,
                    },
                    len,
                },
                PhantomData,
            ),
            Err(iter) => self.grow_extend(iter),
        }
    }

    #[cold]
    #[inline(never)]
    fn grow_extend(&mut self, iter: impl ExactSizeIterator<Item = T>) -> FragSlice<T> {
        let mut current = Fragment::new();
        let (local, len) = current.extend(iter).ok().expect(OUT_OF_SPACE);

        let global = self
            .base
            .inner
            .push(current.clone())
            .ok()
            .expect(OUT_OF_SPACE);

        self.current = current;
        self.global = global;

        FragSlice(
            FragSliceAddr {
                addr: FragAddr {
                    global: self.global,
                    local,
                },
                len,
            },
            PhantomData,
        )
    }

    pub fn indexed(&self, slice: FragSlice<T>) -> impl Iterator<Item = (FragRef<T>, &T)> {
        slice.keys().zip(&self[slice])
    }

    /// # Safety
    /// The caller must ensure he is the only one accessing the index.
    pub unsafe fn cross_access(&mut self, index: FragRef<T>) -> &mut T {
        let FragAddr { global, local } = index.0;
        if global == self.global {
            self.current.get_mut(local, true)
        } else {
            self.base.inner.get_mut(global, false).get_mut(local, true)
        }
    }

    /// # Safety
    /// The caller must ensure he is the only one accessing the index and
    /// the index is in bounds.
    pub unsafe fn ptr_to(&mut self, src: FragRef<T>) -> *mut T {
        let FragAddr { global, local } = src.0;
        (*self.base.inner.ptr_to(global)).ptr_to(local)
    }

    unsafe fn set_len_and_drop_overflow(&mut self, taken_blocks: usize) {
        self.base.inner.set_len_and_drop_overflow(taken_blocks);
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.base
            .inner
            .iter_mut()
            .flat_map(|block| block.iter_mut())
    }

    pub fn is_valid(&self, index: FragRef<T>) -> bool {
        let FragAddr { global, local } = index.0;
        global == self.global && self.current.is_valid(local)
    }
}

impl<T, const SIZE: usize> Clone for FragMap<T, SIZE> {
    fn clone(&self) -> Self {
        self.split()
    }
}

impl<T, const SIZE: usize> Default for FragMap<T, SIZE> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T, const SIZE: usize> Index<FragRef<T>> for FragMap<T, SIZE> {
    type Output = T;

    fn index(&self, index: FragRef<T>) -> &Self::Output {
        let FragAddr { global, local } = index.0;
        if global == self.global {
            self.current.get(local, true, false)
        } else {
            self.base
                .inner
                .get(global, false, false)
                .get(local, false, true)
        }
    }
}

// impl<T, const SIZE: usize> IndexMut<FragRef<T>> for FragMap<T, SIZE> {
//     fn index_mut(&mut self, index: FragRef<T>) -> &mut Self::Output {
//         let FragAddr { global, local } = index.0;

//         debug_assert!(
//             global == self.global,
//             "cannot mutate a value from another fragment"
//         );

//         self.current.get_mut(local)
//     }
// }

impl<T, const SIZE: usize> Index<FragSlice<T>> for FragMap<T, SIZE> {
    type Output = [T];

    fn index(&self, index: FragSlice<T>) -> &Self::Output {
        let FragSliceAddr {
            addr: FragAddr { global, local },
            len,
        } = index.0;
        if global == self.global {
            self.current.get_slice(local, len, true, false)
        } else {
            self.base
                .inner
                .get(global, false, false)
                .get_slice(local, len, false, true)
        }
    }
}

// impl<T, const SIZE: usize> IndexMut<FragSlice<T>> for FragMap<T, SIZE> {
//     fn index_mut(&mut self, index: FragSlice<T>) -> &mut Self::Output {
//         let FragSliceAddr {
//             addr: FragAddr { global, local },
//             len,
//         } = index.0;

//         debug_assert!(global == self.global, "cannot mutate non local fragment");

//         self.current.get_slice_mut(local, len)
//     }
// }

struct Fragment<T, const SIZE: usize> {
    inner: NonNull<FragmentInner<T, SIZE>>,
}

unsafe impl<T, const SIZE: usize> Sync for Fragment<T, SIZE> where T: Sync {}
unsafe impl<T, const SIZE: usize> Send for Fragment<T, SIZE> where T: Send {}

impl<T, const SIZE: usize> Fragment<T, SIZE> {
    unsafe fn next(&self) -> Option<NonMaxU16> {
        let len = self.len();
        if len < SIZE {
            Some(NonMaxU16(len as u16))
        } else {
            None
        }
    }

    fn is_valid(&self, local: NonMaxU16) -> bool {
        local.get() < self.len() as u16 && local.get() >= self.frozen() as u16
    }

    fn new() -> Self {
        let mut inner: Box<MaybeUninit<FragmentInner<T, SIZE>>> = Box::new_uninit();
        unsafe {
            addr_of_mut!((*inner.as_mut_ptr()).ref_count).write(AtomicUsize::new(1));
            addr_of_mut!((*inner.as_mut_ptr()).len).write(AtomicUsize::new(0));
            addr_of_mut!((*inner.as_mut_ptr()).frozen).write(AtomicUsize::new(0));
        }
        Self {
            inner: unsafe { NonNull::new_unchecked(Box::into_raw(inner.assume_init())) },
        }
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> + '_ {
        let len = self.len();
        unsafe {
            self.inner
                .as_mut()
                .data
                .get_unchecked_mut(..len)
                .iter_mut()
                .map(|i| i.assume_init_mut())
        }
    }

    unsafe fn set_len_and_drop_overflow(&mut self, to_size: usize) {
        let len = self.len();
        if to_size < len {
            self.inner.as_mut().data[to_size..len]
                .iter_mut()
                .for_each(|x| x.assume_init_drop());
        }
        addr_of_mut!((*self.inner.as_ptr()).len).write(AtomicUsize::new(to_size));
    }

    fn get(&self, index: NonMaxU16, local: bool, check_frozen: bool) -> &T {
        let index = index.get() as usize;

        if local && !check_frozen {
            debug_assert!(
                index < self.len(),
                "index out of bounds, index: {}, len: {}",
                index,
                self.len(),
            );
        } else {
            debug_assert!(
                index < self.frozen(),
                "index out of bounds, index: {}, frozen: {}, len: {}",
                index,
                self.frozen(),
                self.len(),
            );
        }

        unsafe {
            (*self.inner.as_ptr())
                .data
                .get_unchecked(index)
                .assume_init_ref()
        }
    }

    fn get_mut(&mut self, index: NonMaxU16, check_frozen: bool) -> &mut T {
        let index = index.get() as usize;

        debug_assert!(
            index < self.len() && (!check_frozen || index >= self.frozen()),
            "index out of bounds, index: {}, len: {}, frozen: {}",
            index,
            self.len(),
            self.frozen()
        );

        unsafe {
            (*self.inner.as_ptr())
                .data
                .get_unchecked_mut(index)
                .assume_init_mut()
        }
    }

    fn get_slice(&self, index: NonMaxU16, len: NonMaxU16, local: bool, check_frozen: bool) -> &[T] {
        let index = index.get() as usize;
        let len = len.get() as usize;

        if local || !check_frozen {
            debug_assert!(
                index + len <= self.len(),
                "index out of bounds, index: {}, len: {}",
                index + len,
                self.len()
            );
        } else {
            debug_assert!(
                index + len <= self.frozen(),
                "index out of bounds, index: {}, frozen: {}",
                index + len,
                self.frozen()
            );
        }

        unsafe {
            slice::from_raw_parts(
                (*self.inner.as_ptr())
                    .data
                    .get_unchecked(index)
                    .assume_init_ref(),
                len,
            )
        }
    }

    // fn get_slice_mut(&mut self, index: NonMaxU16, len: NonMaxU16) -> &mut [T] {
    //     let index = index.get() as usize;
    //     let len = len.get() as usize;

    //     debug_assert!(
    //         index >= self.frozen() && index + len <= self.len(),
    //         "index out of bounds, index: {}, len: {}",
    //         index + len,
    //         self.frozen()
    //     );

    //     unsafe {
    //         slice::from_raw_parts_mut(
    //             (*self.inner.as_ptr())
    //                 .data
    //                 .get_unchecked_mut(index)
    //                 .assume_init_mut(),
    //             len,
    //         )
    //     }
    // }

    fn push(&mut self, value: T) -> Result<NonMaxU16, T> {
        let len = self.extend_len(1);
        if len < SIZE {
            unsafe {
                (*self.inner.as_ptr())
                    .data
                    .get_unchecked_mut(len)
                    .write(value);
            }
            Ok(unsafe { NonMaxU16(len as u16) })
        } else {
            Err(value)
        }
    }

    fn extend<I: ExactSizeIterator<Item = T>>(
        &mut self,
        values: I,
    ) -> Result<(NonMaxU16, NonMaxU16), I> {
        let values_len = values.len();
        let len = self.extend_len(values_len);
        if len + values_len <= SIZE {
            unsafe {
                let data = (*self.inner.as_ptr()).data.get_unchecked_mut(len..);
                for (i, value) in values.enumerate() {
                    data.get_unchecked_mut(i).write(value);
                }
            }

            unsafe { Ok((NonMaxU16(len as u16), NonMaxU16(values_len as u16))) }
        } else {
            Err(values)
        }
    }

    fn extend_len(&self, amount: usize) -> usize {
        let inner = unsafe { self.inner.as_ref() };
        inner.len.fetch_add(amount, Ordering::Relaxed)
    }

    fn len(&self) -> usize {
        unsafe { self.inner.as_ref().len.load(Ordering::Relaxed) }
    }

    fn frozen(&self) -> usize {
        unsafe { self.inner.as_ref().frozen.load(Ordering::Relaxed) }
    }

    fn freeze(&self) {
        let inner = unsafe { self.inner.as_ref() };
        let prev = inner.len.load(Ordering::Relaxed);
        inner.frozen.store(prev, Ordering::Relaxed);
    }

    unsafe fn ptr_to(&mut self, local: NonMaxU16) -> *mut T {
        let local = local.get() as usize;
        self.inner
            .as_mut()
            .data
            .get_unchecked_mut(local)
            .assume_init_mut()
    }
}

impl<T, const SIZE: usize> Clone for Fragment<T, SIZE> {
    fn clone(&self) -> Self {
        unsafe {
            (*addr_of!((*self.inner.as_ptr()).ref_count))
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        }
        Self { inner: self.inner }
    }
}

impl<T, const SIZE: usize> Drop for Fragment<T, SIZE> {
    fn drop(&mut self) {
        unsafe {
            let ref_count = addr_of!((*self.inner.as_ptr()).ref_count);
            if (*ref_count).fetch_sub(1, std::sync::atomic::Ordering::Relaxed) == 1 {
                let mut s = Box::from_raw(self.inner.as_ptr());
                if mem::needs_drop::<T>() {
                    s.data
                        .get_unchecked_mut(..s.len.into_inner())
                        .iter_mut()
                        .map(|e| ptr::read(e).assume_init())
                        .for_each(drop);
                }
            }
        }
    }
}

#[repr(transparent)]
#[rustc_layout_scalar_valid_range_end(65_534)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct NonMaxU16(u16);

impl Default for NonMaxU16 {
    fn default() -> Self {
        unsafe { Self(0) }
    }
}

struct FragmentInner<T, const SIZE: usize> {
    ref_count: AtomicUsize,
    len: AtomicUsize,
    frozen: AtomicUsize,
    data: [MaybeUninit<T>; SIZE],
}

#[allow(clippy::derive_hash_xor_eq)]
#[derive(Copy, Clone, PartialOrd, Ord, Hash, Debug)]
pub struct FragAddr {
    global: NonMaxU16,
    local: NonMaxU16,
}
impl FragAddr {
    /// # Safety
    /// User has to ensure that created address points to valid fragment and record
    pub const unsafe fn new(global: u16, local: u16) -> FragAddr {
        FragAddr {
            global: NonMaxU16(global),
            local: NonMaxU16(local),
        }
    }

    /// # Safety
    /// The inputted u32 must be an result of `FragAddr::to_u32`
    pub const unsafe fn from_u32(addr: u32) -> FragAddr {
        FragAddr {
            global: NonMaxU16((addr >> 16) as u16),
            local: NonMaxU16(addr as u16),
        }
    }

    pub const fn to_u32(self) -> u32 {
        (self.global.get() as u32) << 16 | self.local.get() as u32
    }

    pub fn right_after(&self, key: FragAddr) -> bool {
        self.global == key.global && Some(self.local.get()) == key.local.get().checked_add(1)
    }
}

impl const PartialEq for FragAddr {
    fn eq(&self, other: &Self) -> bool {
        self.global.get() == other.global.get() && self.local.get() == other.local.get()
    }
}

impl Eq for FragAddr {}

#[allow(clippy::derive_hash_xor_eq)]
#[derive(Copy, Clone, PartialOrd, Ord, Hash, Debug)]
pub struct FragSliceAddr {
    addr: FragAddr,
    len: NonMaxU16,
}

impl FragSliceAddr {
    /// # Safety
    /// User has to ensure that created address range points to valid fragment and record
    pub const unsafe fn new(global: u16, local: u16, len: u16) -> Self {
        Self {
            addr: FragAddr::new(global, local),
            len: NonMaxU16(len),
        }
    }

    pub fn from_addr(addr: FragAddr) -> FragSliceAddr {
        FragSliceAddr {
            addr,
            len: unsafe { NonMaxU16(1) },
        }
    }
}

impl Default for FragSliceAddr {
    fn default() -> Self {
        unsafe {
            Self {
                addr: FragAddr {
                    global: NonMaxU16(0),
                    local: NonMaxU16(0),
                },
                len: NonMaxU16(0),
            }
        }
    }
}

impl const PartialEq for FragSliceAddr {
    fn eq(&self, other: &Self) -> bool {
        self.addr == other.addr && self.len.get() == other.len.get()
    }
}

impl Eq for FragSliceAddr {}

impl FragSliceAddr {
    pub fn addr(self) -> FragAddr {
        self.addr
    }

    pub fn len(self) -> usize {
        self.len.get() as usize
    }

    pub fn is_empty(self) -> bool {
        self.len.get() == 0
    }

    pub fn keys(self) -> impl Iterator<Item = FragAddr> + DoubleEndedIterator + ExactSizeIterator {
        (self.addr.local.get()..self.addr.local.get() + self.len.get()).map(move |i| FragAddr {
            global: self.addr.global,
            local: unsafe { NonMaxU16(i) },
        })
    }
}

#[cfg(test)]
mod test {
    use std::thread;

    use super::*;

    #[test]
    fn it_works() {
        const SIZE: usize = (1 << 16) - 2;
        let frag = [(); 20].map(|_| FragBase::<usize, SIZE>::new());

        thread::scope(|s| {
            let threads = [(); 4].map(|_| {
                let mut frag = frag.clone().map(|f| f.as_map());
                s.spawn(move || {
                    for i in 0..SIZE {
                        frag.iter_mut().for_each(|f| {
                            f.extend([i, 2, 4, 4, 5]);
                        });
                    }

                    frag.iter_mut()
                        .map(|f| f.extend([1, 2, 4]))
                        .collect::<Vec<_>>()
                })
            });

            let frag = frag.map(|f| f.as_map());
            for jh in threads {
                for (p, frag) in jh.join().unwrap().into_iter().zip(&frag) {
                    assert_eq!(&frag[p], &[1, 2, 4]);
                }
            }
        });
    }
}

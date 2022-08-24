#![feature(allocator_api)]

use std::{
    fmt::Debug,
    mem::MaybeUninit,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

const BUMP_ALLOC_FRAME: usize = 1 << 14;

thread_local! {
    static BUMP_ALLOC: BumpAlloc = BumpAlloc::new(BUMP_ALLOC_FRAME);
}

pub struct BumpAlloc {
    inner: NonNull<BumpAllocInner>,
}

impl Debug for BumpAlloc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", unsafe { self.inner.as_ref() })
    }
}

impl BumpAlloc {
    fn new(frame_size: usize) -> Self {
        let inner = Box::into_raw(Box::new(BumpAllocInner::new(frame_size)));
        Self {
            inner: unsafe { NonNull::new_unchecked(inner) },
        }
    }

    fn make_ref(&self) -> BumpAllocRef {
        unsafe {
            self.inner.clone().as_mut().refs += 1;
        }
        BumpAllocRef { inner: self.inner }
    }
}

impl Drop for BumpAlloc {
    fn drop(&mut self) {
        let s = unsafe { self.inner.as_mut() };
        s.not_owned = true;
        if s.should_drop() {
            unsafe { Box::from_raw(self.inner.as_ptr()) };
        }
    }
}

pub struct BumpAllocRef {
    inner: NonNull<BumpAllocInner>,
}

unsafe impl std::alloc::Allocator for BumpAllocRef {
    fn allocate(
        &self,
        layout: std::alloc::Layout,
    ) -> Result<NonNull<[u8]>, std::alloc::AllocError> {
        if layout.size() == 0 {
            return Err(std::alloc::AllocError);
        }

        let size = layout
            .align_to(std::mem::align_of::<usize>())
            .map_err(|_| std::alloc::AllocError)?
            .pad_to_align()
            .size();

        let s = unsafe { self.inner.clone().as_mut() };

        let len = size / std::mem::size_of::<usize>();

        let chunk = s.get_fitting_chunk(len);
        let ptr = unsafe { chunk.as_mut_ptr().add(chunk.len()) };

        let slice = unsafe { std::slice::from_raw_parts_mut(ptr as *mut u8, size) };
        unsafe { chunk.set_len(chunk.len() + len) };

        Ok(unsafe { NonNull::new_unchecked(slice) })
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: std::alloc::Layout) {
        let size = layout
            .align_to(std::mem::align_of::<usize>())
            .unwrap_unchecked()
            .pad_to_align()
            .size();
        let len = size / std::mem::size_of::<usize>();

        let s = self.inner.clone().as_mut();
        let chunk = s.chunks.last_mut().unwrap_unchecked();
        let chunk_ptr = unsafe { chunk.as_mut_ptr().add(chunk.len()).sub(len) };

        if chunk_ptr as usize == ptr.as_ptr() as usize {
            unsafe { chunk.set_len(chunk.len() - len) };
        }
    }
}

impl Drop for BumpAllocRef {
    fn drop(&mut self) {
        let s = unsafe { self.inner.as_mut() };
        s.refs -= 1;
        if s.should_drop() {
            #[cold]
            fn cold_drop(s: &mut BumpAllocRef) {
                unsafe { drop(Box::from_raw(s.inner.as_ptr())) };
            }
            cold_drop(self);
        } else if s.refs == 0 {
            s.clear();
        }
    }
}

#[derive(Default, Debug)]
struct BumpAllocInner {
    chunks: Vec<Vec<MaybeUninit<usize>>>,
    refs: usize,
    not_owned: bool,
    frame_size: usize,
}

impl BumpAllocInner {
    fn new(frame_size: usize) -> Self {
        Self {
            frame_size,
            chunks: vec![Vec::with_capacity(frame_size)],
            ..Default::default()
        }
    }

    fn get_fitting_chunk(&mut self, size: usize) -> &mut Vec<MaybeUninit<usize>> {
        let frame_size = self.frame_size.max(size);
        let last_chunk = unsafe { self.chunks.last_mut().unwrap_unchecked() };

        if last_chunk.capacity() - last_chunk.len() >= size {
            unsafe { self.chunks.last_mut().unwrap_unchecked() }
        } else {
            self.find_chunk(size, frame_size)
        }
    }

    #[cold]
    fn find_chunk(&mut self, size: usize, frame_size: usize) -> &mut Vec<MaybeUninit<usize>> {
        let full_chunk = unsafe { self.chunks.pop().unwrap_unchecked() };
        let len = full_chunk.len();
        let new_pos = self
            .chunks
            .iter()
            .position(|chunk| chunk.len() > len)
            .unwrap_or(0);

        self.chunks.insert(new_pos, full_chunk);

        let new_last_chunk = unsafe { self.chunks.last().unwrap_unchecked() };
        if new_last_chunk.capacity() - new_last_chunk.len() < size {
            self.chunks.push(Vec::with_capacity(frame_size));
        }
        unsafe { self.chunks.last_mut().unwrap_unchecked() }
    }

    fn clear(&mut self) {
        self.chunks.iter_mut().for_each(|chunk| unsafe {
            chunk.set_len(0);
        });
    }

    fn should_drop(&self) -> bool {
        self.refs == 0 && self.not_owned
    }
}

pub struct BumpVec<T> {
    inner: Vec<T, BumpAllocRef>,
}

impl<T> BumpVec<T> {
    pub fn new() -> Self {
        Self {
            inner: Vec::new_in(BUMP_ALLOC.with(|b| b.make_ref())),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            inner: Vec::with_capacity_in(capacity, BUMP_ALLOC.with(|b| b.make_ref())),
        }
    }
}

impl<T> Deref for BumpVec<T> {
    type Target = Vec<T, BumpAllocRef>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> DerefMut for BumpVec<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

#[macro_export]
macro_rules! bumpvec {
    ($($x:expr),*; $alloc:expr) => {
        {
            let mut vec = BumpVec::new();
            vec.extend([$($x),*]);
        }
    };
    ($expr:expr; $len:expr; $alloc:expr) => {
        {
            let mut v = $crate::BumpVec::new();
            v.resize($len, $expr);
            v
        }
    };
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
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
                println!("{:?}", ptr);
            }),
            ("bump-vec", |timer| {
                let vec = BumpVec::<usize>::with_capacity(100);
                timer.start();
                let mut ptr = 0;
                for _ in 0..1_000_000 {
                    let vec = BumpVec::<usize>::with_capacity(100);
                    ptr += vec.as_ptr() as usize;
                }
                timer.stop();
                drop(vec);
                println!("{:?}", ptr);
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
            let end = self.to.unwrap_or_else(|| std::time::Instant::now());
            println!("{name} {:?}", end.duration_since(start));
        }
    }

    fn bench(task: &[(&str, fn(&mut Timer))]) {
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

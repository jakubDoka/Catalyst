use std::{
    ops::{Deref, DerefMut},
    ptr::NonNull,
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        RwLock, RwLockReadGuard,
    },
};

pub struct RWSwapReader<T> {
    inner: NonNull<RWSwapInner<T>>,
}

unsafe impl<T: Send + Sync> Send for RWSwapReader<T> {}
unsafe impl<T: Send + Sync> Sync for RWSwapReader<T> {}

impl<T> RWSwapReader<T> {
    pub fn read(&self) -> RWSwapReadAccess<T> {
        let (data, lock) = unsafe { self.inner.as_ref() }.read_access();
        RWSwapReadAccess { data, _lock: lock }
    }
}

pub struct RWSwapReadAccess<'a, T> {
    _lock: RwLockReadGuard<'a, ()>,
    data: &'a T,
}

impl<T> Deref for RWSwapReadAccess<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.data
    }
}

impl<T> Clone for RWSwapReader<T> {
    fn clone(&self) -> Self {
        unsafe { self.inner.as_ref() }
            .ref_count
            .fetch_add(1, Ordering::Relaxed);
        Self { inner: self.inner }
    }
}

impl<T> Drop for RWSwapReader<T> {
    fn drop(&mut self) {
        let current = unsafe { self.inner.as_ref() }
            .ref_count
            .fetch_sub(1, Ordering::Relaxed);
        if current == 1 {
            unsafe { Box::from_raw(self.inner.as_ptr()) };
        }
    }
}

pub struct RWSwapWriter<T> {
    inner: NonNull<RWSwapInner<T>>,
}

unsafe impl<T: Send + Sync> Send for RWSwapWriter<T> {}
unsafe impl<T: Send + Sync> Sync for RWSwapWriter<T> {}

impl<T> RWSwapWriter<T> {
    pub fn new(data: T) -> Self
    where
        T: Clone,
    {
        let inner = Box::new(RWSwapInner::new(data));
        Self {
            inner: unsafe { NonNull::new_unchecked(Box::into_raw(inner)) },
        }
    }

    pub fn reader(&self) -> RWSwapReader<T> {
        unsafe { self.inner.as_ref() }
            .ref_count
            .fetch_add(1, Ordering::Relaxed);
        RWSwapReader { inner: self.inner }
    }

    pub fn swap(&mut self) {
        unsafe { self.inner.as_mut() }.swap();
    }
}

impl<T> Deref for RWSwapWriter<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.inner.as_ref() }.local_read_access()
    }
}

impl<T> DerefMut for RWSwapWriter<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.inner.as_mut() }.write_access()
    }
}

impl<T> Drop for RWSwapWriter<T> {
    fn drop(&mut self) {
        let current = unsafe { self.inner.as_ref() }
            .ref_count
            .fetch_sub(1, Ordering::Relaxed);
        if current == 1 {
            unsafe { Box::from_raw(self.inner.as_ptr()) };
        }
    }
}

struct RWSwapInner<T> {
    ref_count: AtomicUsize,
    reading: AtomicBool,
    data: [(T, RwLock<()>); 2],
}

impl<T> RWSwapInner<T> {
    fn new(value: T) -> Self
    where
        T: Clone,
    {
        Self {
            ref_count: AtomicUsize::new(1),
            reading: AtomicBool::new(false),
            data: [
                (value.clone(), RwLock::default()),
                (value, RwLock::default()),
            ],
        }
    }

    fn read_access(&self) -> (&T, RwLockReadGuard<()>) {
        let index = self.reading.load(Ordering::Relaxed) as usize;
        (&self.data[index].0, self.data[index].1.read().unwrap())
    }

    fn write_access(&mut self) -> &mut T {
        let index = !self.reading.load(Ordering::Relaxed) as usize;
        &mut self.data[index].0
    }

    fn local_read_access(&self) -> &T {
        let index = !self.reading.load(Ordering::Relaxed) as usize;
        &self.data[index].0
    }

    fn swap(&self) {
        let reading = self.reading.fetch_not(Ordering::Relaxed);
        let reading = reading as usize;
        drop(self.data[reading].1.write().unwrap());
    }
}

#[cfg(test)]
mod test {
    use std::iter;

    use super::*;

    #[test]
    fn test() {
        let mut swap = RWSwapWriter::new(0);
        let readers = [(); 4].map(|()| swap.reader()).map(|reader| {
            std::thread::spawn(move || {
                iter::repeat_with(|| reader.read())
                    .map(|access| *access)
                    .take_while(|&data| data != 100)
                    .last();
                *reader.read()
            })
        });

        std::thread::sleep(std::time::Duration::from_millis(100));

        for _ in 0..100 {
            *swap += 1;
            swap.swap();
            *swap += 1;
        }

        println!("{}", *swap);

        let sum = readers
            .map(|handle| handle.join().unwrap())
            .into_iter()
            .sum::<usize>();

        assert_eq!(sum, 400);
    }
}

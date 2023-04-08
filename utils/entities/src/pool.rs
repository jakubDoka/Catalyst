use std::{
    default::default,
    iter,
    mem::ManuallyDrop,
    ops::{Deref, DerefMut},
    ptr,
    sync::{Arc, Mutex},
};

#[macro_export]
macro_rules! static_pool {
    ($($vis:vis $name:ident: $ty:ty;)+) => {$(
        $vis static $name: std::sync::LazyLock<$crate::Pool<$ty>> = std::sync::LazyLock::new($crate::Pool::default);
    )+};
}

pub struct Pool<T> {
    allocators: Arc<Mutex<Vec<T>>>,
}

impl<T> Pool<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&self, factory: impl FnOnce() -> T) -> Pooled<T> {
        let allocator = self
            .allocators
            .lock()
            .unwrap()
            .pop()
            .unwrap_or_else(factory);
        Pooled {
            pool: Some(self.clone()),
            inner: ManuallyDrop::new(allocator),
        }
    }

    pub fn get_or_default(&self) -> Pooled<T>
    where
        T: Default,
    {
        self.get(T::default)
    }

    /// Iterator is infinite.
    pub fn iter<'a>(
        &'a self,
        mut f: impl FnMut() -> T + 'a,
    ) -> impl Iterator<Item = Pooled<T>> + 'a {
        iter::repeat_with(move || self.get(&mut f))
    }

    pub fn iter_or_default<'a>(&'a self) -> impl Iterator<Item = Pooled<T>> + 'a
    where
        T: Default,
    {
        self.iter(T::default)
    }
}

impl<T> Default for Pool<T> {
    fn default() -> Self {
        Self {
            allocators: default(),
        }
    }
}

impl<T> Clone for Pool<T> {
    fn clone(&self) -> Self {
        Self {
            allocators: self.allocators.clone(),
        }
    }
}

#[derive(Default)]
pub struct Pooled<T> {
    pool: Option<Pool<T>>,
    inner: ManuallyDrop<T>,
}

impl<T> Deref for Pooled<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> DerefMut for Pooled<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<T> Drop for Pooled<T> {
    fn drop(&mut self) {
        if let Some(pool) = self.pool.take() {
            pool.allocators
                .lock()
                .unwrap()
                .push(unsafe { ptr::read(&*self.inner) });
        } else {
            unsafe {
                ManuallyDrop::drop(&mut self.inner);
            }
        }
    }
}

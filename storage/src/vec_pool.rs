use std::{
    cell::RefCell,
    mem::MaybeUninit,
    ops::{Deref, DerefMut},
    rc::Rc,
};

struct VecPoolInner {
    storage: [Vec<(*mut u32, usize)>; 8],
}

impl VecPoolInner {
    pub fn return_vec<T>(&mut self, mut vec: Vec<T>) {
        let align = std::mem::align_of::<T>();
        let size = std::mem::size_of::<T>();
        vec.clear();
        let (ptr, _, cap) = vec.into_raw_parts();
        if cap != 0 {
            self.storage[align - 1].push((ptr as *mut u32, cap * size));
        }
    }
}

impl Drop for VecPoolInner {
    fn drop(&mut self) {
        for (i, vec) in self.storage.iter_mut().enumerate() {
            unsafe {
                for (ptr, cap) in vec.drain(..) {
                    match i + 1 {
                        1 => drop(Vec::from_raw_parts(ptr as *mut u8, 0, cap)),
                        2 => drop(Vec::from_raw_parts(ptr as *mut u16, 0, cap / 2)),
                        4 => drop(Vec::from_raw_parts(ptr as *mut u32, 0, cap / 4)),
                        8 => drop(Vec::from_raw_parts(ptr as *mut u64, 0, cap / 8)),
                        _ => panic!("Unsupported alignment: {}", i),
                    }
                }
            }
        }
    }
}

pub struct VecPool {
    inner: Rc<RefCell<VecPoolInner>>,
}

impl VecPool {
    pub fn new() -> Self {
        let mut storage: [MaybeUninit<Vec<(*mut (), usize)>>; 8] = unsafe { std::mem::zeroed() };

        for item in storage.iter_mut() {
            *item = MaybeUninit::new(Vec::new());
        }

        Self {
            inner: Rc::new(RefCell::new(VecPoolInner {
                storage: unsafe { std::mem::transmute(storage) },
            })),
        }
    }

    pub fn get<T>(&self) -> PoolVec<T> {
        let align = std::mem::align_of::<T>();
        let size = std::mem::size_of::<T>();
        let vec = self.inner.borrow_mut().storage[align - 1]
            .pop()
            .map(|(ptr, cap)| unsafe { Vec::from_raw_parts(ptr as *mut T, 0, cap / size) })
            .unwrap_or(Vec::new());

        PoolVec {
            vec,
            inner: self.inner.clone(),
        }
    }

    pub fn alloc<T: Clone>(&self, slice: &[T]) -> PoolVec<T> {
        let mut vec = self.get();
        vec.extend_from_slice(slice);
        vec
    }

    pub fn alloc_iter<T: Clone>(&self, iter: impl IntoIterator<Item = T>) -> PoolVec<T> {
        let mut vec = self.get();
        vec.extend(iter);
        vec
    }

    pub fn with_capacity<T>(&self, capacity: usize) -> PoolVec<T> {
        let mut vec = self.get();
        vec.reserve(capacity);
        vec
    }

    pub fn of_size<T: Clone>(&self, init: T, size: usize) -> PoolVec<T> {
        let mut vec = self.get();
        vec.resize(size, init);
        vec
    }
}

pub struct PoolVec<T> {
    vec: Vec<T>,
    inner: Rc<RefCell<VecPoolInner>>,
}

impl<T> Deref for PoolVec<T> {
    type Target = Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.vec
    }
}

impl<T> DerefMut for PoolVec<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.vec
    }
}

impl<T> Drop for PoolVec<T> {
    fn drop(&mut self) {
        self.inner
            .borrow_mut()
            .return_vec(std::mem::take(&mut self.vec));
    }
}

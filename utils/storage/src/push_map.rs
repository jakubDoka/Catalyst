use std::{
    fmt::Debug,
    ops::{Index, IndexMut},
};

use crate::VRef;

#[derive(Clone)]
pub struct PushMap<T> {
    data: Vec<T>,
}

impl<T> PushMap<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, value: T) -> VRef<T> {
        let id = self.data.len();
        self.data.push(value);
        unsafe { VRef::new(id) }
    }

    pub fn clear(&mut self) {
        self.data.clear();
    }

    pub fn get(&self, index: VRef<T>) -> Option<&T> {
        self.data.get(index.index())
    }

    pub fn truncate(&mut self, len: usize) {
        self.data.truncate(len);
    }
}

impl<T> Index<VRef<T>> for PushMap<T> {
    type Output = T;

    fn index(&self, index: VRef<T>) -> &Self::Output {
        &self.data[index.index()]
    }
}

impl<T> IndexMut<VRef<T>> for PushMap<T> {
    fn index_mut(&mut self, index: VRef<T>) -> &mut Self::Output {
        &mut self.data[index.index()]
    }
}

impl<T: Debug> Debug for PushMap<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.data.iter()).finish()
    }
}

impl<T> Default for PushMap<T> {
    fn default() -> Self {
        Self {
            data: Default::default(),
        }
    }
}

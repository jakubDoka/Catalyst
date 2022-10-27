use std::{
    default::default,
    fmt::Debug,
    ops::{Index, IndexMut},
};

use serde::{Deserialize, Serialize};

use crate::VRef;

#[derive(Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct PushMap<T> {
    data: Vec<T>,
}

impl<T> PushMap<T> {
    pub fn new() -> Self {
        Self::default()
    }

    /// # Safety
    /// The id it self is not valid yet, it can be used after pushing
    /// at least one element.
    pub unsafe fn next(&mut self) -> VRef<T> {
        VRef::new(self.data.len())
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
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

    pub fn values(&self) -> impl Iterator<Item = &T> {
        self.data.iter()
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.data.iter_mut()
    }

    pub fn keys(&self) -> impl Iterator<Item = VRef<T>> {
        (0..self.data.len()).map(|i| unsafe { VRef::new(i) })
    }

    pub fn iter(&self) -> impl Iterator<Item = (VRef<T>, &T)> {
        self.keys().zip(self.values())
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
        Self { data: default() }
    }
}

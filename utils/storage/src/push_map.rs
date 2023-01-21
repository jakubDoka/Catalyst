use std::{
    default::default,
    fmt::Debug,
    ops::{Index, IndexMut},
};

use bump_alloc::*;
use bytecheck::CheckBytes;
use rkyv::{Archive, Deserialize, Serialize};

use crate::VRef;

#[derive(Archive, Serialize, Deserialize, PartialEq, Eq)]
#[archive_attr(derive(CheckBytes))]
pub struct PushMap<T> {
    data: Vec<T>,
}

impl<T: Relocated> Relocated for PushMap<T> {
    fn mark(&self, marker: &mut bump_alloc::FragRelocMarker) {
        self.data.mark(marker);
    }

    fn remap(&mut self, ctx: &bump_alloc::FragRelocMapping) -> Option<()> {
        self.data.remap(ctx)
    }
}

impl<T> PushMap<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: Vec::with_capacity(capacity),
        }
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
        VRef::new(id)
    }

    pub fn pop(&mut self) -> Option<T> {
        self.data.pop()
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
        (0..self.data.len()).map(|i| VRef::new(i))
    }

    pub fn iter(&self) -> impl Iterator<Item = (VRef<T>, &T)> + DoubleEndedIterator {
        self.data.iter().enumerate().map(|(i, v)| (VRef::new(i), v))
    }

    pub fn extend(&mut self, other: impl IntoIterator<Item = T>) -> VSlice<T> {
        let prev = self.data.len();
        self.data.extend(other);
        VSlice::new(prev..self.data.len())
    }

    pub fn extend_from_within(&mut self, other: VSlice<T>) -> VSlice<T>
    where
        T: Clone,
    {
        let prev = self.data.len();
        self.data.extend_from_within(other.range());
        VSlice::new(prev..self.data.len())
    }

    pub fn bump_slice(&mut self, items: &[T]) -> VSlice<T>
    where
        T: Clone,
    {
        self.extend(items.iter().cloned())
    }

    pub fn indexed(&self, slice: VSlice<T>) -> impl Iterator<Item = (VRef<T>, &T)> {
        self.data[slice.range()]
            .iter()
            .zip(slice.keys())
            .map(|(elem, key)| (key, elem))
    }
}

impl<T: Clone> Clone for PushMap<T> {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
        }
    }

    fn clone_from(&mut self, source: &Self) {
        self.data.clone_from(&source.data);
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

impl<T> Index<VSlice<T>> for PushMap<T> {
    type Output = [T];

    fn index(&self, index: VSlice<T>) -> &Self::Output {
        &self.data[index.range()]
    }
}

impl<T> IndexMut<VSlice<T>> for PushMap<T> {
    fn index_mut(&mut self, index: VSlice<T>) -> &mut Self::Output {
        &mut self.data[index.range()]
    }
}

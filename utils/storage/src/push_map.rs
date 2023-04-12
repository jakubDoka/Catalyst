use std::{
    borrow::Borrow,
    default::default,
    fmt::Debug,
    mem,
    ops::{Index, IndexMut},
};

use entities::*;

use crate::VRef;

#[repr(transparent)]
pub struct PushMapView<T> {
    data: [T],
}

impl<T> PushMapView<T> {
    pub fn new(base: &PushMap<T>, slice: VSlice<T>) -> &Self {
        unsafe { mem::transmute(&base[slice]) }
    }

    pub fn values(&self) -> impl Iterator<Item = &T> {
        self.data.iter()
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}

impl<T: Clone> ToOwned for PushMapView<T> {
    type Owned = PushMap<T>;

    fn to_owned(&self) -> Self::Owned {
        PushMap {
            data: self.data.to_owned(),
        }
    }
}

impl<T> Default for &PushMapView<T> {
    fn default() -> Self {
        unsafe { mem::transmute::<&[T], Self>(&[]) }
    }
}

impl<T> Borrow<PushMapView<T>> for PushMap<T> {
    fn borrow(&self) -> &PushMapView<T> {
        self.as_view()
    }
}

impl<T> Index<VRef<T>> for PushMapView<T> {
    type Output = T;

    fn index(&self, key: VRef<T>) -> &T {
        &self.data[key.index()]
    }
}

impl<T> Index<VSlice<T>> for PushMapView<T> {
    type Output = [T];

    fn index(&self, key: VSlice<T>) -> &[T] {
        &self.data[key.range()]
    }
}

pub struct PushMapCheck<'a, T> {
    map: &'a mut PushMap<T>,
    index: usize,
}

impl<'a, T> PushMapCheck<'a, T> {
    pub fn new(map: &'a mut PushMap<T>) -> Self {
        Self {
            index: map.len(),
            map,
        }
    }

    fn data(&self) -> &[T] {
        // SAFETY: The index is always valid due to encapsulation
        unsafe { self.map.data.get_unchecked(self.index..) }
    }

    fn data_mut(&mut self) -> &mut [T] {
        // SAFETY: The index is always valid due to encapsulation
        unsafe { self.map.data.get_unchecked_mut(self.index..) }
    }

    pub fn push(&mut self, value: T) -> VRef<T> {
        let id = self.map.len() - self.index;
        self.map.push(value);
        VRef::new(id)
    }

    pub fn extend(&mut self, values: impl IntoIterator<Item = T>) -> VSlice<T> {
        let id = self.map.len() - self.index;
        self.map.data.extend(values);
        VSlice::new(id..self.map.len() - self.index)
    }

    pub fn finish(self) -> VSlice<T> {
        VSlice::new(self.index..self.map.len())
    }

    pub fn indexed(&self, slice: VSlice<T>) -> impl ExactSizeIterator<Item = (VRef<T>, &T)> {
        self[slice]
            .iter()
            .enumerate()
            .map(move |(i, e)| (VRef::new(slice.start() as usize + i), e))
    }

    pub fn pop(&mut self) -> Option<T> {
        self.map.pop()
    }
}

impl<'a, T> Index<VRef<T>> for PushMapCheck<'a, T> {
    type Output = T;

    fn index(&self, key: VRef<T>) -> &T {
        &self.data()[key.index()]
    }
}

impl<'a, T> IndexMut<VRef<T>> for PushMapCheck<'a, T> {
    fn index_mut(&mut self, key: VRef<T>) -> &mut T {
        &mut self.data_mut()[key.index()]
    }
}

impl<'a, T> Index<VSlice<T>> for PushMapCheck<'a, T> {
    type Output = [T];

    fn index(&self, key: VSlice<T>) -> &[T] {
        &self.data()[key.range()]
    }
}

impl<'a, T> IndexMut<VSlice<T>> for PushMapCheck<'a, T> {
    fn index_mut(&mut self, key: VSlice<T>) -> &mut [T] {
        &mut self.data_mut()[key.range()]
    }
}

#[derive(PartialEq, Eq)]
pub struct PushMap<T> {
    data: Vec<T>,
}

impl<T: Relocated> Relocated for PushMap<T> {
    fn mark(&self, marker: &mut entities::FragRelocMarker) {
        self.data.mark(marker);
    }

    fn remap(&mut self, ctx: &entities::FragMarks) -> Option<()> {
        self.data.remap(ctx)
    }
}

impl<T> PushMap<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn as_view(&self) -> &PushMapView<T> {
        PushMapView::new(self, VSlice::new(0..self.data.len()))
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

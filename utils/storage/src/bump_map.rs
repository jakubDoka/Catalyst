use std::{marker::PhantomData, ops::{Index, IndexMut}};

use serde::{Deserialize, Serialize};

use crate::{VPtr, Maybe, Frames};

#[derive(Deserialize, Serialize)]
pub struct BumpMap<K, T, C = (), CACHE = ()> {
    data: Vec<T>,
    indices: Vec<u32>,
    #[serde(skip)]
    frames: CACHE,
    _ph: PhantomData<fn(K, C) -> (K, C)>
}

impl<K, T, C, CACHE: Default> BumpMap<K, T, C, CACHE> {
    pub fn new() -> Self {
        BumpMap {
            data: Vec::new(),
            indices: vec![0],
            frames: CACHE::default(),
            _ph: PhantomData
        }
    }
}

impl<K, T, C> BumpMap<K, T, C, Frames<T>> {
    pub fn cache(&mut self, value: T) {
        self.frames.push(value);
    }

    pub fn start_cache(&mut self) {
        self.frames.mark();
    }

    pub fn extend_cache(&mut self, values: impl IntoIterator<Item = T>) {
        self.frames.extend(values);
    }

    pub fn bump_cached(&mut self) -> Maybe<K> 
        where 
            K: VPtr,
            T: Clone, 
    {
        let top = self.frames.pop();
        self.data.extend(top);
        self.close_frame()
    }
}

impl<K: VPtr, T, C, CACHE> BumpMap<K, T, C, CACHE> {
    pub fn bump_slice(&mut self, items: &[T]) -> Maybe<K> 
        where 
            T: Clone 
    {
        self.bump(items.iter().cloned())
    }

    pub fn bump(&mut self, items: impl IntoIterator<Item = T>) -> Maybe<K> {
        self.data.extend(items.into_iter());
        self.close_frame()
    }

    fn close_frame(&mut self) -> Maybe<K> {
        if self.indices.last().copied() == Some(self.data.len() as u32) {
            return Maybe::none();
        }
        
        let index = self.indices.len();
        self.indices.push(self.data.len() as u32);
        Maybe::some(K::new(index))
    }

    pub fn get(&self, key: K) -> &[T] {
        &self.data[self.indices[key.index()] as usize..self.indices[key.index() + 1] as usize]
    }

    pub fn get_mut(&mut self, key: K) -> &mut [T] {
        &mut self.data[self.indices[key.index()] as usize..self.indices[key.index() + 1] as usize]
    }
}

impl<K, T, C: VPtr, CACHE> BumpMap<K, T, C, CACHE> {
    pub fn push(&mut self, value: T) -> C {
        self.data.push(value);
        C::new(self.data.len() - 1)
    }

    pub fn bump_pushed(&mut self) -> Maybe<K>
        where
            K: VPtr
    {
        self.close_frame()
    }
}

impl<K: VPtr, T, C, CACHE> Index<Maybe<K>> for BumpMap<K, T, C, CACHE> {
    type Output = [T];

    fn index(&self, index: Maybe<K>) -> &Self::Output {
        if let Some(index) = index.expand() {
            self.get(index)
        } else {
            &[]
        }
    }
}

impl<K: VPtr, T, C, CACHE> IndexMut<Maybe<K>> for BumpMap<K, T, C, CACHE> {
    fn index_mut(&mut self, index: Maybe<K>) -> &mut Self::Output {
        if let Some(index) = index.expand() {
            self.get_mut(index)
        } else {
            &mut []
        }
    }
}

impl<K, T, C: VPtr, CACHE> Index<C> for BumpMap<K, T, C, CACHE> {
    type Output = T;

    fn index(&self, index: C) -> &Self::Output {
        &self.data[index.index()]
    }
}

impl<K, T, C: VPtr, CACHE> IndexMut<C> for BumpMap<K, T, C, CACHE> {
    fn index_mut(&mut self, index: C) -> &mut Self::Output {
        &mut self.data[index.index()]
    }
}

impl<K, T, C, CACHE: Default> Default for BumpMap<K, T, C, CACHE> {
    fn default() -> Self {
        BumpMap::new()
    }
}
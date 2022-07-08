use std::{marker::PhantomData, ops::{Index, IndexMut}};

use serde::{Deserialize, Serialize};

use crate::{VPtr, Maybe, Frames};

pub type CacheBumpMap<K, T, C = ()> = BumpMap<K, T, C, Frames<T>>;

/// BumpMap allows allocating slices in a contiguous memory block.
/// If us use '[`Frames`]<T>' as `CACHE` you are able to cache elements 
/// and push them all at once Which is useful when building tree in 
/// recursive manner. If `C` implements [`VPtr`] you are then able to 
/// address elements of slices directly.
#[derive(Deserialize, Serialize)]
pub struct BumpMap<K, T, C = (), CACHE = ()> {
    data: Vec<T>,
    indices: Vec<u32>,
    #[serde(skip)]
    frames: CACHE,
    _ph: PhantomData<fn(K, C) -> (K, C)>
}

impl<K, T, C, CACHE: Default> BumpMap<K, T, C, CACHE> {
    /// Allocates little bit of memory.
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
    /// Stores the value in temporary buffer.
    /// 
    /// # Examples
    /// ```
    /// let mut map = storage::CacheBumpMap::<DummyList, usize>::new();
    /// map.start_cache();
    /// map.cache(0);
    /// map.cache(1);
    /// 
    /// let slice = map.bump_cached();
    /// 
    /// assert_eq!(&map[slice], &[0, 1]);
    /// 
    /// storage::gen_v_ptr!(DummyList);
    /// ```
    pub fn cache(&mut self, value: T) {
        self.frames.push(value);
    }

    /// Discards current frame on the cache. and returns iterator of 
    /// moved elements.
    /// 
    /// # Examples
    /// ```
    /// let mut map = storage::CacheBumpMap::<DummyList, usize>::new();
    /// map.start_cache();
    /// map.start_cache();
    /// map.cache(0);
    /// 
    /// assert_eq!(map.discard_cache().collect::<Vec<_>>(), vec![0]);
    /// assert_eq!(map.bump_cached(), storage::Maybe::none());
    /// 
    /// storage::gen_v_ptr!(DummyList);
    /// ```
    pub fn discard_cache(&mut self) -> impl Iterator<Item = T> + '_ {
        self.frames.pop()
    }

    /// Starts a new cache frame, [`Self::bump_cache`] will push only 
    /// elements cached since this call. Second call will push previous 
    /// elements and so on.
    /// 
    /// # Examples
    /// ```
    /// let mut map = storage::CacheBumpMap::<DummyList, usize>::new();
    /// map.start_cache();
    /// map.cache(0);
    /// map.cache(1);
    /// map.start_cache();
    /// map.extend_cache([2, 3, 4]);
    /// let slice1 = map.bump_cached();
    /// map.cache(5);
    /// let slice2 = map.bump_cached();
    /// 
    /// assert_eq!(&map[slice1], &[2, 3, 4]);
    /// assert_eq!(&map[slice2], &[0, 1, 5]);
    /// 
    /// storage::gen_v_ptr!(DummyList);
    /// ```
    pub fn start_cache(&mut self) {
        self.frames.mark();
    }

    /// Performs bulk push of `values`.
    /// 
    /// # Examples
    /// ```
    /// let mut map = storage::CacheBumpMap::<DummyList, usize>::new();
    /// map.start_cache();
    /// map.extend_cache([0, 1, 2]);
    /// 
    /// assert_eq!(map.discard_cache().collect::<Vec<_>>(), vec![0, 1, 2]);
    /// 
    /// storage::gen_v_ptr!(DummyList);
    /// ```
    pub fn extend_cache(&mut self, values: impl IntoIterator<Item = T>) {
        self.frames.extend(values);
    }

    /// Construct a slice from top frame of the cache.
    /// 
    /// # Examples
    /// ```
    /// let mut map = storage::CacheBumpMap::<DummyList, usize>::new();
    /// map.start_cache();
    /// map.extend_cache([0, 1, 2]);
    /// 
    /// let slice = map.bump_cached();
    /// 
    /// assert_eq!(&map[slice], &[0, 1, 2]);
    /// 
    /// storage::gen_v_ptr!(DummyList);
    /// ```
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
    /// Allocates slice inside the [`BumpMap`] and returns [`VPtr`] to it.
    /// 
    /// # Examples
    /// ```
    /// let mut map = storage::BumpMap::<DummyList, usize>::new();
    /// 
    /// let slice = map.bump_slice(&[0, 1, 2]);
    /// 
    /// assert_eq!(&map[slice], &[0, 1, 2]);
    ///
    /// storage::gen_v_ptr!(DummyList);
    /// ```
    pub fn bump_slice(&mut self, items: &[T]) -> Maybe<K> 
        where 
            T: Clone 
    {
        self.bump(items.iter().cloned())
    }

    /// Allocate an iterator inside the [`BumpMap`] and returns [`VPtr`] to it.
    /// 
    /// # Examples
    /// ```
    /// let mut map = storage::BumpMap::<DummyList, usize>::new();
    /// let mut vec = vec![0, 1, 2];
    /// 
    /// let slice = map.bump(vec.drain(..));
    /// 
    /// assert_eq!(&map[slice], &[0, 1, 2]);
    /// 
    /// storage::gen_v_ptr!(DummyList);
    /// ```
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

    /// Returns slice corresponding to given `key`. The slice is known to not be empty.
    /// 
    /// # Examples
    /// ```
    /// let mut map = storage::BumpMap::<DummyList, usize>::new();
    /// let slice = map.bump_slice(&[0, 1, 2]).unwrap();
    /// 
    /// assert_eq!(map.get(slice), &[0, 1, 2]);
    /// 
    /// storage::gen_v_ptr!(DummyList);
    /// ```
    pub fn get(&self, key: K) -> &[T] {
        &self.data[self.indices[key.index() - 1] as usize..self.indices[key.index()] as usize]
    }

    /// Returns mut slice corresponding to given `key`. The slice is known to not be empty.
    pub fn get_mut(&mut self, key: K) -> &mut [T] {
        &mut self.data[self.indices[key.index() - 1] as usize..self.indices[key.index()] as usize]
    }
}

impl<K, T, C: VPtr, CACHE> BumpMap<K, T, C, CACHE> {
    /// Pushes one element to the [`BumpMap`] and returns direct [`VPtr`] to it.
    ///
    /// # Examples
    /// ```
    /// let mut map = storage::BumpMap::<DummyList, usize, Dummy>::new();
    /// 
    /// let zero = map.push(0);
    /// 
    /// assert_eq!(map[zero], 0);
    /// 
    /// storage::gen_v_ptr!(DummyList Dummy);
    /// ```
    pub fn push(&mut self, value: T) -> C {
        self.data.push(value);
        C::new(self.data.len() - 1)
    }

    /// Discards pushed elements and returns ownership to them to the caller via iterator.
    pub fn discard_pushed(&mut self) -> impl Iterator<Item = T> + '_ {
        self.data.drain(*self.indices.last().unwrap() as usize..)
    }

    /// Bumps pushed elements and returns [`VPtr`] to them. Elements that were bumped 
    /// cannot be discarded anymore.
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
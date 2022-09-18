use std::{
    intrinsics::transmute,
    iter::repeat,
    mem::MaybeUninit,
    ops::{Index, IndexMut, Not, Range},
};

use serde::{Deserialize, Serialize};

use crate::*;

pub type CacheBumpMap<T> = BumpMap<T, Frames<T>>;

/// BumpMap allows allocating slices in a contiguous memory block.
/// If us use '[`Frames`]<T>' as `CACHE` you are able to cache elements
/// and push them all at once Which is useful when building tree in
/// recursive manner. If `VRef<T>` implements [`VPtr`] you are then able to
/// address elements of slices directly.
pub struct BumpMap<T, CACHE = ()> {
    data: Vec<MaybeUninit<T>>,
    indices: Vec<u32>,
    frames: CACHE,
    reserves_in_progress: usize,
}

impl<T, CACHE: Default> BumpMap<T, CACHE> {
    /// Allocates little bit of memory.
    pub fn new() -> Self {
        BumpMap {
            data: Vec::new(),
            indices: vec![0],
            frames: CACHE::default(),
            reserves_in_progress: 0,
        }
    }

    pub fn clear(&mut self)
    where
        CACHE: Clear,
    {
        self.data.clear();
        self.indices.truncate(1);
        self.frames.clear();
    }
}

impl<T> BumpMap<T, Frames<T>> {
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

    /// Returns amount of frames in cache.
    pub fn cache_frame_count(&self) -> usize {
        self.frames.len()
    }

    /// Returns top frame in cache.
    ///
    /// # Examples
    /// ```
    /// let mut map = storage::CacheBumpMap::<DummyList, usize>::new();
    /// map.start_cache();
    /// map.cache(0);
    /// map.cache(1);
    /// map.cache(2);
    ///
    /// assert_eq!(map.top_frame(), &[0, 1, 2]);
    ///
    /// storage::gen_v_ptr!(DummyList);
    /// ```
    pub fn cached(&self) -> &[T] {
        self.frames.top()
    }

    /// Joins first two cache frames.
    ///
    /// # Examples
    /// ```
    /// let mut map = storage::CacheBumpMap::<DummyList, usize>::new();
    /// map.start_cache();
    /// map.cache(0);
    /// map.cache(1);
    /// map.cache(2);
    /// map.start_cache();
    /// map.cache(3);
    /// map.cache(4);
    /// map.join_cache_frames();
    ///
    /// assert_eq!(map.cached(), &[0, 1, 2, 3, 4]);
    ///
    /// storage::gen_v_ptr!(DummyList);
    /// ```
    pub fn join_cache_frames(&mut self) {
        self.frames.join_frames();
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

    /// Splits current frame into two frames where top frame has `top_frame_length`.
    /// # Examples
    /// ```
    /// let mut map = storage::CacheBumpMap::<DummyList, usize>::new();
    /// map.start_cache();
    /// map.cache(0);
    /// map.cache(1);
    /// map.cache(2);
    /// map.cache(3);
    ///
    /// map.split_cache_at(2);
    ///
    /// assert_eq!(map.cached(), &[2, 3]);
    ///
    /// storage::gen_v_ptr!(DummyList);
    /// ```
    pub fn split_cache_at(&mut self, top_frame_length: usize) -> &T {
        self.frames.split_at(top_frame_length)
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
    pub fn bump_cached(&mut self) -> VSlice<T> {
        let top = self.frames.pop();
        self.data.extend(top.map(MaybeUninit::new));
        self.close_frame()
    }

    /// Clones and strips frames from the bump map.
    pub fn without_frames(&self) -> BumpMap<T, ()>
    where
        T: Clone,
    {
        assert_eq!(self.reserves_in_progress, 0);
        BumpMap {
            // SAFETY: this is safe since bump map in not in gc mode.
            data: unsafe { transmute(transmute::<_, &Vec<T>>(&self.data).clone()) },
            indices: self.indices.clone(),
            frames: (),
            reserves_in_progress: 0,
        }
    }
}

impl<T, CACHE> BumpMap<T, CACHE> {
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
    pub fn bump_slice(&mut self, items: &[T]) -> VSlice<T>
    where
        T: Clone,
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
    pub fn bump(&mut self, items: impl IntoIterator<Item = T>) -> VSlice<T> {
        self.data.extend(items.into_iter().map(MaybeUninit::new));
        self.close_frame()
    }

    fn close_frame(&mut self) -> VSlice<T> {
        if self.indices.last().copied() == Some(self.data.len() as u32) {
            return VSlice::empty();
        }

        let index = self.indices.len();
        self.indices.push(self.data.len() as u32);
        unsafe { VSlice::new(index) }
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
    pub fn get(&self, key: VSlice<T>) -> &[T] {
        unsafe { transmute(&self.data[self.range_of(key)]) }
    }

    /// Returns mut slice corresponding to given `key`. The slice is known to not be empty.
    pub fn get_mut(&mut self, key: VSlice<T>) -> &mut [T] {
        unsafe { transmute(self.get_mut_uninit(key)) }
    }

    pub unsafe fn get_mut_uninit(&mut self, key: VSlice<T>) -> &mut [MaybeUninit<T>] {
        let range = self.range_of(key);
        &mut self.data[range]
    }

    pub fn range_of(&self, key: VSlice<T>) -> Range<usize> {
        if key.is_empty() {
            return 0..0;
        }

        let start = self.indices[key.index() - 1] as usize;
        let end = self.indices[key.index()] as usize;
        start..end
    }
}

impl<T, CACHE> BumpMap<T, CACHE> {
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
    pub fn push(&mut self, value: T) -> VRef<T> {
        self.data.push(MaybeUninit::new(value));
        unsafe { VRef::new(self.data.len() - 1) }
    }

    /// Discards pushed elements and returns ownership to them to the caller via iterator.
    pub fn discard_pushed(&mut self) -> impl Iterator<Item = T> + '_ {
        self.data
            .drain(*self.indices.last().unwrap() as usize..)
            .map(|i| unsafe { i.assume_init() })
    }

    /// Bumps pushed elements and returns [`VPtr`] to them. Elements that were bumped
    /// cannot be discarded anymore.
    pub fn bump_pushed(&mut self) -> VSlice<T> {
        self.close_frame()
    }

    /// Shorthand for pushing one element and immediately bumping it.
    pub fn bump_push(&mut self, value: T) -> (VRef<T>, VSlice<T>) {
        (self.push(value), self.bump_pushed())
    }

    /// Uses binary search to determinate in which frame the given `key` is located.
    pub fn slice_of(&self, concrete: VRef<T>) -> VSlice<T> {
        let index = concrete.index();
        let result = self
            .indices
            .binary_search(&(index as u32))
            .map(|i| i + 1)
            .unwrap_or_else(|i| i);
        unsafe { VSlice::new(result) }
    }

    /// Reserves a slice of uninitialized memory that is supposed to be filled later.
    pub fn reserve(&mut self, size: usize) -> Reserved<T>
    where
        T: Clone,
    {
        let start = self.data.len();
        self.data
            .extend(repeat(()).map(|_| MaybeUninit::uninit()).take(size));
        let id = self.close_frame();
        self.reserves_in_progress += 1;
        Reserved {
            start,
            end: start + size,
            id,
        }
    }

    pub fn indexed(&self, key: VSlice<T>) -> impl Iterator<Item = (VRef<T>, &T)> {
        key.is_empty()
            .not()
            .then_some(key)
            .map(|k| self.range_of(k))
            .map(|range| {
                range
                    .clone()
                    .map(|i| unsafe { VRef::new(i) })
                    .zip(unsafe { transmute::<_, &[T]>(&self.data[range]) })
            })
            .into_iter()
            .flatten()
    }

    /// Pushes one element to reserved slice.
    ///
    /// # Panics
    ///
    /// Panics if `reserved` is already filled.
    pub fn push_to_reserved(&mut self, reserved: &mut Reserved<T>, value: T) -> VRef<T> {
        assert_ne!(reserved.end, reserved.start);
        self.data[reserved.start] = MaybeUninit::new(value);
        let key = unsafe { VRef::new(reserved.start) };
        reserved.start += 1;
        key
    }

    pub fn reserve_len(&self, reserved: &Reserved<T>) -> usize {
        self[reserved.id].len() - (reserved.end - reserved.start)
    }

    /// Finalizes the reserved slice and return valid [`VPtr`] to it.
    ///
    /// # Panics
    ///
    /// Panics if `reserved` is not filled.
    pub fn finish_reserved(&mut self, reserved: Reserved<T>) -> VSlice<T> {
        assert_eq!(reserved.end, reserved.start);
        self.reserves_in_progress -= 1;
        reserved.id
    }

    /// Other way of finalizing a reserved slice if filling all itr remaining elements with [`Clone`] value.
    pub fn fill_reserved(&mut self, mut reserved: Reserved<T>, value: T) -> VSlice<T>
    where
        T: Clone,
    {
        self.data[reserved.start..reserved.end]
            .iter_mut()
            .for_each(|i| *i = MaybeUninit::new(value.clone()));
        reserved.start = reserved.end;
        self.finish_reserved(reserved)
    }

    pub fn reserved_view(&self, reserved: &Reserved<T>) -> &[T] {
        &self[reserved.id][..reserved.start]
    }
}

impl<T: Clone> Clone for BumpMap<T> {
    fn clone(&self) -> Self {
        assert_eq!(self.reserves_in_progress, 0);
        Self {
            data: unsafe { transmute(transmute::<_, &Vec<T>>(&self.data).clone()) },
            indices: self.indices.clone(),
            frames: (),
            reserves_in_progress: 0,
        }
    }
}

impl<T, CACHE> Drop for BumpMap<T, CACHE> {
    fn drop(&mut self) {
        if !std::thread::panicking() {
            assert_eq!(self.reserves_in_progress, 0);
        }
    }
}

impl<T, CACHE> Index<VSlice<T>> for BumpMap<T, CACHE> {
    type Output = [T];

    fn index(&self, index: VSlice<T>) -> &Self::Output {
        self.get(index)
    }
}

impl<T, CACHE> IndexMut<VSlice<T>> for BumpMap<T, CACHE> {
    fn index_mut(&mut self, index: VSlice<T>) -> &mut Self::Output {
        self.get_mut(index)
    }
}

impl<T, CACHE> Index<VRef<T>> for BumpMap<T, CACHE> {
    type Output = T;

    fn index(&self, index: VRef<T>) -> &Self::Output {
        unsafe { &*self.data[index.index()].as_ptr() }
    }
}

impl<T, CACHE> IndexMut<VRef<T>> for BumpMap<T, CACHE> {
    fn index_mut(&mut self, index: VRef<T>) -> &mut Self::Output {
        unsafe { &mut *self.data[index.index()].as_mut_ptr() }
    }
}

impl<T, CACHE: Default> Default for BumpMap<T, CACHE> {
    fn default() -> Self {
        BumpMap::new()
    }
}

impl<T: Serialize, CACHE> Serialize for BumpMap<T, CACHE> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        assert_eq!(self.reserves_in_progress, 0);
        unsafe { transmute::<_, &SerializableBumpMap<T, CACHE>>(self) }.serialize(serializer)
    }
}

impl<'de, T: Deserialize<'de>, CACHE: Default> Deserialize<'de> for BumpMap<T, CACHE> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        SerializableBumpMap::<T, CACHE>::deserialize(deserializer).map(|s| BumpMap {
            data: unsafe { transmute(s.data) },
            indices: s.indices,
            frames: CACHE::default(),
            reserves_in_progress: 0,
        })
    }
}

#[derive(Serialize, Deserialize)]
struct SerializableBumpMap<T, CACHE = ()> {
    data: Vec<T>,
    indices: Vec<u32>,
    #[serde(skip)]
    _frames: CACHE,
    _padding: usize,
}

pub struct Reserved<T> {
    start: usize,
    end: usize,
    id: VSlice<T>,
}

impl<T> Reserved<T> {
    pub fn finished(&self) -> bool {
        self.start == self.end
    }
}

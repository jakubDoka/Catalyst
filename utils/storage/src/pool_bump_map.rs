use std::{
    mem::{replace, MaybeUninit},
    ops::{Deref, DerefMut, Index, IndexMut},
};

use crate::{BumpMap, Frames, Maybe, PoolMap, VPtr, VPtrSet};

pub type CachedPoolBumpMap<K, T, C = ()> = PoolBumpMap<K, T, C, Frames<T>>;

/// Struct is similar to [`PoolMap`] as it allows removing elements
/// but it stores slices of T as [`BumpMap`]. Accessing removed memory
/// results into panic. Its is considered `unsafe` to explicitly call
/// [`Deref`] or [`DerefMut`] on this struct.
pub struct PoolBumpMap<K, T, C = (), CACHE = ()> {
    inner: BumpMap<K, T, C, CACHE>,
    heads: Vec<Maybe<Private>>,
    freed: PoolMap<Private, (K, Maybe<Private>)>,
    free_lookup: VPtrSet<K>,
    tmp: Vec<T>,
}

impl<K, T, C, CACHE: Default> PoolBumpMap<K, T, C, CACHE> {
    /// Will allocate little bit of memory.
    pub fn new() -> Self {
        Self {
            inner: BumpMap::new(),
            heads: Vec::new(),
            freed: PoolMap::new(),
            free_lookup: VPtrSet::new(),
            tmp: Vec::new(),
        }
    }
}

impl<K: VPtr, T, C, CACHE> PoolBumpMap<K, T, C, CACHE> {
    /// Same behavior as [`BumpMap::bump_slice`] but removed slices can be reused.
    ///
    /// # Example
    /// ```
    /// let mut map = storage::PoolBumpMap::<DummyList, usize>::new();
    ///
    /// let slice = map.bump_slice(&[10, 20, 30]);
    /// assert_eq!(&map[slice], &[10, 20, 30]);
    /// assert_eq!(map.remove(slice).collect::<Vec<_>>(), vec![10, 20, 30]);
    ///
    /// let restored_slice = map.bump_slice(&[10, 20, 30]);
    /// assert_eq!(slice, restored_slice); // reused
    ///
    /// storage::gen_v_ptr!(DummyList);
    /// ```
    pub fn bump_slice(&mut self, slice: &[T]) -> Maybe<K>
    where
        T: Clone,
    {
        self.bump(slice.iter().cloned())
    }

    /// Same behavior as [`BumpMap::bump`] but removed slices can be reused.
    /// See [`Self::bump_slice`] for more example.
    pub fn bump(&mut self, items: impl IntoIterator<Item = T>) -> Maybe<K> {
        self.tmp.extend(items);
        self.bump_prepared()
    }

    fn bump_prepared(&mut self) -> Maybe<K> {
        if let Some(Some(head)) = self.heads.get(self.tmp.len()).map(|v| v.expand()) {
            let (id, next) = self.freed.remove(head);
            assert!(self.free_lookup.remove(id));
            self.heads[self.tmp.len()] = next;
            self.inner
                .get_mut(id)
                .iter_mut()
                .zip(self.tmp.drain(..))
                .for_each(|(a, b)| *a = b);
            Maybe::some(id)
        } else {
            self.inner.bump(self.tmp.drain(..))
        }
    }

    /// Same behavior as [`BumpMap::get`] but validity of `key` is checked.
    pub fn get(&self, key: K) -> &[T] {
        assert!(!self.free_lookup.contains(key));
        self.inner.get(key)
    }

    /// Same behavior as [`BumpMap::get_mut`] but validity of `key` is checked.
    pub fn get_mut(&mut self, key: K) -> &mut [T] {
        assert!(!self.free_lookup.contains(key));
        self.inner.get_mut(key)
    }
}

impl<K, T, C: VPtr, CACHE> PoolBumpMap<K, T, C, CACHE> {
    /// Same behavior as [`BumpMap::bump_pushed`] but removed slices can be reused.
    pub fn bump_pushed(&mut self) -> Maybe<K>
    where
        K: VPtr,
    {
        self.tmp.extend(self.inner.discard_pushed());
        self.bump_prepared()
    }
}

impl<K: VPtr, T, C, CACHE> PoolBumpMap<K, T, C, CACHE> {
    /// Removes slice from the [`PoolBumpMap`] and returns ownership to slice
    /// contents via iterator. The backing memory is saved for next allocations.
    ///
    /// # Examples
    /// ```
    /// let mut map = storage::PoolBumpMap::<DummyList, usize>::new();
    /// let slice1 = map.bump([10, 20, 30]);
    /// let slice2 = map.bump([40, 50, 60]);
    ///
    /// assert_eq!(map.remove(slice2.unwrap()).collect::<Vec<_>>(), vec![40, 50, 60]);
    /// assert_eq!(map.remove(slice1.unwrap()).collect::<Vec<_>>(), vec![10, 20, 30]);
    /// assert_eq!(map.bump([0, 0, 0]), slice2);
    /// assert_eq!(map.bump([0, 0, 0]), slice1);
    ///
    /// storage::gen_v_ptr!(DummyList);
    /// ```
    pub fn remove(&mut self, key: K) -> impl Iterator<Item = T> + '_ {
        assert!(self.free_lookup.insert(key));

        let size = self.get(key).len();
        let new_size = self.heads.len().max(size + 1);
        self.heads.resize(new_size, Maybe::none());

        let freed = self.freed.push((key, self.heads[size]));
        self.heads[size] = Maybe::some(freed);

        unsafe {
            self.get_mut_uninit(key)
                .iter_mut()
                .map(|v| replace(v, MaybeUninit::uninit()).assume_init())
        }
    }

    /// Same behavior as [`Self::remove`] but removal is optional.
    pub fn may_remove(&mut self, key: Maybe<K>) -> impl Iterator<Item = T> + '_ {
        key.expand()
            .map(|key| self.remove(key))
            .into_iter()
            .flatten()
    }

    fn check_index(&self, concrete: C) -> bool
    where
        C: VPtr,
    {
        self.free_lookup.contains(self.inner.slice_of(concrete))
    }
}

impl<K: VPtr, T, C> PoolBumpMap<K, T, C, Frames<T>> {
    /// Same behavior as [`BumpMap::bump_cached`] but removed slices can be reused.
    pub fn bump_cached(&mut self) -> Maybe<K> {
        self.tmp.extend(self.inner.discard_cache());
        self.bump_prepared()
    }
}

impl<K, T, C, CACHE: Default> Default for PoolBumpMap<K, T, C, CACHE> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K, T, C, CACHE> Deref for PoolBumpMap<K, T, C, CACHE> {
    type Target = BumpMap<K, T, C, CACHE>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<K, T, C, CACHE> DerefMut for PoolBumpMap<K, T, C, CACHE> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<K: VPtr, T, C, CACHE> Index<Maybe<K>> for PoolBumpMap<K, T, C, CACHE> {
    type Output = [T];

    fn index(&self, index: Maybe<K>) -> &Self::Output {
        if let Some(index) = index.expand() {
            self.get(index)
        } else {
            &[]
        }
    }
}

impl<K: VPtr, T, C, CACHE> IndexMut<Maybe<K>> for PoolBumpMap<K, T, C, CACHE> {
    fn index_mut(&mut self, index: Maybe<K>) -> &mut Self::Output {
        if let Some(index) = index.expand() {
            self.get_mut(index)
        } else {
            &mut []
        }
    }
}

impl<K: VPtr, T, C: VPtr, CACHE> Index<C> for PoolBumpMap<K, T, C, CACHE> {
    type Output = T;

    fn index(&self, index: C) -> &Self::Output {
        assert!(!self.check_index(index));
        &self.inner[index]
    }
}

impl<K: VPtr, T, C: VPtr, CACHE> IndexMut<C> for PoolBumpMap<K, T, C, CACHE> {
    fn index_mut(&mut self, index: C) -> &mut Self::Output {
        assert!(!self.check_index(index));
        &mut self.inner[index]
    }
}

gen_v_ptr!(Private);

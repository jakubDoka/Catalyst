use std::{
    mem::{replace, MaybeUninit},
    ops::{Deref, DerefMut, Index, IndexMut},
};

use crate::*;

pub type CachedPoolBumpMap<T> = PoolBumpMap<T, Frames<T>>;

struct FreeSlot<T> {
    id: VSlice<T>,
    next: Maybe<VRef<FreeSlot<T>>>,
}

/// Struct is similar to [`PoolMap`] as it allows removing elements
/// but it stores slices of T as [`BumpMap`]. Accessing removed memory
/// results into panic. Its is considered `unsafe` to explicitly call
/// [`Deref`] or [`DerefMut`] on this struct.
pub struct PoolBumpMap<T, CACHE = ()> {
    inner: BumpMap<T, CACHE>,
    heads: Vec<Maybe<VRef<FreeSlot<T>>>>,
    freed: PoolMap<FreeSlot<T>>,
    free_lookup: BitSet,
    tmp: Vec<T>,
}

impl<T, CACHE: Default> PoolBumpMap<T, CACHE> {
    /// Will allocate little bit of memory.
    pub fn new() -> Self {
        Self {
            inner: BumpMap::new(),
            heads: Vec::new(),
            freed: PoolMap::new(),
            free_lookup: BitSet::new(),
            tmp: Vec::new(),
        }
    }
}

impl<T, CACHE> PoolBumpMap<T, CACHE> {
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
    pub fn bump_slice(&mut self, slice: &[T]) -> VSlice<T>
    where
        T: Clone,
    {
        self.bump(slice.iter().cloned())
    }

    /// Same behavior as [`BumpMap::bump`] but removed slices can be reused.
    /// See [`Self::bump_slice`] for more example.
    pub fn bump(&mut self, items: impl IntoIterator<Item = T>) -> VSlice<T> {
        self.tmp.extend(items);
        self.bump_prepared()
    }

    fn bump_prepared(&mut self) -> VSlice<T> {
        if let Some(Some(head)) = self.heads.get(self.tmp.len()).map(|v| v.expand()) {
            let FreeSlot { id, next } = self.freed.remove(head);
            assert!(self.free_lookup.remove(id.index()));
            self.heads[self.tmp.len()] = next;
            self.inner
                .get_mut(id)
                .iter_mut()
                .zip(self.tmp.drain(..))
                .for_each(|(a, b)| *a = b);
            id
        } else {
            self.inner.bump(self.tmp.drain(..))
        }
    }

    /// Same behavior as [`BumpMap::get`] but validity of `key` is checked.
    pub fn get(&self, key: VSlice<T>) -> &[T] {
        assert!(!self.free_lookup.contains(key.index()));
        self.inner.get(key)
    }

    /// Same behavior as [`BumpMap::get_mut`] but validity of `key` is checked.
    pub fn get_mut(&mut self, key: VSlice<T>) -> &mut [T] {
        assert!(!self.free_lookup.contains(key.index()));
        self.inner.get_mut(key)
    }
}

impl<T, CACHE> PoolBumpMap<T, CACHE> {
    /// Same behavior as [`BumpMap::bump_pushed`] but removed slices can be reused.
    pub fn bump_pushed(&mut self) -> VSlice<T> {
        self.tmp.extend(self.inner.discard_pushed());
        self.bump_prepared()
    }
}

impl<T, CACHE> PoolBumpMap<T, CACHE> {
    /// Removes slice from the [`PoolBumpMap`] and returns ownership to slice
    /// contents via iterator. The backing memory is saved for next allocations.
    ///
    /// # Examples
    /// ```
    /// let mut map = storage::PoolBumpMap::<DummyList, usize>::new();
    /// let slice1 = map.bump([10, 20, 30]);
    /// let slice2 = map.bump([40, 50, 60]);
    ///
    /// assert_eq!(map.remove(slice2.unwrap()).collect::<Vec<_>>(), to_bumpvec[40, 50, 60]);
    /// assert_eq!(map.remove(slice1.unwrap()).collect::<Vec<_>>(), vec![10, 20, 30]);
    /// assert_eq!(map.bump([0, 0, 0]), slice2);
    /// assert_eq!(map.bump([0, 0, 0]), slice1);
    ///
    /// storage::gen_v_ptr!(DummyList);
    /// ```
    pub fn remove(&mut self, key: VSlice<T>) -> impl Iterator<Item = T> + '_ {
        if key.is_empty() {
            return None.into_iter().flatten();
        }

        assert!(self.free_lookup.insert(key.index()));

        let size = self.get(key).len();
        let new_size = self.heads.len().max(size + 1);
        self.heads.resize(new_size, Maybe::none());

        let freed = self.freed.push(FreeSlot {
            id: key,
            next: self.heads[size],
        });
        self.heads[size] = Maybe::some(freed);

        unsafe {
            Some(
                self.get_mut_uninit(key)
                    .iter_mut()
                    .map(|v| replace(v, MaybeUninit::uninit()).assume_init()),
            )
            .into_iter()
            .flatten()
        }
    }

    fn check_index(&self, concrete: VRef<T>) -> bool {
        self.free_lookup
            .contains(self.inner.slice_of(concrete).index())
    }
}

impl<T> PoolBumpMap<T, Frames<T>> {
    /// Same behavior as [`BumpMap::bump_cached`] but removed slices can be reused.
    pub fn bump_cached(&mut self) -> VSlice<T> {
        self.tmp.extend(self.inner.discard_cache());
        self.bump_prepared()
    }
}

impl<T, CACHE: Default> Default for PoolBumpMap<T, CACHE> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T, CACHE> Deref for PoolBumpMap<T, CACHE> {
    type Target = BumpMap<T, CACHE>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T, CACHE> DerefMut for PoolBumpMap<T, CACHE> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<T, CACHE> Index<VSlice<T>> for PoolBumpMap<T, CACHE> {
    type Output = [T];

    fn index(&self, index: VSlice<T>) -> &Self::Output {
        self.get(index)
    }
}

impl<T, CACHE> IndexMut<VSlice<T>> for PoolBumpMap<T, CACHE> {
    fn index_mut(&mut self, index: VSlice<T>) -> &mut Self::Output {
        self.get_mut(index)
    }
}

impl<T, CACHE> Index<VRef<T>> for PoolBumpMap<T, CACHE> {
    type Output = T;

    fn index(&self, index: VRef<T>) -> &Self::Output {
        assert!(!self.check_index(index));
        &self.inner[index]
    }
}

impl<T, CACHE> IndexMut<VRef<T>> for PoolBumpMap<T, CACHE> {
    fn index_mut(&mut self, index: VRef<T>) -> &mut Self::Output {
        assert!(!self.check_index(index));
        &mut self.inner[index]
    }
}

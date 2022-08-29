use serde::{Deserialize, Serialize};

const WIDTH: usize = std::mem::size_of::<usize>() * 8;
const WIDTH_POW: usize = WIDTH.ilog2() as usize;

/// VPtrSet is storage plugin. It stores extra boolean for each [`VPtr`] with 8x efficiency
/// compared to '[`Vec`]<[`bool`]>'. It also offers same speed (vector element is of [`usize`])
/// as cpu would have to perform bit-shift to retrieve boolean value anyway.
#[derive(Serialize, Deserialize)]
pub struct BitSet {
    data: Vec<usize>,
    len: usize,
}

impl BitSet {
    /// No allocations performed.
    pub fn new() -> Self {
        BitSet {
            data: Vec::new(),
            len: 0,
        }
    }

    pub fn with_capacity(free_len: usize) -> Self {
        Self {
            data: Vec::with_capacity((free_len + WIDTH - (free_len & (WIDTH - 1))) << WIDTH_POW),
            len: 0,
        }
    }
}

impl BitSet {
    /// Returns true if given [`VPtr`] is in set.
    ///
    /// # Examples
    /// ```
    /// let mut set = storage::VPtrSet::new();
    /// set.insert(Dummy(0));
    ///
    /// assert!(set.contains(Dummy(0)));
    /// assert!(!set.contains(Dummy(1)));
    ///
    /// storage::gen_v_ptr!(Dummy);
    /// ```
    pub fn contains(&self, key: usize) -> bool {
        let (global, local) = Self::decompose_key(key);

        self.data
            .get(global)
            .map(|&x| x & (1 << local) != 0)
            .unwrap_or(false)
    }

    /// Adds given [`VPtr`] to set and returns tru if set changed.
    ///
    /// # Example
    /// ```
    /// let mut set = storage::VPtrSet::new();
    ///
    /// assert!(set.insert(Dummy(0)));
    /// assert_eq!(set.raw()[0], 1);
    /// assert!(!set.insert(Dummy(0)));
    ///
    /// storage::gen_v_ptr!(Dummy);
    /// ```
    pub fn insert(&mut self, key: usize) -> bool {
        let (global, local) = Self::decompose_key(key);

        let size = (global + 1).max(self.data.len());
        self.len = self.len.max(key + 1);
        self.data.resize(size, 0);

        let entry = &mut self.data[global];
        let prev = *entry;
        *entry |= 1 << local;

        *entry != prev
    }

    /// Removes given [`VPtr`] from set and returns true if set changed.
    ///
    /// # Example
    /// ```
    /// let mut set = storage::VPtrSet::new();
    /// set.insert(Dummy(0));
    ///
    /// assert!(set.remove(Dummy(0)));
    /// assert_eq!(set.raw()[0], 0);
    /// assert!(!set.remove(Dummy(0)));
    ///
    /// storage::gen_v_ptr!(Dummy);
    /// ```
    pub fn remove(&mut self, key: usize) -> bool {
        let (global, local) = Self::decompose_key(key);

        let mut dummy = 0;
        let entry = self.data.get_mut(global).unwrap_or(&mut dummy);
        let prev = *entry;
        *entry &= !(1 << local);

        *entry != prev
    }

    /// Returns underlying data of VPtrSet.
    pub fn raw(&self) -> &[usize] {
        &self.data
    }

    fn decompose_key(index: usize) -> (usize, usize) {
        // SAFETY: this works since WIDTH is a power of 2.
        (index >> WIDTH_POW, index & (WIDTH - 1))
    }

    pub fn size_hint(&self) -> usize {
        self.len
    }
}

use bytecheck::CheckBytes;
use rkyv::{Archive, Deserialize, Serialize};

const WIDTH: usize = std::mem::size_of::<usize>() * 8;
const WIDTH_POW: usize = WIDTH.ilog2() as usize;

/// VPtrSet is storage plugin. It stores extra boolean for each [`VPtr`] with 8x efficiency
/// compared to '[`Vec`]<[`bool`]>'. It also offers same speed (vector element is of [`usize`])
/// as cpu would have to perform bit-shift to retrieve boolean value anyway.

#[derive(Archive, Serialize, Deserialize, Default, Clone)]
#[archive_attr(derive(CheckBytes))]
pub struct BitSet {
    data: Vec<usize>,
    len: usize,
}

impl BitSet {
    /// No allocations performed.
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_capacity(cap: usize) -> Self {
        Self {
            data: Vec::with_capacity(Self::project_len(cap)),
            len: 0,
        }
    }

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

    pub fn decompose_key(index: usize) -> (usize, usize) {
        // SAFETY: this works since WIDTH is a power of 2.
        (index >> WIDTH_POW, index & (WIDTH - 1))
    }

    pub fn size_hint(&self) -> usize {
        self.len
    }

    pub fn clear(&mut self) {
        self.data.clear();
        self.len = 0;
    }

    pub fn truncate(&mut self, len: usize) {
        self.len = len;
        self.data.truncate(Self::project_len(len));
        if let Some(last) = self.data.last_mut() {
            *last &= Self::reminder_mask(len);
        }
    }

    pub fn project_len(len: usize) -> usize {
        let missing = WIDTH - (len & (WIDTH - 1));
        (len + (missing & (WIDTH - 1))) >> WIDTH_POW
    }

    pub fn reminder_mask(len: usize) -> usize {
        let rem = len & (WIDTH - 1);
        ((1 << rem) - 1) | 0usize.wrapping_sub((rem == 0) as usize)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn project_len_test() {
        assert_eq!(BitSet::project_len(0), 0);
        assert_eq!(BitSet::project_len(1), 1);
        assert_eq!(BitSet::project_len(WIDTH), 1);
        assert_eq!(BitSet::project_len(WIDTH + 1), 2);
    }

    #[test]
    fn reminder_mask_test() {
        assert_eq!(BitSet::reminder_mask(0), usize::MAX);
        assert_eq!(BitSet::reminder_mask(1), 1);
        assert_eq!(BitSet::reminder_mask(WIDTH), usize::MAX);
        assert_eq!(BitSet::reminder_mask(WIDTH + 1), 1);
        assert_eq!(BitSet::reminder_mask(5), 0b11111);
    }

    #[test]
    fn test_truncate() {
        let mut set = BitSet::new();
        set.insert(2);
        set.insert(3);
        set.insert(4);
        set.truncate(3);
        assert_eq!(set.raw(), &[0b100]);
    }
}

use std::{
    default::default,
    fmt::{Display, Write},
    hash::{Hash, Hasher},
    ops::{Index, Range},
};

use serde::{Deserialize, Serialize};

use crate::*;

macro_rules! gen_span_constants {
    (
        $($name:ident => $repr:literal,)*
    ) => {
        impl Interner {
            gen_increasing_constants!(exp str => $($name)*);

            fn init(&mut self) {
                $(
                    self.intern($repr);
                )*
            }
        }
    };
}

gen_span_constants! {
    EMPTY => "",
    SELF => "Self",
    EQUAL => "==",
    BAND => "bool & bool",
    ASSIGN => "=",
    CAST => "cast",
    TOKEN_MACRO => "token_macro",
    NEW => "new",
    START => "start",
    NEXT => "next",
    CLEAR => "clear",
    DROP => "drop",
    SIZEOF => "sizeof",
}

/// Struct ensures that all distinct strings are stored just once (not substrings),
/// and are assigned unique id.
pub struct Interner {
    map: Map<InternerEntry, VRef<str>>,
    entries: PushMap<InternerEntry>,
    data: Vec<Vec<u8>>,
    garbage: Vec<Vec<u8>>,
    temp: String,
}

unsafe impl Sync for Interner {}
unsafe impl Send for Interner {}

impl Interner {
    pub const CHUNK_SIZE: usize = 1024 * 1024;

    /// This does allocate very small amount of memory.
    pub fn new() -> Self {
        let mut s = Interner {
            map: Map::default(),
            entries: PushMap::default(),
            data: default(),
            garbage: Vec::new(),
            temp: String::new(),
        };
        s.init();
        s
    }

    pub fn clear(&mut self) {
        self.map.clear();
        self.garbage.append(&mut self.data);
        self.entries.clear();
        self.init();
    }

    pub fn intern_scoped(&mut self, scope: impl Display, name: VRef<str>) -> VRef<str> {
        self.intern_with(|s, t| write!(t, "{}\\{}", scope, &s[name]))
    }

    pub fn intern_with<T>(
        &mut self,
        mut builder: impl FnMut(&Self, &mut String) -> T,
    ) -> VRef<str> {
        let mut temp = std::mem::take(&mut self.temp);
        builder(self, &mut temp);
        let res = self.intern(&temp);
        temp.clear();
        self.temp = temp;
        res
    }

    /// Interns a string
    pub fn intern(&mut self, s: &str) -> VRef<str> {
        let key = InternerEntry::new(s);
        match self.map.get(&key) {
            Some(&str) => str,
            None => {
                if self
                    .data
                    .last()
                    .map_or(true, |l| l.len() + s.len() > l.capacity())
                {
                    self.grow(s.len());
                }
                let last = self.data.last_mut().unwrap();

                let start = last.len();
                last.extend_from_slice(s.as_bytes());

                let entry = unsafe { std::str::from_utf8_unchecked(&last[start..]) };
                let entry = InternerEntry::new(entry);

                let v_ref = unsafe { self.entries.push(entry).cast() };
                self.map.insert(entry, v_ref);
                v_ref
            }
        }
    }

    #[cold]
    #[inline(never)]
    fn grow(&mut self, size: usize) {
        let slice = self
            .garbage
            .pop()
            .unwrap_or_else(|| Vec::with_capacity(Self::CHUNK_SIZE.max(size)));
        self.data.push(slice);
    }

    fn to_raw(&self) -> RawInterner {
        let capacity = self.data.iter().map(|s| s.len()).sum::<usize>();
        let mut data = Vec::with_capacity(capacity);
        let mut slices = Vec::with_capacity(self.entries.len());
        let mut entries = self.entries.values().peekable();
        for slice in &self.data {
            while let Some(entry) = entries.peek() {
                let Range { start, end } = unsafe { (*(entry.str as *const [u8])).as_ptr_range() };

                let Range {
                    start: data_start,
                    end: data_end,
                } = slice.as_ptr_range();

                if start >= data_end || end < data_start {
                    break;
                }

                let start = start as usize - data_start as usize + data.len();
                let end = end as usize - data_start as usize + data.len();
                slices.push(Range { start, end });
                entries.next();
            }
            data.extend_from_slice(slice);
        }

        RawInterner { data, slices }
    }
}

impl Clone for Interner {
    fn clone(&self) -> Self {
        self.to_raw().into_interner()
    }
}

impl Default for Interner {
    fn default() -> Self {
        Interner::new()
    }
}

impl Index<VRef<str>> for Interner {
    type Output = str;

    fn index(&self, ident: VRef<str>) -> &str {
        unsafe { &*self.entries[ident.cast()].str }
    }
}

impl Serialize for Interner {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.to_raw().serialize(serializer)
    }
}

impl<'a> Deserialize<'a> for Interner {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'a>,
    {
        RawInterner::deserialize(deserializer).map(RawInterner::into_interner)
    }
}

#[derive(Deserialize, Serialize)]
struct RawInterner {
    slices: Vec<Range<usize>>,
    data: Vec<u8>,
}

impl RawInterner {
    fn into_interner(self) -> Interner {
        let RawInterner { data, slices } = self;

        let mut map = Map::with_capacity_and_hasher(slices.len(), map::FvnBuildHasher);
        let mut entries = PushMap::with_capacity(slices.len());

        for range in slices {
            let entry = InternerEntry::new(unsafe { std::str::from_utf8_unchecked(&data[range]) });
            let v_ref = unsafe { entries.push(entry).cast() };
            map.insert(entry, v_ref);
        }

        Interner {
            map,
            entries,
            data: vec![data],
            garbage: Vec::new(),
            temp: String::new(),
        }
    }
}

#[derive(Clone, Copy)]
struct InternerEntry {
    str: *const str,
}

impl Hash for InternerEntry {
    fn hash<H: Hasher>(&self, state: &mut H) {
        unsafe {
            (*self.str).hash(state);
        }
    }
}

impl PartialEq<Self> for InternerEntry {
    fn eq(&self, other: &Self) -> bool {
        unsafe { (*self.str) == (*other.str) }
    }
}

impl Eq for InternerEntry {}

impl InternerEntry {
    pub fn new(str: &str) -> InternerEntry {
        InternerEntry { str }
    }
}

impl VRefDefault for str {
    fn default_state() -> VRef<Self> {
        Interner::EMPTY
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use std::fmt::Write;

    #[test]
    fn test_interner() {
        let mut interner = Interner::new();
        assert_eq!(interner.intern("a"), interner.intern("a"));
        let b = interner.intern("b");
        assert_eq!(
            interner.intern_with(|s, t| write!(t, "c{}", &s[b]).unwrap()),
            interner.intern_with(|s, t| write!(t, "c{}", &s[b]).unwrap()),
        );
        let cb = interner.intern_with(|s, t| write!(t, "c{}", &s[b]).unwrap());
        assert_eq!(&interner[cb], "cb");
    }

    #[test]
    fn test_interner_serde() {
        let mut interner = Interner::new();
        let a = interner.intern("a");
        let b = interner.intern("b");
        let c = interner.intern("c");

        let mut buf = Vec::new();
        interner
            .serialize(&mut rmp_serde::encode::Serializer::new(&mut buf))
            .unwrap();

        let interner2 =
            Interner::deserialize(&mut rmp_serde::decode::Deserializer::new(buf.as_slice()))
                .unwrap();

        assert_eq!(&interner2[a], "a");
        assert_eq!(&interner2[b], "b");
        assert_eq!(&interner2[c], "c");
    }
}

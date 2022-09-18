use std::{
    collections::{hash_map::Entry, HashMap},
    hash::{BuildHasher, Hash, Hasher},
    ops::{Index, Range},
};

use serde::{Deserialize, Serialize};

use crate::{VRef, VRefDefault};

pub fn ident_join<'a, T: Into<InternedSegment<'a>>>(
    sep: &'a str,
    identifiers: impl IntoIterator<Item = T>,
) -> impl Iterator<Item = InternedSegment<'a>> {
    identifiers
        .into_iter()
        .flat_map(move |ident| ident!(sep, ident.into()))
        .skip(1)
}

/// Struct ensures that all distinct strings are stored just once (not substrings),
/// and are assigned unique id.
pub struct Interner {
    map: HashMap<InternerEntry, VRef<str>, InternerBuildHasher>,
    indices: Vec<usize>,
    data: Box<String>,
}

impl Interner {
    pub const EMPTY: VRef<str> = unsafe { VRef::new(1) };

    /// This does allocate very small amount of memory.
    pub fn new() -> Self {
        let mut s = Interner {
            map: HashMap::with_hasher(InternerBuildHasher),
            indices: vec![0],
            data: Box::default(),
        };
        assert_eq!(s.intern_str(""), Self::EMPTY);
        s
    }

    pub fn clear(&mut self) {
        self.map.clear();
        self.indices.clear();
        self.indices.push(0);
        self.data.clear();
        assert_eq!(self.intern_str(""), Self::EMPTY);
    }

    /// Interns a string
    pub fn intern_str(&mut self, s: &str) -> VRef<str> {
        self.intern(ident!(s))
    }

    /// Interns a composite ident. This avoids allocating memory for string.
    /// See [`ident!`] macro for ease of use and [`InternedSegment`] for more info.
    pub fn intern(&mut self, components: impl IntoIterator<Item = InternedSegment>) -> VRef<str> {
        self.intern_low(components).0
    }

    fn intern_low(
        &mut self,
        components: impl IntoIterator<Item = InternedSegment>,
    ) -> (VRef<str>, bool) {
        let prev = self.data.len();
        let entry = self.push_segments(components);
        let entry = self.map.entry(entry);
        let vacant = matches!(entry, Entry::Vacant(..));

        (
            *entry
                .and_modify(|_| self.data.truncate(prev))
                .or_insert_with(|| {
                    let index = self.indices.len();
                    self.indices.push(self.data.len());
                    unsafe { VRef::new(index) }
                }),
            vacant,
        )
    }

    fn push_segments(
        &mut self,
        components: impl IntoIterator<Item = InternedSegment>,
    ) -> InternerEntry {
        use std::fmt::Write;
        let start = self.data.len();
        for component in components {
            match component {
                InternedSegment::Ident(ident) => {
                    let range = self.range_of(ident);
                    self.data.extend_from_within(range)
                }
                InternedSegment::String(str) => self.data.push_str(str),
                InternedSegment::Int(int) => write!(self.data, "{}", int).unwrap(),
            }
        }
        let end = self.data.len();
        InternerEntry::new(&self.data, start..end)
    }

    fn range_of(&self, ident: VRef<str>) -> Range<usize> {
        let index = ident.index();
        let start = self.indices[index - 1];
        let end = self.indices[index];
        start..end
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
        let range = self.range_of(ident);
        &self.data[range]
    }
}

impl Serialize for Interner {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let map = self
            .map
            .iter()
            .map(|(&k, &v)| (k.start, k.end, v))
            .collect::<Vec<_>>();
        let raw = unsafe {
            RawInterner {
                map,
                indices: std::ptr::read(&self.indices),
                data: std::ptr::read(&self.data),
            }
        };

        let res = raw.serialize(serializer);

        std::mem::forget(raw.indices);
        std::mem::forget(raw.data);

        res
    }
}

impl<'a> Deserialize<'a> for Interner {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'a>,
    {
        let raw = RawInterner::deserialize(deserializer)?;
        let map = raw
            .map
            .into_iter()
            .map(|(start, end, ident)| {
                (
                    InternerEntry {
                        str: &*raw.data as *const String,
                        start,
                        end,
                    },
                    ident,
                )
            })
            .collect::<HashMap<_, _, InternerBuildHasher>>();

        Ok(Interner {
            map,
            indices: raw.indices,
            data: raw.data,
        })
    }
}

/// Enum offers allocation free passing of composite strings into [`Interner`].
/// See [`ident!`] macro for ease of use. The enum is displayed and pushed to
/// interner storage, hashed. If it is a new string, it is kept, otherwise
/// discarded.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InternedSegment<'a> {
    /// string already interned
    Ident(VRef<str>),
    /// string that may be interned
    String(&'a str),
    /// integer that will be displayed with [`std::fmt::Display`]
    Int(u32),
}

impl From<VRef<str>> for InternedSegment<'_> {
    fn from(ident: VRef<str>) -> Self {
        InternedSegment::Ident(ident)
    }
}

impl<'a> From<&'a str> for InternedSegment<'a> {
    fn from(s: &'a str) -> Self {
        InternedSegment::String(s)
    }
}

impl From<u32> for InternedSegment<'_> {
    fn from(i: u32) -> Self {
        InternedSegment::Int(i)
    }
}

#[derive(Deserialize, Serialize)]
struct RawInterner {
    map: Vec<(u32, u32, VRef<str>)>,
    indices: Vec<usize>,
    data: Box<String>,
}

#[derive(Clone, Copy)]
struct InternerEntry {
    str: *const String,
    start: u32,
    end: u32,
}

impl Hash for InternerEntry {
    fn hash<H: Hasher>(&self, state: &mut H) {
        unsafe {
            (*self.str)[self.start as usize..self.end as usize].hash(state);
        }
    }
}

impl PartialEq<Self> for InternerEntry {
    fn eq(&self, other: &Self) -> bool {
        unsafe {
            (*self.str)[self.start as usize..self.end as usize]
                == (*other.str)[other.start as usize..other.end as usize]
        }
    }
}

impl Eq for InternerEntry {}

impl InternerEntry {
    pub fn new(str: &String, range: Range<usize>) -> InternerEntry {
        InternerEntry {
            str: str as *const String,
            start: range.start as u32,
            end: range.end as u32,
        }
    }
}

#[derive(Default)]
struct InternerBuildHasher;

impl BuildHasher for InternerBuildHasher {
    type Hasher = InternerHasher;

    fn build_hasher(&self) -> InternerHasher {
        InternerHasher { value: 0 }
    }
}

struct InternerHasher {
    value: u64,
}

impl Hasher for InternerHasher {
    fn write(&mut self, bytes: &[u8]) {
        // sdbm hash
        self.value = bytes.iter().fold(self.value, |acc, &b| {
            acc.wrapping_add(acc << 16)
                .wrapping_add(acc << 8)
                .wrapping_sub(b as u64)
        });
    }

    fn finish(&self) -> u64 {
        self.value
    }
}

impl VRefDefault for str {
    fn default_state() -> VRef<Self> {
        Interner::EMPTY
    }
}

// #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
// pub struct Ident(u32);

// impl Ident {
//     pub const EMPTY: Self = Self(1);

//     pub unsafe fn new(index: usize) -> Self {
//         Ident(index as u32)
//     }

//     pub const fn index(self) -> usize {
//         self.0 as usize
//     }
// }

// impl Default for Ident {
//     fn default() -> Self {
//         Self::EMPTY
//     }
// }

// impl Invalid for Ident {
//     unsafe fn invalid() -> Self {
//         Self(0)
//     }

//     fn is_invalid(&self) -> bool {
//         self.0 == 0
//     }
// }

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_interner() {
        let mut interner = Interner::new();
        assert_eq!(interner.intern_str("a"), interner.intern_str("a"));
        let b = interner.intern_str("b");
        assert_eq!(
            interner.intern(ident!("c", b)),
            interner.intern(ident!("c", b))
        );
        let cb = interner.intern(ident!("c", b));
        assert_eq!(&interner[cb], "cb");
    }

    #[test]
    fn test_interner_serde() {
        let mut interner = Interner::new();
        let a = interner.intern_str("a");
        let b = interner.intern_str("b");
        let c = interner.intern_str("c");

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

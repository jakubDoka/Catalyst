use std::{
    fmt::{Display, Write},
    hash::{Hash, Hasher},
    ops::Index,
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
    data: Vec<Box<[u8]>>,
    garbage: Vec<Box<[u8]>>,
    cursor: *mut u8,
    temp: String,
}

unsafe impl Sync for Interner {}
unsafe impl Send for Interner {}

impl Interner {
    pub const CHUNK_SIZE: usize = 1024 * 1024;

    /// This does allocate very small amount of memory.
    pub fn new() -> Self {
        let mut slice = Box::new_uninit_slice(Self::CHUNK_SIZE);
        let cursor = slice.as_mut_ptr_range().end;
        let mut s = Interner {
            map: Map::default(),
            entries: PushMap::default(),
            data: vec![unsafe { slice.assume_init() }],
            garbage: Vec::new(),
            cursor: cursor as *mut u8,
            temp: String::new(),
        };
        s.init();
        s
    }

    pub fn clear(&mut self) {
        self.map.clear();
        self.garbage.extend(self.data.drain(1..));
        self.cursor = unsafe { self.data.get_unchecked_mut(0).as_mut_ptr_range().end };
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
                if self.cursor as usize + s.len() > Self::CHUNK_SIZE {
                    self.grow();
                }

                let new = unsafe { self.cursor.sub(s.len()) };
                unsafe { new.copy_from_nonoverlapping(s.as_ptr(), s.len()) };

                self.cursor = new;

                let slice = unsafe { std::slice::from_raw_parts(new, s.len()) };
                let str = unsafe { std::str::from_utf8_unchecked(slice) };

                let key = InternerEntry::new(str);

                let v_ref = unsafe { self.entries.push(key).cast() };
                self.map.insert(key, v_ref);

                v_ref
            }
        }
    }

    #[cold]
    #[inline(never)]
    fn grow(&mut self) {
        let mut slice = self
            .garbage
            .pop()
            .unwrap_or_else(|| unsafe { Box::new_uninit_slice(Self::CHUNK_SIZE).assume_init() });
        let end = slice.as_mut_ptr_range().end;
        self.data.push(slice);
        self.cursor = end;
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

// impl Serialize for Interner {
//     fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
//     where
//         S: serde::Serializer,
//     {
//         let map = self
//             .map
//             .iter()
//             .map(|(&k, &v)| (k.start, k.end, v))
//             .collect::<Vec<_>>();
//         let raw = unsafe {
//             RawInterner {
//                 map,
//                 indices: std::ptr::read(&self.indices),
//                 data: std::ptr::read(&self.data),
//             }
//         };

//         let res = raw.serialize(serializer);

//         std::mem::forget(raw.indices);
//         std::mem::forget(raw.data);

//         res
//     }
// }

// impl<'a> Deserialize<'a> for Interner {
//     fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
//     where
//         D: serde::Deserializer<'a>,
//     {
//         let raw = RawInterner::deserialize(deserializer)?;
//         let map = raw
//             .map
//             .into_iter()
//             .map(|(start, end, ident)| {
//                 (
//                     InternerEntry {
//                         str: &*raw.data as *const *const str,
//                         start,
//                         end,
//                     },
//                     ident,
//                 )
//             })
//             .collect::<HashMap<_, _, InternerBuildHasher>>();

//         Ok(Interner {
//             map,
//             indices: raw.indices,
//             data: raw.data,
//         })
//     }
// }

#[derive(Deserialize, Serialize)]
struct RawInterner {
    map: Vec<(u32, u32, VRef<str>)>,
    indices: Vec<usize>,
    #[allow(clippy::box_collection)]
    data: Box<String>,
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

    // #[test]
    // fn test_interner_serde() {
    //     let mut interner = Interner::new();
    //     let a = interner.intern("a");
    //     let b = interner.intern("b");
    //     let c = interner.intern("c");

    //     let mut buf = Vec::new();
    //     interner
    //         .serialize(&mut rmp_serde::encode::Serializer::new(&mut buf))
    //         .unwrap();

    //     let interner2 =
    //         Interner::deserialize(&mut rmp_serde::decode::Deserializer::new(buf.as_slice()))
    //             .unwrap();

    //     assert_eq!(&interner2[a], "a");
    //     assert_eq!(&interner2[b], "b");
    //     assert_eq!(&interner2[c], "c");
    // }
}

use std::{
    fmt::{Display, Write},
    hash::{Hash, Hasher},
    ops::{Index, Range},
};

use serde::{Deserialize, Serialize};

use crate::*;

pub type Ident = FragSlice<u8>;

macro_rules! gen_span_constants {
    (
        $($name:ident => $repr:literal,)*
    ) => {
        impl Interner {
            gen_span_constants!(@recur (0) $($name => $repr,)*);

            fn init(&mut self) {
                $(
                    self.intern($repr);
                )*
            }
        }
    };

    (@recur ($acc:expr) $name:ident => $repr:literal, $($rest:tt)*) => {
        pub const $name: Ident = FragSlice::new(FragSliceAddr::new(0, 0, $repr.len() as u16));
        gen_span_constants!(@recur ($acc + $repr.len()) $($rest)*);
    };

    (@recur ($acc:expr)) => {};
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
    map: CMap<InternerEntry, Ident>,
    frag_map: FragMap<u8>,
    temp: String,
}

unsafe impl Sync for Interner {}
unsafe impl Send for Interner {}

impl Interner {
    pub fn new(frag_map: FragMap<u8>) -> Self {
        let mut s = Interner {
            map: CMap::default(),
            frag_map,
            temp: String::new(),
        };
        s.init();
        s
    }

    pub fn update(&mut self, base: &mut FragBase<u8>) {
        self.frag_map.update(base);
    }

    pub fn intern_scoped(&mut self, scope: impl Display, name: Ident) -> Ident {
        self.intern_with(|s, t| write!(t, "{}\\{}", scope, &s[name]))
    }

    pub fn intern_with<T>(&mut self, mut builder: impl FnMut(&Self, &mut String) -> T) -> Ident {
        let mut temp = std::mem::take(&mut self.temp);
        builder(self, &mut temp);
        let res = self.intern(&temp);
        temp.clear();
        self.temp = temp;
        res
    }

    pub fn intern(&mut self, s: &str) -> Ident {
        match self.map.get(&InternerEntry::new(s)) {
            Some(v) => v.to_owned(),
            None => {
                let v = self.frag_map.extend(s.as_bytes().iter().copied());
                let key = InternerEntry::new(&self[v]);
                self.map.insert(key, v);
                v
            }
        }
    }

    fn to_raw(&self) -> RawInterner {
        todo!()
    }
}

impl Index<Ident> for Interner {
    type Output = str;

    fn index(&self, ident: Ident) -> &str {
        unsafe { std::str::from_utf8_unchecked(&self.frag_map[ident]) }
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
        todo!()
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

#[cfg(test)]
mod test {
    use super::*;

    use std::fmt::Write;

    #[test]
    fn test_interner() {
        let (_base, mut frag_maps) = FragBase::new(1);
        let mut interner = Interner::new(frag_maps.pop().unwrap());
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
        let (_base, mut frag_maps) = FragBase::new(1);
        let mut interner = Interner::new(frag_maps.pop().unwrap());
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

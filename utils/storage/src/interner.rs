use std::{
    default::default,
    fmt::{Display, Write},
    ops::Index,
    sync::Arc,
};

use bump_alloc::dashmap::mapref::entry::Entry;
use rkyv::{
    ser::{ScratchSpace, Serializer, SharedSerializeRegistry},
    with::{ArchiveWith, DeserializeWith, SerializeWith},
    Archive, Archived, Deserialize, Fallible, Resolver, Serialize,
};

use crate::*;

#[derive(
    Clone,
    Archive,
    Serialize,
    Deserialize,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Debug,
    Default,
    Hash,
)]
#[archive_attr(derive(PartialEq, Eq, Hash))]
pub struct Ident(FragSlice<u8>);

derive_relocated!(
    struct Ident {}
);

macro_rules! gen_span_constants {
    (
        $($name:ident => $repr:literal,)*
    ) => {
        impl Interner {
            gen_span_constants!(@recur (0) $($name => $repr,)*);

            fn init(&mut self) {
                $(
                    let ident = self.intern($repr);
                    assert_eq!(ident.0, Self::$name.0, "constant {} is not interned", stringify!($name));
                    assert_eq!(&self[ident], $repr, "constant {} repr is not equal to repr", stringify!($name));
                )*
            }
        }
    };

    (@recur ($acc:expr) $name:ident => $repr:literal, $($rest:tt)*) => {
        pub const $name: Ident = Ident(FragSlice::new(FragSliceAddr::new($acc as u32, 0, $repr.len() as u16)));
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
    MOIST => "moist",
    BUILTIN => "builtin",
}

impl Default for Interner {
    fn default() -> Self {
        let base = InternerBase::new(1);
        let int = { base.split().next().unwrap() };
        int
    }
}

pub struct InternerBase {
    map: Arc<CMap<FragSliceKey<u8>, FragSlice<u8>>>,
    frag_base: SyncFragBase<u8>,
}

impl InternerBase {
    pub fn new(thread_count: u8) -> Self {
        let s = Self {
            map: default(),
            frag_base: SyncFragBase::new(thread_count),
        };

        if let Some(mut i) = {
            let i = s.split().next();
            i
        } {
            i.init();
        }

        s
    }

    pub fn split(&self) -> impl Iterator<Item = Interner> + '_ {
        let map = self.map.clone();
        self.frag_base.split().map(move |frag_map| Interner {
            map: map.clone(),
            frag_map,
            temp: String::new(),
        })
    }
}

/// Struct ensures that all distinct strings are stored just once (not substrings),
/// and are assigned unique id.
pub struct Interner {
    map: Arc<CMap<FragSliceKey<u8>, FragSlice<u8>>>,
    frag_map: SyncFragMap<u8>,
    temp: String,
}

unsafe impl Sync for Interner {}
unsafe impl Send for Interner {}

impl Interner {
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
        let slice = self.frag_map.extend(s.as_bytes().iter().copied());
        let key = unsafe { FragSliceKey::new(&self.frag_map, slice) };
        Ident(match self.map.entry(key) {
            Entry::Occupied(entry) => {
                unsafe {
                    self.frag_map.unextend(slice);
                }
                *entry.get()
            }
            Entry::Vacant(entry) => *entry.insert(slice),
        })
    }
}

impl Index<Ident> for Interner {
    type Output = str;

    fn index(&self, ident: Ident) -> &str {
        unsafe {
            let slice = &self.frag_map[ident.0];
            std::str::from_utf8_unchecked(slice)
        }
    }
}

#[derive(Archive, Deserialize, Serialize)]

pub struct ArchivedInterner {
    slices: Vec<FragSlice<u8>>,
    data: SyncFragBase<u8>,
}

impl ArchivedInterner {
    pub fn from_interner(interner: &InternerBase) -> Self {
        Self {
            slices: interner
                .map
                .iter()
                .map(|entry| entry.value().to_owned())
                .collect(),
            data: interner.frag_base.clone(),
        }
    }

    pub fn to_interner(self) -> InternerBase {
        InternerBase {
            map: Arc::new(
                self.slices
                    .into_iter()
                    .map(|s| (unsafe { FragSliceKey::from_base(&self.data, s) }, s))
                    .collect(),
            ),
            frag_base: self.data,
        }
    }
}

pub struct InternerArchiver;

impl ArchiveWith<InternerBase> for InternerArchiver {
    type Archived = Archived<ArchivedInterner>;

    type Resolver = Resolver<ArchivedInterner>;

    unsafe fn resolve_with(
        field: &InternerBase,
        pos: usize,
        resolver: Self::Resolver,
        out: *mut Self::Archived,
    ) {
        ArchivedInterner::from_interner(field).resolve(pos, resolver, out)
    }
}

impl<S: SharedSerializeRegistry + Serializer + ?Sized + ScratchSpace> SerializeWith<InternerBase, S>
    for InternerArchiver
{
    fn serialize_with(
        field: &InternerBase,
        serializer: &mut S,
    ) -> Result<Self::Resolver, <S as rkyv::Fallible>::Error> {
        ArchivedInterner::from_interner(field).serialize(serializer)
    }
}

impl<D: Fallible + ?Sized> DeserializeWith<Archived<ArchivedInterner>, InternerBase, D>
    for InternerArchiver
where
    <ArchivedInterner as Archive>::Archived: Deserialize<ArchivedInterner, D>,
{
    fn deserialize_with(
        field: &Archived<ArchivedInterner>,
        deserializer: &mut D,
    ) -> Result<InternerBase, <D as Fallible>::Error> {
        field.deserialize(deserializer).map(|a| a.to_interner())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use std::{fmt::Write, thread};

    #[test]
    fn test() {
        let base = InternerBase::new(1);
        let mut interner = base.split().next().unwrap();
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
    fn stress_test() {
        let base = InternerBase::new(4);

        thread::scope(|s| {
            for mut thread in base.split() {
                s.spawn(move || {
                    for i in 0..100_000 {
                        let id = i % 10_000;
                        let slice = thread.intern_with(|_s, t| write!(t, "{}", id));
                        assert_eq!(thread[slice].parse(), Ok(id));
                    }
                });
            }
        })
    }

    // #[test]
    // fn test_interner_serde() {
    //     let base = InternerBase::new(1);
    //     let mut interner = base.split().next().unwrap();
    //     let a = interner.intern("a");
    //     let b = interner.intern("b");
    //     let c = interner.intern("c");

    //     let mut buf = Vec::new();
    //     interner
    //         .serialize(&mut rmp_rkyv::encode::Serializer::new(&mut buf))
    //         .unwrap();

    //     let interner2 =
    //         Interner::deserialize(&mut rmp_rkyv::decode::Deserializer::new(buf.as_slice()))
    //             .unwrap();

    //     assert_eq!(&interner2[a], "a");
    //     assert_eq!(&interner2[b], "b");
    //     assert_eq!(&interner2[c], "c");
    // }
}

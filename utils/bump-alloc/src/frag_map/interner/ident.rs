use super::*;

#[derive(
    Clone, Archive, Serialize, Deserialize, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash,
)]
#[archive_attr(derive(PartialEq, Eq, Hash))]
pub struct Ident(pub(super) RawIdent);

impl Ident {
    pub fn get<'a>(&'a self, interner: &'a Interner) -> &'a str {
        if let Some(slice) = unsafe { self.0.inline.data[..].get(..self.0.inline.len as usize) } {
            return unsafe { std::str::from_utf8_unchecked(slice) };
        }

        interner.storage[unsafe { self.compress() }]
    }

    /// # Safety
    /// Returns valid ofly if Ident is constructurd with from_ref
    pub unsafe fn compress(self) -> FragRef<&'static str> {
        FragRef::new(FragAddr::new(
            self.0.allocated.index,
            self.0.allocated.thread,
        ))
    }

    pub(super) fn from_ref(frag: FragRef<&'static str>) -> Ident {
        Self(RawIdent::from_ref(frag))
    }

    pub(super) fn from_str(s: &str) -> Option<Ident> {
        RawIdent::from_str(s).map(Self)
    }
}

impl Default for Ident {
    fn default() -> Self {
        Interner::EMPTY
    }
}

derive_relocated!(
    struct Ident {}
);

#[repr(C)]
#[derive(Clone, Copy)]
pub union RawIdent {
    inline: Inline,
    allocated: Allocated,
    archived: [u8; 16],
}

#[repr(C)]
#[derive(Clone, Copy)]
struct Inline {
    len: u8,
    data: [u8; 15],
}

#[repr(C)]
#[derive(Clone, Copy)]
struct Allocated {
    len: u8,
    thread: u8,
    pad1: [u8; 2],
    index: u32,
    pad2: [u8; 8],
}

impl RawIdent {
    const fn from_str(str: &str) -> Option<Self> {
        if str.len() > 15 {
            return None;
        }

        let mut array = [0; 15];
        let mut i = 0;
        while let Some(&b) = str.as_bytes().get(i) {
            array[i] = b;
            i += 1;
        }
        Some(Self {
            inline: Inline {
                len: str.len() as u8,
                data: array,
            },
        })
    }

    fn from_ref(frag: FragRef<&'static str>) -> Self {
        let FragAddr { index, thread, .. } = frag.addr();
        Self {
            allocated: Allocated {
                len: 16,
                thread,
                pad1: [0; 2],
                index,
                pad2: [0; 8],
            },
        }
    }
}

#[derive(PartialEq, Eq, Hash)]
pub struct ArchivedRawIdent(Archived<[u8; 16]>);

impl Archive for RawIdent {
    type Archived = ArchivedRawIdent;

    type Resolver = Resolver<[u8; 16]>;

    unsafe fn resolve(&self, pos: usize, resolver: Self::Resolver, out: *mut Self::Archived) {
        let (o, f) = out_field!(out.0);
        self.archived.resolve(pos + o, resolver, f);
    }
}

impl<S: Serializer + ?Sized> Serialize<S> for RawIdent {
    fn serialize(&self, serializer: &mut S) -> Result<Self::Resolver, <S as Fallible>::Error> {
        unsafe { self.archived.serialize(serializer) }
    }
}

impl<D: Fallible + ?Sized> Deserialize<RawIdent, D> for ArchivedRawIdent
where
    Archived<[u8; 16]>: Deserialize<[u8; 16], D>,
{
    fn deserialize(&self, deserializer: &mut D) -> Result<RawIdent, <D as Fallible>::Error> {
        let archived = self.0.deserialize(deserializer)?;
        Ok(RawIdent { archived })
    }
}

impl PartialEq for RawIdent {
    fn eq(&self, other: &Self) -> bool {
        unsafe { self.archived == other.archived }
    }
}

impl Eq for RawIdent {}

impl PartialOrd for RawIdent {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(unsafe { self.archived.cmp(&other.archived) })
    }
}

impl Ord for RawIdent {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        unsafe { self.archived.cmp(&other.archived) }
    }
}

impl std::fmt::Debug for RawIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", unsafe { self.archived })
    }
}

impl Hash for RawIdent {
    fn hash<H: ~const std::hash::Hasher>(&self, state: &mut H) {
        unsafe { self.archived.hash(state) }
    }
}
macro_rules! gen_span_constants {
    (
        $($name:ident => $repr:literal,)*
    ) => {
        impl Interner {
            gen_span_constants!(@recur (0) $($name => $repr,)*);

            pub(super) fn init(&mut self) {
                $(
                    let ident = self.intern($repr);
                    assert_eq!(ident.0, Self::$name.0, "constant {} is not interned", stringify!($name));
                    assert_eq!(ident.get(self), $repr, "constant {} repr is not equal to repr", stringify!($name));
                )*
            }
        }
    };

    (@recur ($acc:expr) $name:ident => $repr:literal, $($rest:tt)*) => {
        pub const $name: Ident = Ident(RawIdent::from_str($repr).unwrap());
        gen_span_constants!(@recur ($acc + 1) $($rest)*);
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

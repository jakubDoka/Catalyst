use crate::*;

use storage::*;

pub type Funcs = FragMap<Func, MAX_FRAGMENT_SIZE>;
pub type FuncSlices = FragMap<FragRef<Func>, MAX_FRAGMENT_SIZE>;

#[derive(Clone, Copy, Default)]
pub struct Func {
    pub generics: Generics,
    pub owner: Option<Ty>,
    pub upper_generics: Generics,
    pub signature: Signature,
    pub flags: FuncFlags,
    pub visibility: FuncVisibility,
    pub name: FragSlice<u8>,
    pub loc: Option<Loc>,
}

impl Func {
    pub fn is_generic(&self) -> bool {
        !self.generics.is_empty() || !self.upper_generics.is_empty()
    }
}

gen_water_drops! {
    Func
    funcs
    BOOL_BAND => "bool & bool",
    U8_EQ => "u8 == u8",
    U16_EQ => "u16 == u16",
    U32_EQ => "u32 == u32",
    U64_EQ => "u64 == u64",
    UINT_EQ => "uint == uint",
    CAST => "cast",
    SIZEOF => "sizeof",
}

bitflags! {
    FuncFlags: u8 {
        ENTRY
        BUILTIN
        NO_MOVES
    }
}

#[derive(Clone, Copy, Default, PartialEq, Eq)]
pub enum FuncVisibility {
    #[default]
    Local,
    Exported,
    Imported,
}

#[derive(Clone, Copy, Default)]
pub struct Signature {
    pub cc: Option<FragSlice<u8>>,
    pub args: FragSlice<Ty>,
    pub ret: Ty,
}

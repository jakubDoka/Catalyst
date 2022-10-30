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
    ANON_TEMP => "anon_temp",
    CAST => "cast",
    SIZEOF => "sizeof",
}

bitflags! {
    FuncFlags: u8 {
        ENTRY
        BUILTIN
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

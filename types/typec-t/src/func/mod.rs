use crate::*;
use serde::{Deserialize, Serialize};
use storage::*;

pub type Funcs = PushMap<Func>;
pub type FuncSlices = BumpMap<VRef<Func>>;

#[derive(Clone, Copy, Default, Deserialize, Serialize)]
pub struct Func {
    pub generics: Generics,
    pub owner: Option<Ty>,
    pub upper_generics: Generics,
    pub signature: Signature,
    pub flags: FuncFlags,
    pub visibility: FuncVisibility,
    pub name: VRef<str>,
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
    #[Deserialize, Serialize]
    FuncFlags: u8 {
        ENTRY
        BUILTIN
    }
}

#[derive(Clone, Copy, Default, PartialEq, Eq, Deserialize, Serialize)]
pub enum FuncVisibility {
    #[default]
    Local,
    Exported,
    Imported,
}

#[derive(Clone, Copy, Default, Deserialize, Serialize)]
pub struct Signature {
    pub cc: Option<VRef<str>>,
    pub args: VSlice<Ty>,
    pub ret: Ty,
}

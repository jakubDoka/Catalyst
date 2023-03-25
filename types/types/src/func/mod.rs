use crate::*;

use storage::*;

use rkyv::{Archive, Deserialize, Serialize};

pub type Funcs = FragMap<Func>;
pub type FuncSlices = FragMap<FragRef<Func>>;

#[derive(Clone, Copy, Default, Deserialize, Serialize, Archive)]

pub struct Func {
    pub generics: WhereClause,
    pub outer_param_count: u16,
    pub owner: Option<Ty>,
    pub signature: Signature,
    pub flags: FuncFlags,
    pub visibility: FuncVisibility,
    pub name: Ident,
    pub loc: Loc,
}

derive_relocated!(struct Func { generics owner signature });

impl Func {
    pub fn is_generic(&self) -> bool {
        !self.generics.is_empty()
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
    EXIT => "exit",
}

bitflags! {
    FuncFlags: u8 {
        ENTRY
        BUILTIN
        NO_MOVES
    }
}

#[derive(Clone, Copy, Default, PartialEq, Eq, Deserialize, Serialize, Archive)]

pub enum FuncVisibility {
    #[default]
    Local,
    Exported,
    Imported,
}

#[derive(Clone, Copy, Default, Deserialize, Serialize, Archive)]

pub struct Signature {
    pub cc: Option<Ident>,
    pub args: FragSlice<Ty>,
    pub ret: Ty,
}

derive_relocated!(struct Signature { args ret });

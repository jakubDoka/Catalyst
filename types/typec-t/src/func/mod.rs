use crate::*;
use storage::*;

pub type Funcs = OrderedMap<VRef<str>, Func>;
pub type FuncSlices = PoolBumpMap<VRef<Func>>;

#[derive(Clone, Copy, Default)]
pub struct Func {
    pub generics: VRefSlice<Spec>,
    pub upper_generics: VRefSlice<Spec>,
    pub signature: Signature,
    pub flags: FuncFlags,
    pub visibility: FuncVisibility,
    pub name: VRef<str>,
    pub loc: Loc,
}

impl Func {
    gen_increasing_constants!(ANON_TEMP);

    pub fn is_generic(&self) -> bool {
        !self.generics.is_empty() || !self.upper_generics.is_empty()
    }
}

impl_located!(Func);
impl_flagged!(Func, FuncFlags);

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
    pub cc: Option<VRef<str>>,
    pub args: VRefSlice<Ty>,
    pub ret: VRef<Ty>,
}

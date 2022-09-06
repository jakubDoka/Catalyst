use crate::*;
use lexing_t::*;
use storage::*;

pub type Funcs = OrderedMap<Ident, Func>;
pub type FuncSlices = PoolBumpMap<VRef<Func>>;

#[derive(Clone, Copy, Default)]
pub struct Func {
    pub generics: VRefSlice<Bound>,
    pub signature: Signature,
    pub flags: FuncFlags,
    pub loc: Loc,
}

impl_located!(Func);
impl_flagged!(Func, FuncFlags);

bitflags! {
    FuncFlags: u8 {
        GENERIC
    }
}

#[derive(Clone, Copy, Default)]
pub struct Signature {
    pub cc: Maybe<Ident>,
    pub args: VRefSlice<Ty>,
    pub ret: Maybe<VRef<Ty>>,
}

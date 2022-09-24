use crate::*;
use lexing_t::*;
use storage::*;

pub type Funcs = OrderedMap<VRef<str>, Func>;
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
        EXTERN
    }
}

#[derive(Clone, Copy, Default)]
pub struct Signature {
    pub cc: Maybe<VRef<str>>,
    pub args: VRefSlice<Ty>,
    pub ret: VRef<Ty>,
}

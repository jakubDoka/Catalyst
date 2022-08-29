use std::default::default;

use crate::*;
use lexing_t::*;
use storage::*;

pub mod bound_checker;

#[derive(Clone, Copy, Debug, Default)]
pub struct Bound {
    pub kind: BoundKind,
    pub flags: BoundFlags,
    pub loc: Loc,
}

impl Invalid for Bound {
    unsafe fn invalid() -> Self {
        Bound {
            loc: Loc::invalid(),
            ..default()
        }
    }

    fn is_invalid(&self) -> bool {
        self.loc.is_invalid()
    }
}

gen_kind! {
    BoundKind {
        Base = BoundBase {
            inherits: VSlice<VRef<Bound>>,
            generics: VSlice<VRef<Bound>>,
            assoc_types: VSlice<VRef<Bound>>,
            funcs: VSlice<BoundFunc>,
        },
        Instance = BoundInstance {
            base: VRef<Bound>,
            params: VSlice<VRef<Ty>>,
            assoc_types: VSlice<VRef<Ty>>,
        },
    }
}

impl Default for BoundKind {
    fn default() -> Self {
        BoundKind::Base(BoundBase::default())
    }
}

bitflags! {
    struct BoundFlags: u8 {
        ANON
        GENERIC
    }
}

#[derive(Clone, Copy, Default)]
pub struct BoundFunc {
    pub sig: Sig,
    pub params: VSlice<VRef<Bound>>,
    pub loc: Loc,
    pub parent: VRef<Bound>,
}

#[derive(Clone, Copy, Default)]
pub struct Impl {
    pub id: Ident,
    pub params: VSlice<VRef<Bound>>,
    pub bound: VRef<Bound>,
    pub implementor: VRef<Ty>,
    pub funcs: VSlice<VRef<Func>>,
    pub loc: Loc,
    pub next: Maybe<VRef<Impl>>,
}

impl Bound {
    gen_increasing_constants!(ANY);
}

impl VRefDefault for Bound {
    fn default_state() -> VRef<Self> {
        Self::ANY
    }
}

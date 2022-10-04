use std::default::default;

use crate::*;
use lexing_t::Span;
use storage::*;

pub type Specs = OrderedMap<VRef<str>, Spec>;
pub type SpecFuncs = PoolBumpMap<SpecFunc>;
pub type SpecSlices = PoolBumpMap<VRef<Spec>>;

#[derive(Clone, Copy, Default)]
pub struct Spec {
    pub kind: BoundKind,
    pub flags: BoundFlags,
    pub loc: Loc,
}

impl_located!(Spec);
impl_variadic!(Spec, BoundKind);
impl_flagged!(Spec, BoundFlags);

impl Spec {
    gen_v_ref_constants!(ANY);
}

impl VRefDefault for Spec {
    fn default_state() -> VRef<Self> {
        Self::ANY
    }
}

gen_kind!(BoundKind
    Base = SpecBase {
        inherits: VRefSlice<Spec>,
        generics: VRefSlice<Spec>,
        methods: VSlice<SpecFunc>,
    },
    Instance = SpecInstance {
        base: VRef<Spec>,
        params: VRefSlice<Ty>,
    },
);

impl Default for BoundKind {
    fn default() -> Self {
        Self::Base(default())
    }
}

pub struct SpecFunc {
    pub generics: VRefSlice<Spec>,
    pub signature: Signature,
    pub name: VRef<str>,
    pub span: Option<Span>,
    pub parent: VRef<Spec>,
}

bitflags! {
    BoundFlags: u8 {
        GENERIC
    }
}

impl BoundExt for Specs {}

pub trait BoundExt: StorageExt<Spec> {
    #[inline]
    fn base(&self, bound: VRef<Spec>) -> VRef<Spec> {
        self.try_inner::<SpecInstance>(bound)
            .map(|bound| bound.base)
            .unwrap_or(bound)
    }
}

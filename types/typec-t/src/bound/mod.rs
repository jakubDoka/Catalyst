use std::default::default;

use crate::*;
use lexing_t::*;
use storage::*;

pub type Bounds = OrderedMap<VRef<str>, Bound>;
pub type BoundFuncs = PoolBumpMap<BoundFunc>;
pub type BoundSlices = PoolBumpMap<VRef<Bound>>;

#[derive(Clone, Copy, Default)]
pub struct Bound {
    pub kind: BoundKind,
    pub flags: BoundFlags,
    pub loc: Loc,
}

impl_located!(Bound);
impl_variadic!(Bound, BoundKind);
impl_flagged!(Bound, BoundFlags);

impl Bound {
    gen_v_ref_constants!(ANY);
}

impl VRefDefault for Bound {
    fn default_state() -> VRef<Self> {
        Self::ANY
    }
}

gen_kind!(BoundKind
    Base = BoundBase {
        inherits: VRefSlice<Bound>,
        generics: VRefSlice<Bound>,
        methods: VSlice<BoundFunc>,
    },
    Instance = BoundInstance {
        base: VRef<Bound>,
        params: VRefSlice<Ty>,
    },
);

impl Default for BoundKind {
    fn default() -> Self {
        Self::Base(default())
    }
}

pub struct BoundFunc {
    pub generics: VRefSlice<Bound>,
    pub signature: Signature,
    pub loc: Loc,
    pub parent: VRef<Bound>,
}

bitflags! {
    BoundFlags: u8 {
        GENERIC
    }
}

impl BoundExt for Bounds {}

pub trait BoundExt: StorageExt<Bound> {
    #[inline]
    fn base(&self, bound: VRef<Bound>) -> VRef<Bound> {
        self.try_inner::<BoundInstance>(bound)
            .map(|bound| bound.base)
            .unwrap_or(bound)
    }
}

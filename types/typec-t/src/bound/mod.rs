use crate::*;
use lexing_t::*;
use storage::*;

pub type Bounds = OrderedMap<Ident, Bound>;
pub type BoundFuncs = PoolBumpMap<BoundFunc>;
pub type BoundSlices = PoolBumpMap<VRef<Bound>>;

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
        generics: VRefSlice<Bound>,
        inherits: VRefSlice<Bound>,
        assoc_types: VRefSlice<Bound>,
        methods: VSlice<BoundFunc>,
    },
    Instance = BoundInstance {
        base: VRef<Bound>,
        params: VRefSlice<Ty>,
        assoc_types: VRefSlice<Ty>,
    },
);

pub struct BoundFunc {
    pub generics: VRefSlice<Bound>,
    pub signature: Signature,
    pub loc: Loc,
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

    #[inline]
    fn assoc_types(&self, bound: VRef<Bound>) -> VRefSlice<Bound> {
        self.inner::<BoundBase>(self.base(bound)).assoc_types
    }
}

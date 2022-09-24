use crate::*;
use lexing_t::*;
use scope::Vis;
use storage::*;

pub type Types = OrderedMap<VRef<str>, Ty>;
pub type TySlices = PoolBumpMap<VRef<Ty>>;
pub type Fields = PoolBumpMap<Field>;

#[derive(Default)]
pub struct Ty {
    pub kind: TyKind,
    pub flags: TyFlags,
    pub loc: Loc,
}

impl_located!(Ty);
impl_variadic!(Ty, TyKind);
impl_flagged!(Ty, TyFlags);

impl Ty {
    gen_v_ref_constants!(
        INFERRED
        MUTABLE IMMUTABLE
        UNIT
        UINT
        TERMINAL
    );

    gen_v_ref_const_group!(
        INTEGERS = [UINT];
        UNSIGNED_INTEGER = [UINT];
    );

    #[inline(always)]
    pub fn compatible(expected: VRef<Self>, inferred: VRef<Self>) -> bool {
        expected == inferred || inferred == Self::TERMINAL
    }
}

impl VRefDefault for Ty {
    fn default_state() -> VRef<Self> {
        Self::INFERRED
    }
}

gen_kind!(TyKind
    SelfBound,
    Instance = TyInstance {
        base: VRef<Ty>,
        args: VRefSlice<Ty>,
    },
    Struct = TyStruct {
        generics: VRefSlice<Bound>,
        fields: VSlice<Field>,
    },
    Pointer = TyPointer {
        base: VRef<Ty>,
        mutability: VRef<Ty>,
        depth: u32,
    },
    Integer = TyInteger {
        size: u8,
        signed: bool,
    },
    Param = u32,
    Bool,
    Inferred,
);

impl Default for TyKind {
    fn default() -> Self {
        Self::Inferred
    }
}

#[derive(Clone, Copy, Default)]
pub struct Field {
    pub vis: Vis,
    pub ty: VRef<Ty>,
    pub flags: FieldFlags,
    pub loc: Loc,
}

impl_located!(Field);
impl_flagged!(Field, FieldFlags);

bitflags! {
    FieldFlags: u8 {
        MUTABLE
        USED
    }
}

bitflags! {
    TyFlags: u8 {
        GENERIC
    }
}

impl TyExt for Types {}

pub trait TyExt: StorageExt<Ty> {
    #[inline]
    fn generics(&self, target: VRef<Ty>) -> Option<VRefSlice<Bound>> {
        let target = self.base(target);
        Some(match self[target].kind {
            TyKind::Struct(s) => s.generics,
            _ => return None,
        })
    }

    #[inline]
    fn pointer_base(&self, target: VRef<Ty>) -> VRef<Ty> {
        self.try_inner::<TyPointer>(target)
            .map(|ty| ty.base)
            .unwrap_or(target)
    }

    #[inline]
    fn instance_base(&self, target: VRef<Ty>) -> VRef<Ty> {
        self.try_inner::<TyInstance>(target)
            .map(|ty| ty.base)
            .unwrap_or(target)
    }

    #[inline]
    fn base(&self, target: VRef<Ty>) -> VRef<Ty> {
        self.instance_base(self.pointer_base(target))
    }

    #[inline]
    fn is_generic(&self, target: VRef<Ty>) -> bool {
        self.flags(target).contains(TyFlags::GENERIC)
    }
}

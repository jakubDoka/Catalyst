use std::{default::default, ops::Index};

use crate::*;
use lexing_t::Span;
use parsing_t::Vis;
use storage::*;

pub type Types = OrderedMap<VRef<str>, Ty>;
pub type TySlices = PoolBumpMap<VRef<Ty>>;
pub type Fields = PoolBumpMap<Field>;
pub type SpecFuncs = PoolBumpMap<SpecFunc>;
pub type ImplLookup = Map<(VRef<Ty>, VRef<Ty>), Option<VRef<Impl>>>;
pub type Impls = PushMap<Impl>;

#[derive(Clone, Copy)]
pub struct Impl {
    pub generics: VRefSlice<Ty>,
    pub ty: VRef<Ty>,
    pub spec: VRef<Ty>,
    pub methods: VRefSlice<Func>,
    pub next: Option<VRef<Impl>>,
    pub span: Option<Span>,
}

impl Impl {
    gen_increasing_constants!(ANY);
}

#[derive(Default)]
pub struct Ty {
    pub kind: TyKind,
    pub flags: TyFlags,
    pub loc: Loc,
}

impl_located!(Ty);

impl Ty {
    gen_v_ref_constants!(
        MUTABLE IMMUTABLE
        ANY
        UNIT
        UINT
        U32
        CHAR
        TERMINAL
    );

    gen_v_ref_const_group!(
        INTEGERS = [UINT U32];
        UNSIGNED_INTEGER = [UINT U32];
    );

    #[inline(always)]
    pub fn compatible(expected: VRef<Self>, inferred: VRef<Self>) -> bool {
        expected == inferred || inferred == Self::TERMINAL
    }
}

impl VRefDefault for Ty {
    fn default_state() -> VRef<Self> {
        Self::UNIT
    }
}

gen_kind!(TyKind
    Instance = TyInstance {
        base: VRef<Ty>,
        args: VRefSlice<Ty>,
    },
    Struct = TyStruct {
        generics: VRefSlice<Ty>,
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
    Spec = TySpec {
        inherits: VRefSlice<Ty>,
        generics: VRefSlice<Ty>,
        methods: VSlice<SpecFunc>,
    },
    Param = u32,
    Bool,
);

impl Default for TyKind {
    fn default() -> Self {
        Self::Struct(default())
    }
}

#[derive(Clone, Copy)]
pub struct SpecFunc {
    pub generics: VRefSlice<Ty>,
    pub signature: Signature,
    pub name: VRef<str>,
    pub span: Option<Span>,
    pub parent: VRef<Ty>,
}

#[derive(Clone, Copy, Default)]
pub struct Field {
    pub vis: Vis,
    pub ty: VRef<Ty>,
    pub flags: FieldFlags,
    pub span: Option<Span>,
    pub name: VRef<str>,
}

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

pub trait TyExt: Index<VRef<Ty>, Output = Ty> {
    #[inline]
    fn is_signed(&self, ty: VRef<Ty>) -> bool {
        matches!(self[ty].kind, TyKind::Integer(TyInteger { signed, .. }) if signed)
    }

    #[inline]
    fn generics(&self, target: VRef<Ty>) -> Option<VRefSlice<Ty>> {
        let target = self.base(target);
        Some(match self[target].kind {
            TyKind::Struct(s) => s.generics,
            _ => return None,
        })
    }

    #[inline]
    fn pointer_base(&self, target: VRef<Ty>) -> VRef<Ty> {
        self[target]
            .kind
            .try_cast::<TyPointer>()
            .map(|ty| ty.base)
            .unwrap_or(target)
    }

    #[inline]
    fn instance_base(&self, target: VRef<Ty>) -> VRef<Ty> {
        self[target]
            .kind
            .try_cast::<TyInstance>()
            .map(|ty| ty.base)
            .unwrap_or(target)
    }

    #[inline]
    fn base(&self, target: VRef<Ty>) -> VRef<Ty> {
        self.instance_base(self.pointer_base(target))
    }

    #[inline]
    fn is_generic(&self, target: VRef<Ty>) -> bool {
        self[target].flags.contains(TyFlags::GENERIC)
    }
}

use std::{default::default, fmt, mem};

use crate::*;
use span::*;

use rkyv::{Archive, Deserialize, Serialize};
use storage::*;

use self::{
    compact::{CompactTy, ExpandedTy},
    spec::TyParam,
};

pub mod compact;
pub mod data;
pub mod pointer;
pub mod spec;
pub mod spec_set;

wrapper_enum! {
    #[derive(
        Clone, Copy, PartialEq, Eq, Hash, Debug, PartialOrd, Ord
    )]
    enum Spec 'a: {
        Base: FragRef<SpecBase>,
        Instance: ExpInstance<'a, FragRef<SpecBase>>,
    }
}

impl<'a> Spec<'a> {
    pub fn load(compact: CompactSpec, types: &Types, arena: &ProxyArena<'a>) -> Self {
        match compact.expanded() {
            ExpandedSpec::Base(b) => Self::Base(b),
            ExpandedSpec::Instance(i) => {
                let SpecInstance { base, args } = types[i];
                let args = Ty::load_slice(&types[args], types, arena);
                Self::Instance(ExpInstance { base, args })
            }
        }
    }

    pub fn load_slice(slice: &[CompactSpec], types: &Types, arena: &ProxyArena<'a>) -> &'a [Self] {
        arena.alloc_iter(slice.iter().map(|&s| Self::load(s, types, arena)))
    }

    pub fn base(self) -> FragRef<SpecBase> {
        match self {
            Spec::Base(base) => base,
            Spec::Instance(instance) => instance.base,
        }
    }

    pub fn params(self) -> &'a [Ty<'a>] {
        match self {
            Spec::Base(..) => &[],
            Spec::Instance(instance) => instance.args,
        }
    }

    pub fn infer(
        self,
        template: Self,
        params: &mut [Option<Ty<'a>>],
    ) -> Result<(), SpecCmpError<'a>> {
        match (self, template) {
            (Spec::Instance(reference), Spec::Instance(template)) => {
                if reference.base != template.base {
                    return Err(SpecCmpError::Specs(
                        Spec::Base(reference.base),
                        Spec::Base(template.base),
                    ));
                }

                reference
                    .args
                    .iter()
                    .zip(template.args)
                    .fold(Ok(()), |acc, (&reference, &template)| {
                        acc.and(reference.infer(template, params))
                    })
                    .map_err(|(a, b)| SpecCmpError::Args(a, b))
            }
            _ if self == template => Ok(()),
            _ => Err(SpecCmpError::Specs(self, template)),
        }
    }
}

wrapper_enum! {
    #[derive(
        Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize, Archive,
    )]
    #[archive_attr(derive(PartialEq, Eq, Hash))]
    enum BaseTy: relocated {
        Struct: FragRef<Struct>,
        Enum: FragRef<Enum>,
    }
}

impl fmt::Display for BaseTy {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BaseTy::Struct(s) => write!(f, "struct{:x}", s.bits()),
            BaseTy::Enum(e) => write!(f, "enum{:x}", e.bits()),
        }
    }
}

impl BaseTy {
    pub fn as_ty(self) -> Ty<'static> {
        Ty::Base(self)
    }

    pub fn generics(self, types: &Types) -> WhereClause {
        match self {
            BaseTy::Struct(s) => types[s].generics,
            BaseTy::Enum(e) => types[e].generics,
        }
    }

    pub fn is_generic(self, types: &Types) -> bool {
        !self.generics(types).is_empty()
    }

    pub fn span(self, types: &Types) -> Option<Span> {
        match self {
            Self::Struct(s) => Some(types[s].loc.source_loc(types)?.span),
            Self::Enum(e) => Some(types[e].loc.source_loc(types)?.span),
        }
    }

    pub fn from_ty(ty: Ty) -> Option<Self> {
        match ty {
            Ty::Base(b) => Some(b),
            Ty::Instance(i) => Some(i.base),
            Ty::Pointer(..) | Ty::Array(..) | Ty::Param(..) | Ty::Builtin(..) => None,
        }
    }

    pub fn drop_spec(self, types: &Types) -> DropSpec {
        match self {
            Self::Struct(s) => types[s].drop_spec,
            Self::Enum(e) => types[e].drop_spec,
        }
    }
}

pub type ParamRepr = u16;

pub enum NonBaseTy<'a> {
    Pointer(Pointer<'a>),
    Array(ExpArray<'a>),
    Param(TyParam),
    Builtin(Builtin),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ExpInstance<'a, T = BaseTy> {
    pub base: T,
    pub args: &'a [Ty<'a>],
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ExpArray<'a> {
    pub item: &'a Ty<'a>,
    pub len: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Ty<'a> {
    Base(BaseTy),
    Instance(ExpInstance<'a>),
    Pointer(Pointer<'a>),
    Array(ExpArray<'a>),
    Param(TyParam),
    Builtin(Builtin),
}

impl From<FragRef<Struct>> for Ty<'_> {
    fn from(ty: FragRef<Struct>) -> Self {
        Self::Base(ty.into())
    }
}

impl From<FragRef<Enum>> for Ty<'_> {
    fn from(ty: FragRef<Enum>) -> Self {
        Self::Base(ty.into())
    }
}

impl<'a> Ty<'a> {
    pub fn significant_type(self) -> Option<BaseTy> {
        match self {
            Ty::Base(b) => Some(b),
            Ty::Instance(i) => Some(i.base),
            Ty::Pointer(p) => p.ty.significant_type(),
            Ty::Array(arr) => arr.item.significant_type(),
            Ty::Param(..) | Ty::Builtin(..) => None,
        }
    }

    pub fn drop_spec(&self, types: &Types) -> DropSpec {
        match self {
            Ty::Base(b) => b.drop_spec(types),
            Ty::Instance(i) => i.base.drop_spec(types),
            Ty::Pointer(..) => DropSpec::Copy,
            Ty::Array(arr) if arr.len == 0 => DropSpec::Copy,
            Ty::Array(arr) => arr.item.drop_spec(types),
            Ty::Param(..) => DropSpec::Hibrid,
            Ty::Builtin(..) => DropSpec::Copy,
        }
    }

    pub fn load(compact: CompactTy, types: &Types, arena: &ProxyArena<'a>) -> Self {
        match compact.expanded() {
            ExpandedTy::Builtin(b) => Self::Builtin(b),
            ExpandedTy::Array(array) => {
                let Array { len, item } = types[array];
                Self::Array(ExpArray {
                    item: arena.alloc(Self::load(item, types, arena)),
                    len,
                })
            }
            ExpandedTy::Pointer {
                depth,
                mutability,
                ty,
            } => {
                let ty = Self::load(types[ty], types, arena);
                Self::Pointer(Pointer {
                    depth,
                    mutability: mutability.to_mutability(),
                    ty: arena.alloc(ty),
                })
            }
            ExpandedTy::Base(s) => Self::Base(s),
            ExpandedTy::Instance(instance) => {
                let Instance { base, args } = types[instance];
                let args =
                    arena.alloc_iter(types[args].iter().map(|&arg| Self::load(arg, types, arena)));
                Self::Instance(ExpInstance {
                    base: base.expanded(),
                    args,
                })
            }
            ExpandedTy::Param { param, asoc } => Self::Param(TyParam { index: param, asoc }),
        }
    }

    pub fn load_slice(slice: &[CompactTy], types: &Types, arena: &ProxyArena<'a>) -> &'a [Ty<'a>] {
        arena.alloc_iter(slice.iter().map(|&ty| Self::load(ty, types, arena)))
    }

    pub fn is_aggregate(self) -> bool {
        matches!(self, Self::Base(..) | Self::Instance(..))
    }

    pub fn int_eq(self) -> Option<FragRef<Func>> {
        Some(match self {
            Ty::Builtin(b) => match b {
                Builtin::Uint => Func::UINT_EQ,
                Builtin::U32 => Func::U32_EQ,
                Builtin::U16 => Func::U16_EQ,
                Builtin::U8 => Func::U8_EQ,
                Builtin::Char => Func::U32_EQ,
                _ => return None,
            },
            _ => return None,
        })
    }

    pub fn to_base_and_params(self) -> Result<(BaseTy, &'a [Self]), NonBaseTy<'a>> {
        Err(match self {
            Ty::Base(b) => return Ok((b, &[])),
            Ty::Instance(i) => return Ok((i.base, i.args)),
            Ty::Pointer(p) => NonBaseTy::Pointer(p),
            Ty::Array(a) => NonBaseTy::Array(a),
            Ty::Param(p) => NonBaseTy::Param(p),
            Ty::Builtin(b) => NonBaseTy::Builtin(b),
        })
    }

    pub fn array_base(self) -> Option<Self> {
        match self {
            Ty::Array(a) => Some(*a.item),
            _ => None,
        }
    }

    pub fn compatible(a: Self, b: Self) -> bool {
        b == Ty::TERMINAL
            || a == Ty::TERMINAL
            || a == b
            || matches!(
                (a, b),
                (Ty::Pointer(t), Ty::Pointer(t2))
                if t.compatible(t2)
            )
    }

    pub fn as_generic(self) -> Option<BaseTy> {
        match self {
            Self::Base(b) => Some(b),
            _ => None,
        }
    }

    pub fn base_with_params(self) -> (Self, &'a [Self]) {
        match self {
            Self::Instance(i) => (i.base.as_ty(), i.args),
            _ => (self, default()),
        }
    }

    pub fn base(self) -> Option<BaseTy> {
        Some(match self {
            Self::Base(b) => b,
            Self::Instance(i) => i.base,
            _ => return None,
        })
    }

    pub fn caller_with_params(self) -> (Self, &'a [Self]) {
        self.ptr_base().base_with_params()
    }

    pub fn caller(self) -> Self {
        self.caller_with_params().0
    }

    pub fn ptr_base(self) -> Self {
        match self {
            Self::Pointer(p) => p.ty.ptr_base(),
            _ => self,
        }
    }

    pub fn mutability(self) -> Mutability {
        match self {
            Self::Pointer(p) => p.mutability,
            _ => Mutability::Mutable,
        }
    }

    pub fn ptr_depth(self) -> u8 {
        match self {
            Self::Pointer(p) => p.depth,
            _ => 0,
        }
    }

    pub fn ptr(self) -> (Self, u8, Mutability) {
        match self {
            Self::Pointer(p) => (*p.ty, p.depth, p.mutability),
            _ => (self, 0, Mutability::Immutable),
        }
    }

    pub fn is_signed(self) -> bool {
        Ty::SIGNED_INTEGERS.contains(&self)
    }

    pub fn contains_params(self) -> bool {
        !matches!(self.contains_params_low(), ParamPresence::Absent)
    }

    pub fn contains_params_low(self) -> ParamPresence {
        use ParamPresence::*;
        match self {
            Ty::Instance(instance) => instance
                .args
                .iter()
                .map(|&ty| ty.contains_params_low())
                .fold(Absent, ParamPresence::combine),
            Ty::Pointer(pointer) => pointer
                .ty
                .contains_params_low()
                .combine(pointer.mutability.into())
                .put_behind_pointer(),
            Ty::Array(array) => array.item.contains_params_low(),
            Ty::Param(..) => Present,
            Ty::Base(..) | Ty::Builtin(..) => Absent,
        }
    }

    pub fn dereference(self) -> Self {
        match self {
            Ty::Pointer(ptr, ..) => *ptr.ty,
            _ => self,
        }
    }

    pub fn infer_ty_params(self, param_count: usize, template: Ty) -> BumpVec<Ty> {
        let mut params = bumpvec![None; param_count];
        let res = self.infer(template, &mut params);
        assert!(res.is_ok());
        assert!(params.iter().all(|p| p.is_some()));
        const _: () = assert!(mem::size_of::<Option<Ty>>() == mem::size_of::<Ty>());
        unsafe { mem::transmute(params) }
    }

    pub fn infer(self, template: Self, params: &mut [Option<Self>]) -> Result<(), (Self, Self)> {
        let mut stack = bumpvec![(self, template)];

        let check = |a, b| Ty::compatible(a, b).then_some(()).ok_or((a, b));

        while let Some((reference, template)) = stack.pop() {
            if reference == template && !self.contains_params() {
                continue;
            }

            match (reference, template) {
                (Ty::Pointer(reference_p), Ty::Pointer(template_p)) => {
                    match (reference_p.mutability, template_p.mutability) {
                        (val, Mutability::Param(i)) => params[i.get()] = Some(val.as_ty()),
                        _ if reference_p.mutability.compatible(template_p.mutability) => (),
                        _ => return Err((reference, template)),
                    }
                    stack.push((*reference_p.ty, *template_p.ty));
                }
                (Ty::Instance(reference), Ty::Instance(template)) => {
                    check(reference.base.as_ty(), template.base.as_ty())?;
                    stack.extend(
                        reference.args
                            .iter()
                            .copied()
                            .zip(template.args.iter().copied()),
                    );
                }
                (_, Ty::Param(param)) if let Some(inferred) = params[param.index.get()] => {
                    check(reference, inferred)?;
                }
                (_, Ty::Param(param)) => params[param.index.get()] = Some(reference),
                _ => return Err((reference, template)),
            }
        }

        Ok(())
    }

    pub fn is_unsigned(self) -> bool {
        Ty::INTEGERS.contains(&self)
    }

    pub fn is_float(self) -> bool {
        Ty::FLOATS.contains(&self)
    }

    pub fn span(self, types: &Types) -> Option<Span> {
        self.as_generic()?.span(types)
    }
}

macro_rules! gen_builtin {
    (
        atoms {
            $(
                $name:ident => $builtin:ident => $repr:literal,
            )*
        }
        groups {
            $(
                $group_name:ident => [$($group_elem:ident)*],
            )*
        }
    ) => {
        impl Ty<'static> {
            $(
                pub const $name: Self = Self::Builtin(Builtin::$builtin);
            )*

            pub const ALL: [Self; [$(Self::$name),*].len()] = [
                $(
                    Self::$name,
                )*
            ];

            $(
                pub const $group_name: [Self; [$(Self::$group_elem),*].len()] = [
                    $(
                        Self::$group_elem,
                    )*
                ];
            )*
        }

        #[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, PartialOrd, Ord, Serialize, Deserialize, Archive)]
        #[archive_attr(derive(PartialEq, Eq, Hash))]
        pub enum Builtin {
            $($builtin),*
        }

        impl Builtin {
            pub const ALL: [Self; [$(Self::$builtin),*].len()] = [
                $(
                    Self::$builtin,
                )*
            ];

            $(
                pub const $name: Self = Builtin::$builtin;
            )*

            $(
                pub const $group_name: [Self; [$(Self::$group_elem),*].len()] = [
                    $(
                        Self::$group_elem,
                    )*
                ];
            )*

            pub fn name(self) -> &'static str {
                match self {
                    $(Builtin::$builtin => $repr),*
                }
            }
        }
    };
}

gen_builtin!(
    atoms {
        UNIT => Unit => "()",
        TERMINAL => Terminal => "!",
        UINT => Uint => "uint",
        U32 => U32 => "u32",
        U16 => U16 => "u16",
        U8 => U8 => "u8",
        CHAR => Char => "char",
        BOOL => Bool => "bool",
        F32 => F32 => "f32",
        F64 => F64 => "f64",
        // c abi
        SHORT => Short => "short",
        CINT => Cint => "cint",
        LONG => Long => "long",
        LONGLONG => LongLong => "longlong",
    }

    groups {
        SCALARS => [UINT U32 U16 U8 CHAR BOOL F32 F64 SHORT CINT LONG LONGLONG],
        BINARY => [UINT U32 U16 U8 BOOL],
        INTEGERS => [UINT U32 U16 U8],
        SIGNED_INTEGERS => [CINT],
        FLOATS => [F32 F64],
        CTYPES => [SHORT CINT LONG LONGLONG],
    }
);

impl Builtin {
    pub fn is_signed(self) -> bool {
        false
    }
}

impl Default for Ty<'_> {
    fn default() -> Self {
        Ty::UNIT
    }
}

bitflags! {
    TyFlags: u8 {
        GENERIC
    }
}

pub trait Humid: Sized + Clone + NoInteriorMutability {
    const NAMES: &'static [&'static str];
    fn is_water_drop(key: FragRef<Self>) -> bool;
    fn lookup_water_drop(key: &str) -> Option<FragRef<Self>>;
    fn name(&self) -> Ident;
    fn storage(types: &mut Types) -> &mut FragMap<Self>;
}

use std::{
    default::default,
    fmt::{self, Display},
};

use crate::*;
use span::*;

use rkyv::{Archive, Deserialize, Serialize};
use storage::*;

pub mod data;
pub mod pointer;
pub mod spec;

pub type Generics = FragSlice<FragSlice<Spec>>;

wrapper_enum! {
    #[derive(
        Clone, Copy, PartialEq, Eq, Hash, Debug, PartialOrd, Ord, Deserialize, Archive, Serialize,
    )]
    #[archive_attr(derive(PartialEq, Eq, Hash))]
    enum Spec: relocated {
        Base: FragRef<SpecBase>,
        Instance: FragRef<SpecInstance>,
    }
}

impl Spec {
    pub fn base(self, types: &Types) -> FragRef<SpecBase> {
        match self {
            Spec::Base(base) => base,
            Spec::Instance(instance) => types[instance].base,
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
    pub fn as_ty(self) -> Ty {
        self.into()
    }

    pub fn is_generic(self, types: &Types) -> bool {
        !match self {
            BaseTy::Struct(s) => types[s].generics.is_empty(),
            BaseTy::Enum(e) => types[e].generics.is_empty(),
        }
    }

    pub fn span(self, types: &Types) -> Option<Span> {
        match self {
            Self::Struct(s) => Some(types[s].loc.source_loc(types)?.span),
            Self::Enum(e) => Some(types[e].loc.source_loc(types)?.span),
        }
    }

    pub fn from_ty(ty: Ty, types: &Types) -> Option<Self> {
        match ty {
            Ty::Base(b) => Some(b),
            Ty::Instance(i) => Some(types[i].base.into()),
            Ty::Pointer(..) | Ty::Array(..) | Ty::Param(..) | Ty::Builtin(..) => None,
        }
    }
}

wrapper_enum! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Archive)]
    enum ComputedTypecItem: relocated {
        Pointer: FragRef<Ty>,
        Instance: FragRef<Instance>,
        SpecInstance: FragRef<SpecInstance>,
        SpecSum: FragSlice<Spec>,
        Array: FragRef<Array>,
    }
}

pub type ParamRepr = u16;

pub enum NonBaseTy {
    Pointer(Pointer),
    Array(FragRef<Array>),
    Param(ParamRepr),
    Builtin(Builtin),
}

wrapper_enum! {
    #[derive(
        Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize, Archive,
    )]
    #[archive_attr(derive(PartialEq, Eq, Hash))]
    enum Ty: {
        Base: BaseTy,
        Instance: FragRef<Instance>,
        Pointer: Pointer,
        Array: FragRef<Array>,
        Param: ParamRepr,
        Builtin: Builtin,
    }
}

impl From<FragRef<Struct>> for Ty {
    fn from(ty: FragRef<Struct>) -> Self {
        Self::Base(ty.into())
    }
}

impl From<FragRef<Enum>> for Ty {
    fn from(ty: FragRef<Enum>) -> Self {
        Self::Base(ty.into())
    }
}

derive_relocated! {
    enum Ty {
        Base(b) => b,
        Instance(i) => i,
        Pointer(p) => p,
        Array(a) => a,
        Param(..) =>,
        Builtin(..) =>,
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ty::Base(b) => write!(f, "{b}"),
            Ty::Instance(i) => write!(f, "inst{:x}", i.bits()),
            Ty::Pointer(p) => {
                write!(f, "{} ptr{:x}", p.mutability.to_mutability(), p.ty().bits())
            }
            Ty::Param(i) => write!(f, "param{i}"),
            Ty::Builtin(b) => write!(f, "{}", b.name()),
            Ty::Array(a) => write!(f, "array{:x}", a.bits()),
        }
    }
}

impl Ty {
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

    pub fn to_base_and_params(self, types: &Types) -> Result<(BaseTy, FragSlice<Ty>), NonBaseTy> {
        Err(match self {
            Ty::Base(b) => return Ok((b, FragSlice::empty())),
            Ty::Instance(i) => return Ok((types[i].base, types[i].args)),
            Ty::Pointer(p) => NonBaseTy::Pointer(p),
            Ty::Array(a) => NonBaseTy::Array(a),
            Ty::Param(p) => NonBaseTy::Param(p),
            Ty::Builtin(b) => NonBaseTy::Builtin(b),
        })
    }

    pub fn array_base(self, types: &Types) -> Option<Ty> {
        match self {
            Ty::Array(a) => Some(types[a].item),
            _ => None,
        }
    }

    pub fn compatible(a: Self, b: Self) -> bool {
        b == Self::TERMINAL
            || a == Self::TERMINAL
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

    pub fn base_with_params(self, types: &Types) -> (Self, FragSlice<Ty>) {
        match self {
            Self::Instance(i) => (types[i].base.as_ty(), types[i].args),
            _ => (self, default()),
        }
    }

    pub fn base(self, types: &Types) -> Self {
        self.base_with_params(types).0
    }

    pub fn caller_with_params(self, types: &Types) -> (Self, FragSlice<Ty>) {
        self.ptr_base(types).base_with_params(types)
    }

    pub fn caller(self, types: &Types) -> Self {
        self.caller_with_params(types).0
    }

    pub fn ptr_base(self, types: &Types) -> Self {
        match self {
            Self::Pointer(p) => types[p.ty()].ptr_base(types),
            _ => self,
        }
    }

    pub fn mutability(self) -> RawMutability {
        match self {
            Self::Pointer(p) => p.mutability,
            _ => RawMutability::IMMUTABLE,
        }
    }

    pub fn ptr_depth(self) -> u8 {
        match self {
            Self::Pointer(p) => p.depth,
            _ => 0,
        }
    }

    pub fn ptr(self, types: &Types) -> (Self, u8, Mutability) {
        match self {
            Self::Pointer(p) => (types[p.ty()], p.depth, p.mutability.to_mutability()),
            _ => (self, 0, Mutability::Immutable),
        }
    }

    pub fn is_signed(self) -> bool {
        Ty::SIGNED_INTEGERS.contains(&self)
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
        impl Ty {
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
        MUTABLE => Mutable => "mutable",
        IMMUTABLE => Immutable => "immutable",
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

impl Default for Ty {
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

#[derive(Default)]
pub struct SpecSet {
    storage: Vec<(u32, Spec)>,
}

impl SpecSet {
    pub fn extend(&mut self, index: u32, specs: impl IntoIterator<Item = Spec>) {
        for spec in specs {
            if let Err(i) = self.storage.binary_search(&(index, spec)) {
                self.storage.insert(i, (index, spec));
            }
        }
    }

    pub fn truncate(&mut self, length: usize) {
        self.storage.truncate(length);
    }

    pub fn iter(
        &self,
    ) -> impl Iterator<
        Item = (
            u32,
            impl Iterator<Item = Spec> + '_ + Clone + ExactSizeIterator,
        ),
    >
           + '_
           + DoubleEndedIterator {
        self.storage
            .group_by(|(a, ..), (b, ..)| a == b)
            .map(|group| (group[0].0, group.iter().map(|&(.., s)| s)))
    }

    pub fn clear(&mut self) {
        self.storage.clear();
    }
}

use std::fmt::{self, Display};

use crate::*;
use lexing_t::Span;
use parsing_t::Vis;
use storage::*;

pub type TypecLookup = Map<VRef<str>, ComputedTypecItem>;
pub type ImplLookup = Map<ImplKey, Option<VRef<Impl>>>;

pub type ParamSlices = BumpMap<VSlice<Spec>>;
pub type SpecSums = BumpMap<Spec>;
pub type ArgSlices = BumpMap<Ty>;
pub type Fields = BumpMap<Field>;
pub type SpecFuncs = BumpMap<SpecFunc>;

pub type Impls = PushMap<Impl>;
pub type Instances = PushMap<Instance>;
pub type Structs = PushMap<Struct>;
pub type Pointers = PushMap<Pointer>;
pub type BaseSpecs = PushMap<SpecBase>;
pub type SpecInstances = PushMap<SpecInstance>;

#[derive(Clone, Copy)]
pub struct Impl {
    pub generics: Generics,
    pub key: ImplKey,
    pub methods: VRefSlice<Func>,
    pub next: Option<VRef<Impl>>,
    pub span: Option<Span>,
}

impl Impl {
    gen_increasing_constants!(ANY);
}

pub type Generics = VSlice<VSlice<Spec>>;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ImplKey {
    pub ty: Ty,
    pub spec: Spec,
}

#[derive(Clone, Copy)]
pub struct Instance {
    pub base: GenericTy,
    pub args: VSlice<Ty>,
}

#[derive(Clone, Copy)]
pub struct Struct {
    pub name: VRef<str>,
    pub generics: Generics,
    pub fields: VSlice<Field>,
    pub loc: Loc,
}

#[derive(Clone, Copy)]
pub struct Pointer {
    pub base: Ty,
    pub mutability: Mutability,
    pub depth: u16,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Mutability {
    Mutable,
    Immutable,
    Param(u16),
}

impl fmt::Display for Mutability {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Mutability::Mutable => write!(f, "mut "),
            Mutability::Immutable => write!(f, ""),
            Mutability::Param(i) => write!(f, "param{} ", i),
        }
    }
}

#[derive(Clone, Copy)]
pub struct SpecBase {
    pub name: VRef<str>,
    pub generics: Generics,
    pub methods: VSlice<SpecFunc>,
    pub loc: Loc,
}

#[derive(Clone, Copy)]
pub struct SpecInstance {
    pub base: VRef<SpecBase>,
    pub args: VSlice<Ty>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Spec {
    Base(VRef<SpecBase>),
    Instance(VRef<SpecInstance>),
}

impl Spec {
    pub fn base(self, typec: &Typec) -> VRef<SpecBase> {
        match self {
            Spec::Base(base) => base,
            Spec::Instance(instance) => typec.spec_instances[instance].base,
        }
    }
}

#[derive(Clone, Copy)]
pub enum GenericTy {
    Struct(VRef<Struct>),
}

impl GenericTy {
    pub fn as_ty(self) -> Ty {
        match self {
            GenericTy::Struct(s) => Ty::Struct(s),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ComputedTypecItem {
    Pointer(VRef<Pointer>),
    Instance(VRef<Instance>),
    SpecInstance(VRef<SpecInstance>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Ty {
    Struct(VRef<Struct>),
    Instance(VRef<Instance>),
    Pointer(VRef<Pointer>),
    Param(u16),
    Builtin(Builtin),
}

impl Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ty::Struct(s) => write!(f, "struct{}", s.index()),
            Ty::Instance(i) => write!(f, "inst{}", i.index()),
            Ty::Pointer(p) => write!(f, "ptr{}", p.index()),
            Ty::Param(i) => write!(f, "param{}", i),
            Ty::Builtin(b) => write!(f, "{}", b.name()),
        }
    }
}

impl Ty {
    pub fn compatible(a: Self, b: Self) -> bool {
        b == Self::TERMINAL || a == b
    }

    pub fn as_generic(self) -> Option<GenericTy> {
        Some(match self {
            Self::Struct(s) => GenericTy::Struct(s),
            _ => return None,
        })
    }

    pub fn base(self, typec: &Typec) -> Self {
        match self {
            Self::Instance(i) => typec[i].base.as_ty(),
            _ => self,
        }
    }

    pub fn caller(self, typec: &Typec) -> Self {
        self.ptr_base(typec).base(typec)
    }

    pub fn ptr_base(self, typec: &Typec) -> Self {
        match self {
            Self::Pointer(p) => typec[p].base.ptr_base(typec),
            _ => self,
        }
    }

    pub fn ptr_depth(self, typec: &Typec) -> u16 {
        match self {
            Self::Pointer(p) => typec[p].depth,
            _ => 0,
        }
    }

    pub fn is_signed(self) -> bool {
        match self {
            Self::Builtin(b) => b.is_signed(),
            _ => false,
        }
    }

    pub fn span(self, typec: &Typec) -> Option<Span> {
        match self {
            Self::Struct(s) => {
                Some(typec.module_items[typec[s].loc.module][typec[s].loc.item].span)
            }
            Self::Instance(..) | Self::Pointer(..) | Self::Param(..) | Self::Builtin(..) => None,
        }
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

        #[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
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
        UINT => Uint => "uint",
        U32 => U32 => "u32",
        CHAR => Char => "char",
        BOOL => Bool => "bool",
    }

    groups {
        INTEGERS => [UINT U32],
    }
);

impl Builtin {
    pub fn is_signed(self) -> bool {
        match self {
            Builtin::Unit
            | Builtin::Terminal
            | Builtin::Uint
            | Builtin::U32
            | Builtin::Char
            | Builtin::Bool => false,
        }
    }

    pub fn size(self) -> u32 {
        match self {
            Builtin::Unit => 0,
            Builtin::Terminal => 0,
            Builtin::Uint => 8,
            Builtin::U32 => 4,
            Builtin::Char => 4,
            Builtin::Bool => 1,
        }
    }
}

impl Default for Ty {
    fn default() -> Self {
        Ty::UNIT
    }
}

// gen_kind!(TyKind
//     Instance = TyInstance {
//         base: Ty,
//         args: VRefSlice<Ty>,
//     },
//     Struct = TyStruct {
//         generics: VRefSlice<Ty>,
//         fields: VSlice<Field>,
//     },
//     Pointer = TyPointer {
//         base: Ty,
//         mutability: Ty,
//         depth: u32,
//     },
//     Integer = TyInteger {
//         size: u8,
//         signed: bool,
//     },
//     Spec = TySpec {
//         inherits: VRefSlice<Ty>,
//         generics: VRefSlice<Ty>,
//         methods: VSlice<SpecFunc>,
//     },
//     Param = u32,
//     Bool,
// );

// impl Default for TyKind {
//     fn default() -> Self {
//         Self::Struct(default())
//     }
// }

#[derive(Clone, Copy)]
pub struct SpecFunc {
    pub generics: Generics,
    pub signature: Signature,
    pub name: VRef<str>,
    pub span: Option<Span>,
    pub parent: VRef<SpecBase>,
}

impl SpecFunc {
    pub fn is_generic(&self) -> bool {
        !self.generics.is_empty()
    }
}

#[derive(Clone, Copy)]
pub struct Field {
    pub vis: Vis,
    pub ty: Ty,
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

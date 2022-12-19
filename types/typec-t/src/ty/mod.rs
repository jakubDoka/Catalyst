use std::{
    default::default,
    fmt::{self, Display},
};

use crate::*;
use lexing_t::Span;
use parsing_t::Vis;

use storage::*;

pub type TypecLookup = CMap<Ident, ComputedTypecItem>;
pub type ImplLookup = CMap<(FragRef<SpecBase>, Ty), SmallVec<[FragRef<Impl>; 4]>>;
pub type Implemented = CMap<ImplKey, (FragRef<Impl>, FragSlice<Ty>)>;

pub type ParamSlices = FragMap<FragSlice<Spec>, MAX_FRAGMENT_SIZE>;
pub type SpecSums = FragMap<Spec, MAX_FRAGMENT_SIZE>;
pub type ArgSlices = FragMap<Ty, MAX_FRAGMENT_SIZE>;
pub type Fields = FragMap<Field, MAX_FRAGMENT_SIZE>;
pub type SpecFuncs = FragMap<SpecFunc, MAX_FRAGMENT_SIZE>;
pub type Variants = FragMap<Variant, MAX_FRAGMENT_SIZE>;

pub type Impls = FragMap<Impl, MAX_FRAGMENT_SIZE>;
pub type Instances = FragMap<Instance, MAX_FRAGMENT_SIZE>;
pub type Enums = FragMap<Enum, MAX_FRAGMENT_SIZE>;
pub type Pointers = FragMap<Pointer, MAX_FRAGMENT_SIZE>;
pub type BaseSpecs = FragMap<SpecBase, MAX_FRAGMENT_SIZE>;
pub type SpecInstances = FragMap<SpecInstance, MAX_FRAGMENT_SIZE>;

#[derive(Clone, Copy)]
pub struct Impl {
    pub generics: Generics,
    pub key: ImplKey,
    pub methods: FragRefSlice<Func>,
    pub loc: Loc,
}

impl Impl {
    gen_v_ref_constants!(ANY);
}

pub type Generics = FragSlice<FragSlice<Spec>>;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
pub struct ImplKey {
    pub ty: Ty,
    pub spec: Spec,
}

#[derive(Clone, Copy)]
pub struct Instance {
    pub base: GenericTy,
    pub args: FragSlice<Ty>,
}

#[derive(Clone, Copy, Default)]
pub struct Struct {
    pub name: Ident,
    pub generics: Generics,
    pub fields: FragSlice<Field>,
    pub loc: Option<Loc>,
}

impl Struct {
    pub fn find_field(s: FragRef<Self>, name: Ident, typec: &Typec) -> Option<(usize, Field)> {
        typec[typec[s].fields]
            .iter()
            .enumerate()
            .find_map(|(i, &v)| (v.name == name).then_some((i, v)))
    }
}

gen_water_drops! {
    Struct
    structs
    MACRO_LEXER => "MacroLexer",
}

#[derive(Clone, Copy, Default)]
pub struct Enum {
    pub name: Ident,
    pub generics: Generics,
    pub variants: FragSlice<Variant>,
    pub loc: Option<Loc>,
}

impl Enum {
    pub fn find_variant(e: FragRef<Self>, name: Ident, typec: &Typec) -> Option<(usize, Ty)> {
        typec[typec[e].variants]
            .iter()
            .enumerate()
            .find_map(|(i, v)| (v.name == name).then_some((i, v.ty)))
    }
}

gen_water_drops! {
    Enum
    enums
    OPTION => "Option",
    MACRO_TOKEN_KIND => "MacroTokenKind",
}

#[derive(Clone, Copy)]
pub struct Variant {
    pub name: Ident,
    pub ty: Ty,
    pub span: Option<Span>,
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
            Mutability::Param(i) => write!(f, "param{i} "),
        }
    }
}

#[derive(Clone, Copy, Default)]
pub struct SpecBase {
    pub name: Ident,
    pub generics: Generics,
    pub inherits: FragSlice<Spec>,
    pub methods: FragSlice<SpecFunc>,
    pub loc: Option<Loc>,
}

impl SpecBase {
    pub fn is_macro(s: FragRef<Self>) -> bool {
        s <= Self::TOKEN_MACRO
    }
}

gen_water_drops! {
    SpecBase
    base_specs
    DROP => "Drop",
    COPY => "Copy",
    TOKEN_MACRO => "TokenMacro",
}

#[derive(Clone, Copy)]
pub struct SpecInstance {
    pub base: FragRef<SpecBase>,
    pub args: FragSlice<Ty>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
pub enum Spec {
    Base(FragRef<SpecBase>),
    Instance(FragRef<SpecInstance>),
}

impl Spec {
    pub fn base(self, typec: &Typec) -> FragRef<SpecBase> {
        match self {
            Spec::Base(base) => base,
            Spec::Instance(instance) => typec.spec_instances[instance].base,
        }
    }
}

impl From<FragRef<SpecBase>> for Spec {
    fn from(base: FragRef<SpecBase>) -> Self {
        Spec::Base(base)
    }
}

impl From<FragRef<SpecInstance>> for Spec {
    fn from(instance: FragRef<SpecInstance>) -> Self {
        Spec::Instance(instance)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GenericTy {
    Struct(FragRef<Struct>),
    Enum(FragRef<Enum>),
}

impl GenericTy {
    pub fn as_ty(self) -> Ty {
        match self {
            GenericTy::Struct(s) => Ty::Struct(s),
            GenericTy::Enum(e) => Ty::Enum(e),
        }
    }

    pub fn is_generic(self, typec: &Typec) -> bool {
        !match self {
            GenericTy::Struct(s) => typec.structs[s].generics.is_empty(),
            GenericTy::Enum(e) => typec.enums[e].generics.is_empty(),
        }
    }
}

impl From<FragRef<Struct>> for GenericTy {
    fn from(s: FragRef<Struct>) -> Self {
        GenericTy::Struct(s)
    }
}

impl From<FragRef<Enum>> for GenericTy {
    fn from(e: FragRef<Enum>) -> Self {
        GenericTy::Enum(e)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ComputedTypecItem {
    Pointer(FragRef<Pointer>),
    Instance(FragRef<Instance>),
    SpecInstance(FragRef<SpecInstance>),
    SpecSum(FragSlice<Spec>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Ty {
    Struct(FragRef<Struct>),
    Enum(FragRef<Enum>),
    Instance(FragRef<Instance>),
    Pointer(FragRef<Pointer>),
    Param(u16),
    Builtin(Builtin),
}

impl Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ty::Struct(s) => write!(f, "struct{:X}", s.to_u32()),
            Ty::Enum(e) => write!(f, "enum{:x}", e.to_u32()),
            Ty::Instance(i) => write!(f, "inst{:x}", i.to_u32()),
            Ty::Pointer(p) => write!(f, "ptr{:x}", p.to_u32()),
            Ty::Param(i) => write!(f, "param{i}"),
            Ty::Builtin(b) => write!(f, "{}", b.name()),
        }
    }
}

impl Ty {
    pub fn component_ty(
        self,
        index: usize,
        typec: &mut Typec,
        interner: &mut Interner,
    ) -> Option<Ty> {
        Some(match self {
            Ty::Struct(s) => typec[typec[s].fields][index].ty,
            Ty::Enum(e) => typec[typec[e].variants][index].ty,
            Ty::Instance(i) => {
                let Instance { base, args } = typec[i];
                let ty = match base {
                    GenericTy::Struct(s) => typec[typec[s].fields][index].ty,
                    GenericTy::Enum(e) => typec[typec[e].variants][index].ty,
                };
                typec.instantiate(ty, args, interner)
            }
            _ => return None,
        })
    }

    pub fn find_component(
        self,
        name: Ident,
        typec: &mut Typec,
        interner: &mut Interner,
    ) -> Option<(usize, Ty)> {
        match self {
            Ty::Struct(s) => Struct::find_field(s, name, typec).map(|(i, f)| (i, f.ty)),
            Ty::Enum(e) => Enum::find_variant(e, name, typec),
            Ty::Instance(i) => {
                let Instance { base, args } = typec[i];
                let (index, ty) = match base {
                    GenericTy::Struct(s) => {
                        Struct::find_field(s, name, typec).map(|(i, f)| (i, f.ty))
                    }
                    GenericTy::Enum(e) => Enum::find_variant(e, name, typec),
                }?;
                Some((index, typec.instantiate(ty, args, interner)))
            }
            _ => None,
        }
    }

    pub fn int_eq(self) -> Option<FragRef<Func>> {
        Some(match self {
            Ty::Builtin(b) => match b {
                Builtin::Unit => todo!(),
                Builtin::Terminal => todo!(),
                Builtin::Uint => Func::UINT_EQ,
                Builtin::U32 => Func::U32_EQ,
                Builtin::U16 => Func::U16_EQ,
                Builtin::U8 => Func::U8_EQ,
                Builtin::Char => Func::U32_EQ,
                Builtin::Bool => todo!(),
            },
            _ => return None,
        })
    }

    pub fn compatible(a: Self, b: Self) -> bool {
        b == Self::TERMINAL || a == Self::TERMINAL || a == b
    }

    pub fn as_generic(self) -> Option<GenericTy> {
        Some(match self {
            Self::Struct(s) => GenericTy::Struct(s),
            Self::Enum(e) => GenericTy::Enum(e),
            _ => return None,
        })
    }

    pub fn base_with_params(self, typec: &Typec) -> (Self, FragSlice<Ty>) {
        match self {
            Self::Instance(i) => (typec[i].base.as_ty(), typec[i].args),
            _ => (self, default()),
        }
    }

    pub fn base(self, typec: &Typec) -> Self {
        self.base_with_params(typec).0
    }

    pub fn caller_with_params(self, typec: &Typec) -> (Self, FragSlice<Ty>) {
        self.ptr_base(typec).base_with_params(typec)
    }

    pub fn caller(self, typec: &Typec) -> Self {
        self.caller_with_params(typec).0
    }

    pub fn ptr_base(self, typec: &Typec) -> Self {
        match self {
            Self::Pointer(p) => typec[p].base.ptr_base(typec),
            _ => self,
        }
    }

    pub fn mutability(self, typec: &Typec) -> Mutability {
        match self {
            Self::Pointer(p) => typec[p].mutability,
            _ => Mutability::Immutable,
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
                Some(typec.module_items[typec[s].loc?.module].items[typec[s].loc?.item].span)
            }
            Self::Enum(e) => {
                Some(typec.module_items[typec[e].loc?.module].items[typec[e].loc?.item].span)
            }
            Self::Instance(..) | Self::Pointer(..) | Self::Param(..) | Self::Builtin(..) => None,
        }
    }

    /// None - don't know
    /// Some(None) - not drop
    /// Some(Some(..)) - drop
    pub fn is_drop(
        self,
        params: &[FragSlice<Spec>],
        typec: &mut Typec,
        interner: &mut Interner,
    ) -> Option<Option<(FragRef<Impl>, FragSlice<Ty>)>> {
        match self {
            Ty::Pointer(..) | Ty::Builtin(..) => Some(None),
            Ty::Param(..) => typec
                .find_implementation(
                    self,
                    Spec::Base(SpecBase::COPY),
                    params,
                    &mut None,
                    interner,
                )
                .map(|_| None),
            Ty::Struct(..) | Ty::Enum(..) | Ty::Instance(..) => Some(
                typec
                    .find_implementation(
                        self,
                        Spec::Base(SpecBase::DROP),
                        params,
                        &mut None,
                        interner,
                    )
                    .flatten(),
            ),
        }
    }

    pub fn is_copy(
        self,
        params: &[FragSlice<Spec>],
        typec: &mut Typec,
        interner: &mut Interner,
    ) -> bool {
        match self {
            Ty::Pointer(..) | Ty::Builtin(..) => true,
            Ty::Struct(_) | Ty::Enum(_) | Ty::Instance(_) | Ty::Param(_) => typec
                .find_implementation(
                    self,
                    Spec::Base(SpecBase::COPY),
                    params,
                    &mut None,
                    interner,
                )
                .is_some(),
        }
    }
}

impl From<FragRef<Struct>> for Ty {
    fn from(s: FragRef<Struct>) -> Self {
        Ty::Struct(s)
    }
}

impl From<FragRef<Enum>> for Ty {
    fn from(e: FragRef<Enum>) -> Self {
        Ty::Enum(e)
    }
}

impl From<FragRef<Instance>> for Ty {
    fn from(i: FragRef<Instance>) -> Self {
        Ty::Instance(i)
    }
}

impl From<FragRef<Pointer>> for Ty {
    fn from(p: FragRef<Pointer>) -> Self {
        Ty::Pointer(p)
    }
}

impl From<Builtin> for Ty {
    fn from(b: Builtin) -> Self {
        Ty::Builtin(b)
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

        #[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
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
        U16 => U16 => "u16",
        U8 => U8 => "u8",
        CHAR => Char => "char",
        BOOL => Bool => "bool",
    }

    groups {
        SCALARS => [UINT U32 U16 U8 CHAR BOOL],
        BINARY => [UINT U32 U16 U8 BOOL],
        INTEGERS => [UINT U32 U16 U8],
    }
);

impl Builtin {
    pub fn is_signed(self) -> bool {
        false
    }

    pub fn size(self) -> u32 {
        match self {
            Builtin::Unit => 0,
            Builtin::Terminal => 0,
            Builtin::Uint => 8,
            Builtin::U32 => 4,
            Builtin::U16 => 2,
            Builtin::U8 => 1,
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

#[derive(Clone, Copy)]
pub struct SpecFunc {
    pub generics: Generics,
    pub signature: Signature,
    pub name: Ident,
    pub span: Span,
    pub parent: FragRef<SpecBase>,
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
    pub span: Span,
    pub name: Ident,
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

pub trait Humid: Sized {
    const NAMES: &'static [&'static str];
    fn is_water_drop(key: FragRef<Self>) -> bool;
    fn lookup_water_drop(key: &str) -> Option<FragRef<Self>>;
    fn name(&self) -> Ident;
    fn storage(typec: &mut Typec) -> &mut FragMap<Self, MAX_FRAGMENT_SIZE>;
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

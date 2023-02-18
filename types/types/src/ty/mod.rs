use std::{
    default::default,
    fmt::{self, Display},
};

use crate::*;
use lexing_t::*;

use rkyv::{Archive, Deserialize, Serialize};
use storage::*;

#[derive(Archive, Serialize, Deserialize)]
pub struct Const {
    pub name: Ident,
    pub ty: Ty,
    pub value: FolderValue,
    pub loc: Loc,
}

derive_relocated!(struct Const { ty });

#[derive(Clone, Copy, Serialize, Deserialize, Archive)]

pub struct Impl {
    pub generics: Generics,
    pub key: ImplKey,
    pub methods: FragRefSlice<Func>,
    pub loc: GuaranteedLoc,
}

derive_relocated!(struct Impl { generics key methods });

impl Impl {
    gen_v_ref_constants!(ANY);
}

pub type Generics = FragSlice<FragSlice<Spec>>;

#[derive(
    Clone, Copy, PartialEq, Eq, Hash, Debug, PartialOrd, Ord, Serialize, Deserialize, Archive,
)]
#[archive_attr(derive(PartialEq, Eq, Hash))]
pub struct ImplKey {
    pub ty: Ty,
    pub spec: Spec,
}

derive_relocated!(struct ImplKey { ty spec });

#[derive(Clone, Copy, Serialize, Deserialize, Archive)]

pub struct Instance {
    pub base: GenericTy,
    pub args: FragSlice<Ty>,
}

derive_relocated!(struct Instance { base args });

#[derive(Clone, Copy, Default, Serialize, Deserialize, Archive)]

pub struct Struct {
    pub name: Ident,
    pub generics: Generics,
    pub fields: FragSlice<Field>,
    pub loc: Loc,
}

derive_relocated!(struct Struct { generics fields });

impl Struct {
    pub fn find_field(s: FragRef<Self>, name: Ident, types: &Types) -> Option<(usize, Field)> {
        types[types[s].fields]
            .iter()
            .enumerate()
            .find_map(|(i, &v)| (v.name == name).then_some((i, v)))
    }
}

gen_water_drops! {
    Struct
    structs
    LEXER => "Lexer",
}

#[derive(Clone, Copy, Default, Serialize, Deserialize, Archive)]

pub struct Enum {
    pub name: Ident,
    pub generics: Generics,
    pub variants: FragSlice<Variant>,
    pub loc: Loc,
}

derive_relocated!(struct Enum { generics variants });

impl Enum {
    pub fn find_variant(e: FragRef<Self>, name: Ident, types: &Types) -> Option<(usize, Ty)> {
        types[types[e].variants]
            .iter()
            .enumerate()
            .find_map(|(i, v)| (v.name == name).then_some((i, v.ty)))
    }
}

gen_water_drops! {
    Enum
    enums
    OPTION => "Option",
    TOKEN_KIND => "TokenKind",
}

#[derive(Clone, Copy, Serialize, Deserialize, Archive)]

pub struct Variant {
    pub name: Ident,
    pub ty: Ty,
    pub span: Option<Span>,
}

derive_relocated!(struct Variant { ty });

#[derive(
    Clone, Serialize, Deserialize, Archive, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug,
)]
#[archive_attr(derive(PartialEq, Eq, Hash))]
pub struct Pointer {
    index: u32,
    thread: u8,
    pub mutability: RawMutability,
    pub depth: u8,
}

impl Pointer {
    pub fn new(ty: FragRef<Ty>, mutability: RawMutability, depth: u8) -> Self {
        let FragAddr { index, thread, .. } = ty.addr();
        Self {
            index,
            thread,
            mutability,
            depth,
        }
    }

    pub fn ty(self) -> FragRef<Ty> {
        FragRef::new(FragAddr::new(self.index, self.thread))
    }

    pub fn compatible(self, other: Self) -> bool {
        self.index == other.index
            && self.thread == other.thread
            && self.mutability.compatible(other.mutability)
    }
}

impl Relocated for Pointer {
    fn mark(&self, marker: &mut FragRelocMarker) {
        marker.mark(self.ty());
    }

    fn remap(&mut self, ctx: &FragMarks) -> Option<()> {
        let FragSliceAddr { index, thread, .. } = ctx.project(self.ty().as_slice())?.addr();
        self.index = index;
        self.thread = thread;
        Some(())
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Mutability {
    Mutable,
    Immutable,
    Param(u8),
}

impl Mutability {
    pub fn as_ty(self) -> Ty {
        match self {
            Mutability::Mutable => Ty::MUTABLE,
            Mutability::Immutable => Ty::IMMUTABLE,
            Mutability::Param(i) => Ty::Param(i),
        }
    }
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

#[derive(
    Clone, Deserialize, Archive, Serialize, Copy, Debug, PartialEq, Eq, Ord, PartialOrd, Hash,
)]
#[archive_attr(derive(PartialEq, Eq, Hash))]
pub struct RawMutability(u8);

impl RawMutability {
    pub const MUTABLE: Self = Self(0);
    pub const IMMUTABLE: Self = Self(1);
    pub const PARAM_OFFSET: u8 = 2;
    pub fn new(mutability: Mutability) -> Option<Self> {
        Some(match mutability {
            Mutability::Mutable => Self::MUTABLE,
            Mutability::Immutable => Self::IMMUTABLE,
            Mutability::Param(i) => Self(i.checked_add(Self::PARAM_OFFSET)?),
        })
    }

    pub fn compatible(self, other: Self) -> bool {
        self == other || other == Self::IMMUTABLE
    }

    pub fn from_ty(ty: Ty) -> Self {
        match ty {
            Ty::Builtin(Builtin::Mutable) => Self::MUTABLE,
            Ty::Param(i) => Self(i + Self::PARAM_OFFSET),
            _ => Self::IMMUTABLE,
        }
    }

    pub fn to_mutability(self) -> Mutability {
        match self {
            Self::IMMUTABLE => Mutability::Immutable,
            Self::MUTABLE => Mutability::Mutable,
            Self(i) => Mutability::Param(i - Self::PARAM_OFFSET),
        }
    }

    pub fn instantiate(self, params: &[Ty]) -> RawMutability {
        match self {
            Self::IMMUTABLE => Self::IMMUTABLE,
            Self::MUTABLE => Self::MUTABLE,
            Self(i) => Self::from_ty(params[(i - Self::PARAM_OFFSET) as usize]),
        }
    }

    pub fn try_instantiate(self, params: &[Option<Ty>]) -> Option<RawMutability> {
        Some(match self {
            Self::IMMUTABLE => Self::IMMUTABLE,
            Self::MUTABLE => Self::MUTABLE,
            Self(i) => Self::from_ty(params[(i - Self::PARAM_OFFSET) as usize]?),
        })
    }
}

#[derive(Clone, Copy, Default, Deserialize, Archive, Serialize)]

pub struct SpecBase {
    pub name: Ident,
    pub generics: Generics,
    pub inherits: FragSlice<Spec>,
    pub methods: FragSlice<SpecFunc>,
    pub loc: Loc,
}

derive_relocated!(struct SpecBase { generics inherits methods });

impl SpecBase {
    pub fn is_macro(_s: FragRef<Self>) -> bool {
        false
    }
}

gen_water_drops! {
    SpecBase
    base_specs
    DROP => "Drop",
    COPY => "Copy",
}

#[derive(Clone, Copy, Deserialize, Archive, Serialize)]

pub struct SpecInstance {
    pub base: FragRef<SpecBase>,
    pub args: FragSlice<Ty>,
}

derive_relocated! {
    struct SpecInstance { base args }
}

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
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Archive, Serialize)]
    enum GenericTy: relocated {
        Struct: FragRef<Struct>,
        Enum: FragRef<Enum>,
    }
}

impl GenericTy {
    pub fn as_ty(self) -> Ty {
        match self {
            GenericTy::Struct(s) => Ty::Struct(s),
            GenericTy::Enum(e) => Ty::Enum(e),
        }
    }

    pub fn is_generic(self, types: &Types) -> bool {
        !match self {
            GenericTy::Struct(s) => types[s].generics.is_empty(),
            GenericTy::Enum(e) => types[e].generics.is_empty(),
        }
    }
}

pub type ArraySize = u32;

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Deserialize, Archive, Serialize,
)]
#[archive_attr(derive(PartialEq, Eq, Hash))]
pub struct Array {
    pub item: Ty,
    pub len: ArraySize,
}

derive_relocated!(
    struct Array {}
);

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

wrapper_enum! {
    #[derive(
        Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize, Archive,
    )]
    #[archive_attr(derive(PartialEq, Eq, Hash))]
    enum Ty: {
        Struct: FragRef<Struct>,
        Enum: FragRef<Enum>,
        Instance: FragRef<Instance>,
        Pointer: Pointer,
        Array: FragRef<Array>,
        Param: u8,
        Builtin: Builtin,
    }
}

derive_relocated! {
    enum Ty {
        Struct(s) => s,
        Enum(e) => e,
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
            Ty::Struct(s) => write!(f, "struct{:x}", s.bits()),
            Ty::Enum(e) => write!(f, "enum{:x}", e.bits()),
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
        matches!(self, Self::Struct(..) | Self::Enum(..) | Self::Instance(..))
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

    pub fn as_generic(self) -> Option<GenericTy> {
        Some(match self {
            Self::Struct(s) => GenericTy::Struct(s),
            Self::Enum(e) => GenericTy::Enum(e),
            _ => return None,
        })
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
        match self {
            Self::Struct(s) => Some(types[s].loc.source_loc(types)?.span),
            Self::Enum(e) => Some(types[e].loc.source_loc(types)?.span),
            Self::Instance(..)
            | Self::Pointer(..)
            | Self::Param(..)
            | Self::Builtin(..)
            | Self::Array(..) => None,
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

#[derive(Clone, Copy, Serialize, Deserialize, Archive)]

pub struct SpecFunc {
    pub generics: Generics,
    pub signature: Signature,
    pub name: Ident,
    pub span: Span,
    pub parent: FragRef<SpecBase>,
}

derive_relocated!(struct SpecFunc { generics signature parent });

impl SpecFunc {
    pub fn is_generic(&self) -> bool {
        !self.generics.is_empty()
    }
}

#[derive(Clone, Copy, Serialize, Deserialize, Archive)]

pub struct Field {
    pub vis: Option<Vis>,
    pub ty: Ty,
    pub flags: FieldFlags,
    pub span: Span,
    pub name: Ident,
}

derive_relocated!(struct Field { ty });

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
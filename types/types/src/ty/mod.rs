use std::{
    default::default,
    fmt::{self, Display},
    ops::Index,
};

use crate::*;
use span::*;

use rkyv::{Archive, Deserialize, Serialize};
use storage::*;

use self::data::DropSpec;

pub mod data;
pub mod pointer;
pub mod spec;

pub type Generics = FragSlice<FragSlice<Spec>>;

pub type Spec = Node<FragRef<SpecBase>>;

wrapper_enum! {
    #[derive(
        Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize, Archive,
    )]

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
    pub fn drop_spec(self, types: &Types) -> DropSpec {
        match self {
            Self::Struct(s) => types[s].drop_spec,
            Self::Enum(e) => types[e].drop_spec,
        }
    }

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
            Ty::Node(n) => Some(n.base(types)),
            Ty::Pointer(..) | Ty::Array(..) | Ty::Param(..) | Ty::Builtin(..) => None,
        }
    }
}

pub type ParamRepr = u16;

pub struct Param {
    pub repr: ParamRepr,
    pub spec: OptFragRef<Asoc>,
}

pub struct AsocUse {
    pub spec: Spec,
    pub asoc_def: Asoc,
}

pub struct AsocBase {
    pub name: Ident,
    pub generics: Generics,
    pub loc: GuaranteedLoc,
}

type Asoc = Node<FragRef<AsocBase>>;

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize, Archive,
)]
pub enum Node<T> {
    Instance(FragRef<Instance<T>>),
    Base(T),
}

impl<T: Copy> Node<T> {
    pub fn base(self, types: &Types) -> T
    where
        Types: Index<FragRef<Instance<T>>, Output = Instance<T>>,
    {
        match self {
            Node::Base(base) => base,
            Node::Instance(instance) => types[instance].base,
        }
    }

    pub fn base_and_args(self, types: &Types) -> (T, FragSlice<Ty>)
    where
        Types: Index<FragRef<Instance<T>>, Output = Instance<T>>,
    {
        match self {
            Node::Base(base) => (base, default()),
            Node::Instance(instance) => {
                let instance = &types[instance];
                (instance.base, instance.args)
            }
        }
    }
}

impl<T> Relocated for Node<T>
where
    T: Relocated + 'static,
{
    fn mark(&self, marker: &mut FragRelocMarker) {
        match self {
            Node::Instance(i) => i.mark(marker),
            Node::Base(b) => b.mark(marker),
        }
    }

    fn remap(&mut self, ctx: &FragMarks) -> Option<()> {
        match self {
            Node::Instance(i) => i.remap(ctx)?,
            Node::Base(b) => b.remap(ctx)?,
        }
        Some(())
    }
}

#[derive(Clone, Copy, Serialize, Deserialize, Archive)]
pub struct Instance<T> {
    pub base: T,
    pub args: FragSlice<Ty>,
}

impl<T: Relocated> Relocated for Instance<T> {
    fn mark(&self, marker: &mut FragRelocMarker) {
        self.base.mark(marker);
        self.args.mark(marker);
    }

    fn remap(&mut self, ctx: &FragMarks) -> Option<()> {
        self.base.remap(ctx)?;
        self.args.remap(ctx)?;
        Some(())
    }
}

#[derive(Clone, Copy, Debug)]
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
    enum Ty: {
        Node: Node<BaseTy>,
        Pointer: Pointer,
        Array: FragRef<Array>,
        Param: ParamRepr,
        Builtin: Builtin,
    }
}

impl From<FragRef<Struct>> for Ty {
    fn from(ty: FragRef<Struct>) -> Self {
        Self::Node(Node::Base(ty.into()))
    }
}

impl From<FragRef<Enum>> for Ty {
    fn from(ty: FragRef<Enum>) -> Self {
        Self::Node(Node::Base(ty.into()))
    }
}

impl From<BaseTy> for Ty {
    fn from(ty: BaseTy) -> Self {
        Self::Node(Node::Base(ty))
    }
}

derive_relocated! {
    enum Ty {
        Node(n) => n,
        Pointer(p) => p,
        Array(a) => a,
        Param(..) =>,
        Builtin(..) =>,
    }
}

derive_relocated!(enum SignificantTy {
    Base(b) => b,
    Builtin(..) =>,
});
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize, Archive,
)]
pub enum SignificantTy {
    Base(BaseTy),
    Builtin(Builtin),
}

impl fmt::Display for SignificantTy {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Base(b) => write!(f, "{}", b),
            Self::Builtin(b) => write!(f, "{}", b.name()),
        }
    }
}

impl Ty {
    pub fn significant(self, types: &Types) -> Option<SignificantTy> {
        match self {
            Ty::Node(b) => Some(SignificantTy::Base(b.base(types))),
            Ty::Pointer(p) => types[p.ty()].significant(types),
            Ty::Array(a) => types[a].item.significant(types),
            Ty::Param(..) => None,
            Ty::Builtin(b) => Some(SignificantTy::Builtin(b)),
        }
    }

    pub fn drop_spec(self, types: &Types) -> DropSpec {
        match self {
            Ty::Node(b) => b.base(types).drop_spec(types),
            Ty::Builtin(..) | Ty::Pointer(..) => DropSpec::ImplementsCopy,
            Ty::Array(a) if types[a].len == 0 => DropSpec::ImplementsCopy,
            Ty::Array(a) => types[a].item.drop_spec(types),
            Ty::Param(..) => DropSpec::Uncretain,
        }
    }

    pub fn is_copy(self, types: &Types) -> bool {
        self.drop_spec(types) == DropSpec::ImplementsCopy
    }

    pub fn may_need_drop(self, types: &Types) -> bool {
        matches!(self.drop_spec(types), DropSpec::Drop | DropSpec::Uncretain)
    }

    pub fn is_aggregate(self) -> bool {
        matches!(self, Ty::Node(..))
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
            Ty::Node(n) => return Ok(n.base_and_args(types)),
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
            Self::Node(Node::Base(b)) => Some(b),
            _ => None,
        }
    }

    pub fn base_with_params(self, types: &Types) -> (Self, FragSlice<Ty>) {
        match self {
            Self::Node(Node::Instance(i)) => (types[i].base.as_ty(), types[i].args),
            _ => (self, default()),
        }
    }

    pub fn base(self, types: &Types) -> Option<BaseTy> {
        match self {
            Ty::Node(n) => Some(n.base(types)),
            Ty::Pointer(p) => types[p.ty()].base(types),
            Ty::Array(a) => types[a].item.base(types),
            Ty::Param(..) | Ty::Builtin(..) => None,
        }
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
        match self {
            Ty::Builtin(b) => b.is_signed(),
            _ => false,
        }
    }

    pub fn is_unsigned(self) -> bool {
        match self {
            Ty::Builtin(b) => b.is_unsigned(),
            _ => false,
        }
    }

    pub fn is_float(self) -> bool {
        match self {
            Ty::Builtin(b) => b.is_float(),
            _ => false,
        }
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

        }

        #[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, PartialOrd, Ord, Serialize, Deserialize, Archive)]

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

    pub fn is_unsigned(self) -> bool {
        Self::INTEGERS.contains(&self)
    }

    pub fn is_float(self) -> bool {
        Self::FLOATS.contains(&self)
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

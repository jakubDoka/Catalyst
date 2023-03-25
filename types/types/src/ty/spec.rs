use std::{
    default::default,
    fmt::{Display, Formatter},
};

use crate::*;
use rkyv::{Archive, Deserialize, Serialize};
use span::*;
use storage::*;

derive_relocated!(struct Impl { generics key methods });
#[derive(Clone, Copy, Serialize, Deserialize, Archive)]
pub struct Impl {
    pub generics: WhereClause,
    pub key: ImplKey,
    pub methods: FragRefSlice<Func>,
    pub loc: GuaranteedLoc,
}

derive_relocated!(struct ImplKey { ty spec });
#[derive(
    Clone, Copy, PartialEq, Eq, Hash, Debug, PartialOrd, Ord, Serialize, Deserialize, Archive,
)]
#[archive_attr(derive(PartialEq, Eq, Hash))]
pub struct ImplKey {
    pub ty: Ty,
    pub spec: Spec,
}

derive_relocated!(struct SpecBase { generics inherits methods });
#[derive(Clone, Copy, Default, Deserialize, Archive, Serialize)]
pub struct SpecBase {
    pub name: Ident,
    pub generics: WhereClause,
    pub inherits: FragSlice<Spec>,
    pub methods: FragSlice<SpecFunc>,
    pub asoc_tys: FragSlice<AsocTy>,
    pub loc: Loc,
}

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

derive_relocated!(struct SpecInstance { base args });
#[derive(Clone, Copy, Deserialize, Archive, Serialize)]
pub struct SpecInstance {
    pub base: FragRef<SpecBase>,
    pub args: FragSlice<Ty>,
}

derive_relocated!(struct SpecFunc { generics signature parent });
#[derive(Clone, Copy, Serialize, Deserialize, Archive)]
pub struct SpecFunc {
    pub generics: WhereClause,
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

derive_relocated!(
    struct AsocTy {}
);
#[derive(Clone, Copy, Serialize, Deserialize, Archive)]
pub struct AsocTy {
    pub generics: WhereClause,
    pub parent: FragRef<SpecBase>,
    pub name: Ident,
    pub span: Span,
}

type ParameterCountRepr = u16;
#[derive(
    Clone,
    Copy,
    PartialEq,
    Default,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Debug,
    Serialize,
    Deserialize,
    Archive,
)]
pub struct ParameterCount(ParameterCountRepr);

impl ParameterCount {
    const MAX: usize = ParameterCountRepr::MAX as usize;
    pub fn new(count: usize) -> Result<Self, ParameterCountError> {
        (count <= Self::MAX)
            .then_some(Self(count as ParameterCountRepr))
            .ok_or(ParameterCountError(count))
    }

    pub fn get(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug)]
pub struct ParameterCountError(usize);

impl std::fmt::Display for ParameterCountError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "parameter count {} is too largem, max is {}",
            self.0,
            ParameterCount::MAX
        )
    }
}

type TypeParameterRepr = u16;

#[derive(
    Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize, Deserialize, Archive,
)]
#[archive_attr(derive(PartialEq, Eq, Hash))]
pub struct TyParamIdx(TypeParameterRepr);

impl TyParamIdx {
    const SCOPE_SIZE: usize = 0;
    const INDEX_SIZE: usize = std::mem::size_of::<TypeParameterRepr>() * 8 - Self::SCOPE_SIZE;
    const INDEX_MASK: TypeParameterRepr = (1 as TypeParameterRepr)
        .wrapping_shl(Self::INDEX_SIZE as u32)
        .wrapping_sub(1 + (Self::SCOPE_SIZE == 0) as TypeParameterRepr);
    const SCOPE_MASK: TypeParameterRepr = !Self::INDEX_MASK;
    const MAX_SCOPE: usize = (1 << Self::SCOPE_SIZE) - 1;
    const MUTABLE_IDX: TypeParameterRepr = 1 << (Self::INDEX_SIZE - 1);
    const IMMUTABLE_IDX: TypeParameterRepr = Self::MUTABLE_IDX - 1;
    const MAX_INDEX: usize = Self::IMMUTABLE_IDX as usize - 1;
    pub const IMMUTABLE: Self = Self(Self::IMMUTABLE_IDX);
    pub const MUTABLE: Self = Self(Self::MUTABLE_IDX);

    pub fn new(scope: usize, index: usize) -> Result<Self, TypeParameterError> {
        let scope = Self::is_valid_scope(scope)?;
        let index = Self::is_valid_index(index)?;
        let inner = scope.wrapping_shl(Self::INDEX_SIZE as u32) | index;
        Ok(Self(inner))
    }

    pub fn is_valid_scope(scope: usize) -> Result<TypeParameterRepr, TypeParameterError> {
        (scope <= Self::MAX_SCOPE)
            .then_some(scope as TypeParameterRepr)
            .ok_or(TypeParameterError::ScopeTooLarge(scope))
    }

    pub fn is_valid_index(index: usize) -> Result<TypeParameterRepr, TypeParameterError> {
        (index <= Self::MAX_INDEX)
            .then_some(index as TypeParameterRepr)
            .ok_or(TypeParameterError::IndexTooLarge(index))
    }

    pub fn scope(self) -> usize {
        (self.0 & Self::SCOPE_MASK) as usize >> Self::INDEX_SIZE
    }

    pub fn get(self) -> usize {
        (self.0 & Self::INDEX_MASK) as usize
    }

    pub fn compatible(self, other: Self) -> bool {
        self == other || other.0 == Self::IMMUTABLE_IDX
    }

    pub fn from_ty(ty: Ty) -> Self {
        match ty {
            Ty::Param(ty::spec::TyParam { index, .. }) => index,
            _ => Self(Self::IMMUTABLE_IDX),
        }
    }

    pub fn to_ty(self) -> Ty {
        Ty::Param(ty::spec::TyParam::new(self, None))
    }

    pub fn to_mutability(self) -> Mutability {
        match self.0 {
            Self::IMMUTABLE_IDX => Mutability::Immutable,
            Self::MUTABLE_IDX => Mutability::Mutable,
            _ => Mutability::Param(self),
        }
    }

    pub fn instantiate(self, params: &[Ty]) -> TyParamIdx {
        match self {
            Self::IMMUTABLE => Self::IMMUTABLE,
            Self::MUTABLE => Self::MUTABLE,
            _ => Self::from_ty(params[self.get()]),
        }
    }

    pub fn try_instantiate(self, params: &[Option<Ty>]) -> Option<TyParamIdx> {
        Some(match self {
            Self::IMMUTABLE => Self::IMMUTABLE,
            Self::MUTABLE => Self::MUTABLE,
            _ => Self::from_ty(params[self.get()]?),
        })
    }
}

#[derive(Default, Clone, Copy)]
pub struct TyParamIter {
    scope: usize,
    index: usize,
}

impl TyParamIter {
    pub fn progress(&self) -> usize {
        self.index
    }
}

impl Iterator for TyParamIter {
    type Item = TyParamIdx;

    fn next(&mut self) -> Option<Self::Item> {
        self.index += 1;
        TyParamIdx::new(self.scope, self.index - 1).ok()
    }
}

impl AsMut<TyParamIter> for TyParamIter {
    fn as_mut(&mut self) -> &mut Self {
        self
    }
}

impl std::fmt::Display for TyParamIdx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.scope(), self.get())
    }
}

#[derive(Debug)]
pub enum TypeParameterError {
    ScopeTooLarge(usize),
    IndexTooLarge(usize),
}

impl std::fmt::Display for TypeParameterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ScopeTooLarge(scope) => {
                write!(
                    f,
                    "scope {} is too large, max is {}",
                    scope,
                    TyParamIdx::MAX_SCOPE
                )
            }
            Self::IndexTooLarge(index) => {
                write!(
                    f,
                    "index {} is too large, max is {}",
                    index,
                    TyParamIdx::MAX_INDEX
                )
            }
        }
    }
}

#[derive(
    Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize, Deserialize, Archive,
)]
#[archive_attr(derive(PartialEq, Eq, Hash))]
pub struct TyParam {
    pub index: TyParamIdx,
    is_asoc: bool,
    asoc_index: u32,
    asoc_thread: u8,
}

impl TyParam {
    pub fn new(index: TyParamIdx, asoc: OptFragRef<AsocTy>) -> Self {
        Self {
            index,
            is_asoc: asoc.is_some(),
            asoc_index: asoc.map_or(0, |asoc| asoc.addr().index),
            asoc_thread: asoc.map_or(0, |asoc| asoc.addr().thread),
        }
    }

    pub fn asoc(&self) -> OptFragRef<AsocTy> {
        self.is_asoc.then_some(FragRef::new(FragAddr::new(
            self.asoc_index,
            self.asoc_thread,
        )))
    }
}

impl Relocated for TyParam {
    fn mark(&self, marker: &mut FragRelocMarker) {
        if let Some(asoc) = self.asoc() {
            marker.mark(asoc);
        }
    }

    fn remap(&mut self, ctx: &FragMarks) -> Option<()> {
        if let Some(asoc) = self.asoc() {
            let addr = ctx.project(asoc.as_slice())?.addr();
            self.asoc_index = addr.index;
            self.asoc_thread = addr.thread;
        }
        Some(())
    }
}

impl Display for TyParam {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.index)?;
        if let Some(asoc) = self.asoc() {
            write!(f, ":{:x}", asoc.bits())?;
        }
        Ok(())
    }
}

derive_relocated!(struct WhereClause { predicates });
#[derive(
    Clone,
    Default,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Debug,
    Serialize,
    Deserialize,
    Archive,
)]
pub struct WhereClause {
    pub parameter_count: ParameterCount,
    pub predicates: FragSlice<WherePredicate>,
}

impl WhereClause {
    pub fn basic(len: usize, types: &mut Types) -> Result<WhereClause, ParameterCountError> {
        Self::new((0..len).map(|_| default()), len, types)
    }

    pub fn new<I>(
        predicates: I,
        parameter_count: usize,
        types: &mut Types,
    ) -> Result<Self, ParameterCountError>
    where
        I: IntoIterator<Item = WherePredicate>,
        I::IntoIter: ExactSizeIterator,
    {
        let predicates = types.cache.predicates.extend(predicates);
        Ok(Self {
            parameter_count: ParameterCount::new(parameter_count)?,
            predicates,
        })
    }

    pub fn is_empty(&self) -> bool {
        self.predicates.is_empty()
    }

    pub fn root_predicates<'a>(&self, types: &'a Types) -> &'a [WherePredicate] {
        &types[self.predicates][..self.parameter_count.get()]
    }

    pub fn other_predicates<'a>(
        &self,
        types: &'a Types,
    ) -> impl Iterator<Item = (Ty, FragSlice<Spec>)> + 'a {
        types[self.predicates]
            .iter()
            .skip(self.parameter_count.get())
            .map(|pred| {
                (
                    pred.ty
                        .expect("all predicated after parameters has to have a type"),
                    pred.bounds,
                )
            })
    }

    pub fn len(&self) -> usize {
        self.predicates.len()
    }
}

derive_relocated!(struct WherePredicate { ty bounds });
#[derive(
    Clone,
    Copy,
    PartialEq,
    Default,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Debug,
    Serialize,
    Deserialize,
    Archive,
)]
pub struct WherePredicate {
    pub ty: Option<Ty>,
    pub bounds: FragSlice<Spec>,
}

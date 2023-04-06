use core::fmt;

use crate::*;

use super::spec::TyParamIdx;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Pointer<'a> {
    pub mutability: Mutability,
    pub depth: u8,
    pub ty: &'a Ty<'a>,
}

impl<'a> Pointer<'a> {
    pub fn compatible(self, other: Self) -> bool {
        self.mutability.compatible(other.mutability) && Ty::compatible(*self.ty, *other.ty)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Mutability {
    Mutable,
    Immutable,
    Param(TyParamIdx),
}

impl Mutability {
    pub fn compatible(self, other: Self) -> bool {
        self == other || other == Mutability::Immutable
    }
}

impl Mutability {
    pub fn as_ty(self) -> Ty<'static> {
        match self {
            Mutability::Mutable => TyParamIdx::MUTABLE,
            Mutability::Immutable => TyParamIdx::IMMUTABLE,
            Mutability::Param(i) => i,
        }
        .to_ty()
    }

    pub fn as_param(self) -> TyParamIdx {
        match self {
            Mutability::Mutable => TyParamIdx::MUTABLE,
            Mutability::Immutable => TyParamIdx::IMMUTABLE,
            Mutability::Param(i) => i,
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

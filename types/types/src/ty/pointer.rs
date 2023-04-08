use core::fmt;

use crate::*;
use rkyv::{Archive, Deserialize, Serialize};
use storage::*;

use super::ParamRepr;

#[derive(
    Clone, Serialize, Deserialize, Archive, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug,
)]

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
    Param(ParamRepr),
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

pub struct RawMutability(ParamRepr);

impl RawMutability {
    pub const MUTABLE: Self = Self(!0);
    pub const IMMUTABLE: Self = Self(!1);
    pub const PARAM_OFFSET: ParamRepr = 2;
    pub fn new(mutability: Mutability) -> Option<Self> {
        Some(match mutability {
            Mutability::Mutable => Self::MUTABLE,
            Mutability::Immutable => Self::IMMUTABLE,
            Mutability::Param(i) => Self(i),
        })
    }

    pub fn compatible(self, other: Self) -> bool {
        self == other || other == Self::IMMUTABLE
    }

    pub fn from_ty(ty: Ty) -> Self {
        match ty {
            Ty::Builtin(Builtin::Mutable) => Self::MUTABLE,
            Ty::Param(i) => Self(i),
            _ => Self::IMMUTABLE,
        }
    }

    pub fn to_ty(self) -> Ty {
        match self {
            Self::IMMUTABLE => Ty::IMMUTABLE,
            Self::MUTABLE => Ty::MUTABLE,
            Self(i) => Ty::Param(i),
        }
    }

    pub fn to_mutability(self) -> Mutability {
        match self {
            Self::IMMUTABLE => Mutability::Immutable,
            Self::MUTABLE => Mutability::Mutable,
            Self(i) => Mutability::Param(i),
        }
    }

    pub fn instantiate(self, params: &[Ty]) -> RawMutability {
        match self {
            Self::IMMUTABLE => Self::IMMUTABLE,
            Self::MUTABLE => Self::MUTABLE,
            Self(i) => Self::from_ty(params[i as usize]),
        }
    }

    pub fn try_instantiate(self, params: &[Option<Ty>]) -> Option<RawMutability> {
        Some(match self {
            Self::IMMUTABLE => Self::IMMUTABLE,
            Self::MUTABLE => Self::MUTABLE,
            Self(i) => Self::from_ty(params[i as usize]?),
        })
    }
}

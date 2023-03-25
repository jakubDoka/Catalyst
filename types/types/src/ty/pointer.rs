use core::fmt;

use crate::*;
use rkyv::{Archive, Deserialize, Serialize};
use storage::*;

use super::spec::TyParamIdx;

#[derive(
    Clone, Serialize, Deserialize, Archive, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug,
)]
#[archive_attr(derive(PartialEq, Eq, Hash))]
pub struct Pointer {
    index: u32,
    thread: u8,
    pub mutability: TyParamIdx,
    pub depth: u8,
}

impl Pointer {
    pub fn new(ty: FragRef<Ty>, mutability: TyParamIdx, depth: u8) -> Self {
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
    Param(TyParamIdx),
}

impl Mutability {
    pub fn as_ty(self) -> Ty {
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

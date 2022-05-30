#![feature(bool_to_option)]
#![feature(let_else)]
#![feature(result_into_ok_or_err)]
#![feature(let_chains)]

pub mod error;
pub mod func;
pub mod graph;
pub mod jit;
pub mod tir;
pub mod ty;

pub use error::TyError;
pub use func::{Func, FuncEnt, FuncFlags, FuncKind, FuncList, FuncMeta, FuncMetaData, Funcs, Sig, ToCompile};
pub use graph::Graph;
pub use tir::{FuncBodies, Tir, TirData, TirDisplay, TirEnt, TirFlags, TirKind, TirList, TirPatternGraph, TirStack, TirPatternMeta};
pub use ty::{
    BoundImpl, BoundImpls, BuiltinTypes, Instances, FuncLists, Ty, TyComp, TyCompEnt, TyCompList,
    TyCompLookup, TyComps, TyDisplay, TyEnt, TyFlags, TyKind, TyList, TyLists, TypeBase, Types,
    TyGraph,
};

#[macro_export]
macro_rules! ty_display {
    ($self:expr, $ty:expr) => {
        TyDisplay::new(&$self.types, &$self.ty_lists, &$self.sources, $ty)
    };
}

#[macro_export]
macro_rules! sig_display {
    ($self:expr, $sig:expr) => {
        SignatureDisplay::new($self.sources, $self.ty_lists, $self.types, $sig)
    };
}

#[macro_export]
macro_rules! tir_display {
    ($self:expr, $tir:expr) => {
        $crate::tir::TirDisplay::new(
            $self.types,
            $self.ty_lists,
            $self.ty_comps,
            $self.sources,
            $self.data,
            $tir,
        )
    };
}

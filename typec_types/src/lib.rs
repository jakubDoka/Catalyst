#![feature(bool_to_option)]
#![feature(let_else)]
#![feature(result_into_ok_or_err)]
#![feature(let_chains)]

pub mod error;
pub mod func;
pub mod tir;
pub mod ty;
pub mod jit;
pub mod graph;
pub mod exhaust;

pub use error::TyError;
pub use func::{Func, FuncList, Funcs, Sig, FuncMetaData, FuncFlags, FuncKind, FuncMeta, FuncEnt};
pub use tir::{Tir, TirData, TirDisplay, TirEnt, TirFlags, TirKind, TirList, FuncBodies};
pub use ty::{
    BoundImpl, BoundImpls, BuiltinTypes, Instances, TyComp, TyCompEnt, TyCompList,
    TyCompLookup, TyComps, TFuncLists, Ty, TyDisplay, TyEnt, TyFlags, TyKind, TyList,
    TyLists, TypeBase, Types
};
pub use graph::Graph;
pub use exhaust::{Exhaust, ExhaustMap, PatternNode};

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

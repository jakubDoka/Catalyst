#![feature(let_else)]
#![feature(result_into_ok_or_err)]
#![feature(let_chains)]

pub mod error;
pub mod func;
pub mod global;
pub mod graph;
pub mod jit;
pub mod tir;
pub mod ty;

pub use error::TyError;
pub use func::{
    Func, FuncEnt, FuncFlags, FuncInstances, FuncKind, FuncList, FuncMeta, Funcs, Initializers,
    Macros, Sig, ToCompile, ToLink,
};
pub use global::{Global, GlobalBytes, GlobalData, GlobalEnt, GlobalMap, Globals};
pub use graph::Graph;
pub use tir::{
    Tir, TirData, TirDisplay, TirEnt, TirFlags, TirKind, TirList, TirPatternGraph, TirPatternMeta,
    TirStack,
};
pub use ty::{
    BoundImpl, BoundImpls, BuiltinTypes, FuncLists, Ty, TyComp, TyCompEnt, TyCompList, TyComps,
    TyDisplay, TyEnt, TyFlags, TyGraph, TyInstances, TyKind, TyList, TyLists, TypeBase, Types,
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

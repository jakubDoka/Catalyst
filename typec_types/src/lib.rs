#![feature(bool_to_option)]

pub mod error;
pub mod func;
pub mod size;
pub mod tir;
pub mod ty;

pub use error::TyError;
pub use func::{Func, FuncList, Funcs, Sig, TFuncEnt, TFuncFlags, TFuncKind, ToCompile};
pub use size::{Layout, Offset};
pub use tir::{Tir, TirData, TirDisplay, TirEnt, TirFlags, TirKind, TirList};
pub use ty::{
    BoundImpl, BoundImpls, BuiltinTypes, FuncInstances, Instances, SField, SFieldEnt, SFieldList,
    SFieldLookup, SFieldRef, SFields, TFuncLists, Ty, TyDisplay, TyEnt, TyFlags, TyKind, TyList,
    TyLists, TypeBase, Types,
};

#[macro_export]
macro_rules! ty_display {
    ($self:expr, $ty:expr) => {
        TyDisplay::new($self.types, $self.ty_lists, $self.sources, $ty)
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
            $self.sfields,
            $self.sources,
            $self.data,
            $tir,
        )
    };
}

#![feature(bool_to_option)]

pub mod ty;
pub mod func;
pub mod tir;
pub mod error;
pub mod size;

pub use ty::{
    BuiltinTypes, Ty, TyEnt, TyFlags, TyKind, TyList, Types, BoundImpl, 
    SField, SFieldEnt, SFieldList, SFieldRef, TyDisplay, TyLists, SFields, 
    SFieldLookup, TypeBase, Instances, BoundImpls, TFuncLists,
};
pub use func::{Func, TFuncEnt, TFuncFlags, TFuncKind, FuncList, Funcs, Sig};
pub use tir::{Tir, TirList, TirKind, TirFlags, TirData, TirEnt, TirDisplay};
pub use error::TyError;
pub use size::{Offset, Layout};

#[macro_export]
macro_rules! ty_display {
    ($self:expr, $ty:expr) => {
        TyDisplay::new(
            $self.types, 
            $self.ty_lists, 
            $self.sources, 
            $ty
        )
    }
}

#[macro_export]
macro_rules! sig_display {
    ($self:expr, $sig:expr) => {
        SignatureDisplay::new(
            $self.sources, 
            $self.ty_lists, 
            $self.types, 
            $sig
        )
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
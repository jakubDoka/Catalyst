#![feature(bool_to_option)]

pub mod ty;
pub mod func;
pub mod tir;
pub mod error;

pub use ty::{Ty, TyEnt, TyFlags, TyKind, TyList, Types, BoundImpl, SField, SFieldEnt, SFieldList, SFieldRef, TyDisplay};
pub use func::{Func, TFuncEnt, TFuncFlags, TFuncKind, FuncList, Funcs, Sig};
pub use tir::{Tir, TirList, TirKind, TirFlags, TirData, TirEnt};
pub use error::TyError;
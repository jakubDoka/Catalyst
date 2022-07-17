pub mod func;
pub mod tir;
pub mod ty;

pub use func::{DefEnt, Func, FuncEnt, FuncList, Funcs, Sig};
pub use tir::{Tir, TirData, TirEnt, TirFlags, TirKind, TirMeta};
pub use ty::{BuiltinTypes, Ty, TyEnt, TyList};

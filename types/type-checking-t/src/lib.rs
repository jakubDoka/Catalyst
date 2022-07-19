mod func;
mod tir;
mod ty;

pub use func::{DefEnt, Func, FuncEnt, FuncList, Funcs, Sig};
pub use tir::{Tir, TirData, TirEnt, TirFlags, TirKind, TirMeta};
pub use ty::{BuiltinTypes, Ty, TyEnt, TyList, Types, TyKind, TyFlags};
pub use items::ItemContext;

mod items {
    use parsing_t::*;
    use crate::*;

    pub struct ItemContext {
        pub attrs: Vec<AstEnt>,
        pub funcs: Vec<(AstEnt, Func)>,
        pub types: Vec<(AstEnt, Ty)>,
        pub bounds: Vec<(AstEnt, Ty)>,
    }
}
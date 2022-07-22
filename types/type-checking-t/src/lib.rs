#![feature(let_else)]
#![feature(inline_const)]
#![feature(default_free_fn)]

#[macro_export]
macro_rules! field_ident {
    ($self:expr, $field:expr) => {
        ident!($self, ".", $field)
    };
}

mod func;
mod state_gen;
mod tir;
mod ty;
mod ty_factory;
mod builtin_builder;

pub use func::{DefEnt, Func, FuncEnt, FuncList, Funcs, Sig};
pub use items::ItemContext;
pub use state_gen::{TyFactory, BuiltinBuilder};
pub use tir::{Tir, TirData, TirEnt, TirFlags, TirKind, TirMeta};
pub use ty::{BuiltinTypes, Field, FieldEnt, FieldList, Ty, TyEnt, TyFlags, TyKind, TyList, Types};

mod items {
    use crate::*;
    use parsing_t::*;

    #[derive(Default)]
    pub struct ItemContext {
        pub attrs: Vec<AstEnt>,
        pub funcs: Vec<(AstEnt, Func)>,
        pub types: Vec<(AstEnt, Ty)>,
        pub bounds: Vec<(AstEnt, Ty)>,
    }
}

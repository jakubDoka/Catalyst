#![feature(let_else)]
#![feature(inline_const)]
#![feature(default_free_fn)]
#![feature(let_chains)]

#[macro_export]
macro_rules! field_ident {
    ($self:expr, $field:expr) => {
        storage::ident!($self, ".", $field)
    };
}

#[macro_export]
macro_rules! param_ident {
    ($param:expr) => {
        storage::ident!("param ", $param)
    };
}

#[macro_export]
macro_rules! binary_ident {
    ($param:expr) => {
        storage::ident!("binary ", $param)
    };
}

#[macro_export]
macro_rules! bound_impl_ident {
    ($bound:expr, $implementor:expr) => {
        storage::ident!("impl ", $bound, " for ", $implementor)
    };
}

#[macro_export]
macro_rules! match_scope_ptr {
    (
        ($self:expr, $ptr:expr, $span:expr) => {
            $($binding:ident: $ty:ty => $code:expr,)*
        }
    ) => {
        $(
            if let Some($binding) = $ptr.try_read::<$ty>() {
                let _ = $binding;
                $code
            } else
        )*
        {
            let options = [$(stringify!($binding),)*];
            $self.workspace.push(diag! {
                ($span, $self.current_file) => "expected {}" { options.join(" or ") },
            });
            return Err(());
        }
    };
}

mod bound_checker;
mod builtin_builder;
mod funcs;
mod state_gen;
mod tir;
mod ty;
mod ty_factory;

pub use funcs::{Def, DefEnt, DefList, FuncFlags, FuncParserCtx, Sig};
pub use items::ItemContext;
pub use state_gen::{BoundChecker, BuiltinBuilder, TirDisplay, TyFactory};
pub use tir::{Tir, TirData, TirEnt, TirFlags, TirKind, TirList, TirMeta};
pub use ty::{
    BoundFunc, BoundFuncEnt, BoundFuncList, BuiltinTypes, EnumVariant, EnumVariantEnt,
    EnumVariantList, Field, FieldEnt, FieldList, Impl, ImplEnt, Ty, TyEnt, TyFlags, TyKind, TyList,
    Typec,
};

mod items {
    use crate::*;
    use parsing_t::*;

    #[derive(Default)]
    pub struct ItemContext {
        pub attrs: Vec<AstEnt>,
        pub funcs: Vec<(AstEnt, Def)>,
        pub types: Vec<(AstEnt, Ty)>,
        pub bound_impls: Vec<(AstEnt, Impl)>,
    }
}

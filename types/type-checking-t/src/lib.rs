#![feature(let_else)]
#![feature(inline_const)]
#![feature(default_free_fn)]

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
macro_rules! ident_chain_id {
    ($self:expr, $ast:expr) => {
        $crate::ident_chain_id(
            $self.current_file,
            $ast,
            $self.interner,
            $self.ast_data,
            $self.packages,
        )
    };
}

mod bound_checker;
mod builtin_builder;
mod funcs;
mod state_gen;
mod tir;
mod ty;
mod ty_factory;

pub use funcs::{Def, DefEnt, FuncFlags, FuncList, FuncParserCtx, Funcs, Sig};
pub use items::{ident_chain_id, ItemContext};
pub use state_gen::{BoundChecker, BuiltinBuilder, TirDisplay, TyFactory};
pub use tir::{Tir, TirData, TirEnt, TirFlags, TirKind, TirList, TirMeta};
pub use ty::{
    BoundFunc, BoundFuncEnt, BoundFuncList, BuiltinTypes, EnumVariant, EnumVariantEnt,
    EnumVariantList, Field, FieldEnt, FieldList, Impl, ImplEnt, Ty, TyEnt, TyFlags, TyKind, TyList,
    Types,
};

mod items {
    use crate::*;
    use packaging_t::*;
    use parsing_t::*;
    use storage::*;

    #[derive(Default)]
    pub struct ItemContext {
        pub attrs: Vec<AstEnt>,
        pub funcs: Vec<(AstEnt, Def)>,
        pub types: Vec<(AstEnt, Ty)>,
        pub impls: Vec<(AstEnt, Impl)>,
    }

    pub fn ident_chain_id(
        file: Ident,
        ast: AstEnt,
        interner: &mut Interner,
        ast_data: &AstData,
        packages: &Packages,
    ) -> Ident {
        match ast.kind {
            AstKind::IdentChain => {
                let segments = ast_data[ast.children]
                    .iter()
                    .map(|child| interner.intern_str(packages.span_str(file, child.span)))
                    .map(|segment| [InternedSegment::from(segment), "`".into()])
                    .flatten()
                    .take(
                        (ast_data[ast.children].len() * 2)
                            .checked_sub(1)
                            .unwrap_or(0),
                    ) // -1 for the excess `
                    .collect::<Vec<_>>();

                interner.intern(&segments)
            }
            AstKind::Ident => interner.intern_str(packages.span_str(file, ast.span)),
            _ => unimplemented!(),
        }
    }
}

#![feature(let_else)]
#![feature(inline_const)]
#![feature(default_free_fn)]

#[macro_export]
macro_rules! field_ident {
    ($self:expr, $field:expr) => {
        ident!($self, ".", $field)
    };
}

#[macro_export]
macro_rules! param_ident {
    ($param:expr) => {
        ident!("param ", $param)
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

mod builtin_builder;
mod fns;
mod state_gen;
mod tir;
mod ty;
mod ty_factory;

pub use fns::{Def, DefEnt, FnEnt, FnFlags, FnList, FnParserCtx, Fns, Sig};
pub use items::{ident_chain_id, ItemContext};
pub use state_gen::{BuiltinBuilder, TyFactory};
pub use tir::{Tir, TirData, TirEnt, TirFlags, TirKind, TirList, TirMeta};
pub use ty::{BuiltinTypes, Field, FieldEnt, FieldList, Ty, TyEnt, TyFlags, TyKind, TyList, Types};

mod items {
    use crate::*;
    use packaging_t::*;
    use parsing_t::*;
    use storage::*;

    #[derive(Default)]
    pub struct ItemContext {
        pub attrs: Vec<AstEnt>,
        pub fns: Vec<(AstEnt, Def)>,
        pub types: Vec<(AstEnt, Ty)>,
        pub bounds: Vec<(AstEnt, Ty)>,
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

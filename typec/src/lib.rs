#![feature(let_else)]
#![feature(explicit_generic_args_with_impl_trait)]
#![feature(let_chains)]
#![feature(bool_to_option)]

pub mod collector;
pub mod error;
pub mod func;
pub mod tir;
pub mod ty;

pub use collector::*;
pub use func::*;
pub use tir::*;
pub use ty::*;

use error::Error;
use lexer::*;
use modules::*;
use parser::*;

pub type Result<T = ()> = std::result::Result<T, Error>;

pub fn parse_type(scope: &Scope, ast: &ast::Data, sources: &Sources, ty: Ast) -> Result<Ty> {
    let ast::Ent { kind, span, .. } = ast.nodes[ty];
    match kind {
        ast::Kind::Ident => {
            return scope
                .get(sources.display(span), span)
                .map_err(Convert::convert);
        }
        _ => todo!("Unhandled type expr {:?}: {}", kind, sources.display(span)),
    }
}

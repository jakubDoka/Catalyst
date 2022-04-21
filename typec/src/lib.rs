#![feature(let_else)]
#![feature(explicit_generic_args_with_impl_trait)]
#![feature(let_chains)]
#![feature(bool_to_option)]

pub mod collector;
pub mod func;
pub mod tir;
pub mod ty;
pub mod error;

pub use collector::*;
pub use func::*;
pub use tir::*;
pub use ty::*;
pub use error::Error;

use lexer::*;
use modules::*;
use parser::*;

pub trait TypeParser {

    fn state(&mut self) -> (&mut Scope, &mut Types, &Sources, &ast::Data, &mut errors::Diagnostics);

    fn parse_type(&mut self, ty: Ast) -> errors::Result<Ty> {
        let (scope, _types, sources, ast, diagnostics) = self.state();
        let ast::Ent { kind, span, .. } = ast.nodes[ty];
        match kind {
            ast::Kind::Ident => {
                let str = sources.display(span);
                return scope.get(diagnostics, str, span)
            }
            _ => todo!("Unhandled type expr {:?}: {}", kind, sources.display(span)),
        }
    }
}
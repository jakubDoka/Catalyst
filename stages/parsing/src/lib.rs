#![feature(default_free_fn)]
#![feature(let_else)]

#[macro_export]
macro_rules! list {
    ($self:expr, $start:ident, $sep:ident, $end:ident, $func:ident) => {
        $self.list(TokenKind::$start, TokenKind::$sep, TokenKind::$end, Self::$func)
    };
}

pub mod ast;
pub mod parser;
pub mod state_gen;

pub use ast::{Ast, AstData, AstEnt, AstKind, AstList};
pub use parser::Parser;
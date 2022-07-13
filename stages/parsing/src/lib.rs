#![feature(default_free_fn)]
#![feature(let_else)]
#![feature(let_chains)]

#[macro_export]
macro_rules! token {
    (none) => {
        None
    };
    ($value:ident) => {
        Some(TokenKind::$value)
    };
}

#[macro_export]
macro_rules! list {
    ($self:expr, $start:ident, $sep:ident, $end:ident, exp $func:expr) => {
        $self.list(token!($start), token!($sep), token!($end), $func)
    };

    ($self:expr, $start:ident, $sep:ident, $end:ident, $func:ident) => {
        list!($self, $start, $sep, $end, exp Self::$func)
    };
}

#[macro_export]
macro_rules! branch {
    (
        $self:expr => {
            $($cond:ident => $res:expr,)*
        }
    ) => {
        match $self.state.current.kind {
            $(TokenKind::$cond => drop($res),)*
            _ => {
                let terminals = [
                    $(TokenKind::$cond),*
                ];
                $self.expect_error(&terminals);
                return Err(())
            },
        }
    };
}

pub mod ast;
pub mod parser;
pub mod state_gen;

pub use ast::{Ast, AstData, AstEnt, AstKind, AstList};
pub use parser::{Parser, ParserState};

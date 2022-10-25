#![feature(unboxed_closures)]
#![feature(fn_traits)]

//! Crate split form `lexing-t` to isolate code generated by `logos`.
//! The [`TokenKind`] is used only when parsing.

mod ctl;
mod lexer;
mod token;

pub use {
    ctl::{ctl_lexer_next, CtlLexer, TokenMacro, TokenMacroCtx, TokenMacroData, TokenMacroSpec},
    lexer::Lexer,
    token::{Token, TokenKind, EQUAL_SIGN_PRECEDENCE},
};

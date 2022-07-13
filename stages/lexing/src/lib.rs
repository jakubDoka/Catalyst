#![feature(let_chains)]
//! Crate provides simple constructs of lexical analysis.

pub extern crate lexing_t as types;

pub mod lexer;
pub mod token;

pub use lexer::Lexer;
pub use token::{Token, TokenKind};
pub use types::*;

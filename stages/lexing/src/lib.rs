#![feature(let_chains)]
//! Crate provides simple constructs of lexical analysis.

pub mod token;
pub mod lexer;
pub mod span;

pub use token::{Token, TokenKind};
pub use lexer::{Lexer};
pub use span::{Span};
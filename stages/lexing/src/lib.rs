//! Crate provides simple constructs of lexical analysis.

mod lexer;
mod token;

pub use lexer::Lexer;
pub use token::{Token, TokenKind, EQUAL_SIGN_PRECEDENCE};

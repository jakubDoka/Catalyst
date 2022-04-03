#![feature(let_else)]
#![feature(explicit_generic_args_with_impl_trait)]

use std::ops::Range;

pub mod error;
pub mod func;
pub mod ir;
pub mod logic;
pub mod ty;

#[macro_export]
macro_rules! gen_context {
    ($name:ident<$($lifetime:lifetime),*> { $($field_name:ident: $field_type:ty),* $(,)?}) => {
        pub struct $name<$($lifetime),*> {
            $(pub $field_name: $field_type),*
        }

        impl<$($lifetime),*> From<($($field_type),*)> for $name<$($lifetime),*> {
            fn from(($($field_name),*): ($($field_type),*)) -> Self {
                Self {
                    $($field_name),*
                }
            }
        }
    };
}

pub enum TokenKind {
    Ident,
    Op,
    Int(i64),
    Float(f64),
    String,
    Eof,
}

pub struct Source {
    pub content: String,
}

impl Source {
    pub fn get_underlying_string(&self, token: &Token) -> &str {
        &self.content[token.span.clone()]
    }   
}

pub struct Token {
    pub kind: TokenKind,
    pub span: Range<usize>,
}


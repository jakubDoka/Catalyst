#![feature(auto_traits)]
#![feature(negative_impls)]

pub extern crate cranelift_entity;

pub mod map;
pub mod source_info;
pub mod token;

use std::str::Chars;

pub use map::*;
pub use source_info::*;
pub use token::*;

#[macro_export]
macro_rules! gen_entity {
    ($($name:ident)*) => {
        $(
            #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            pub struct $name(pub u32);

            $crate::cranelift_entity::entity_impl!($name);

            impl Default for $name {
                fn default() -> Self {
                    $crate::cranelift_entity::packed_option::ReservedValue::reserved_value()
                }
            }


            impl std::fmt::Display for $name {
                fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                    write!(f, "{}{}", stringify!($name).chars().next().unwrap().to_lowercase(), self.0)
                }
            }

            impl $crate::map::IDFilter for $name {}
        )*
    };
}

use cranelift_entity::{packed_option::ReservedValue, EntityList, EntityRef, ListPool};

pub trait ListPoolExt<T: EntityRef + ReservedValue> {
    fn list(&mut self, slice: &[T]) -> EntityList<T>;
    fn view(&self, list: EntityList<T>) -> &[T];
}

impl<T: EntityRef + ReservedValue> ListPoolExt<T> for ListPool<T> {
    fn list(&mut self, slice: &[T]) -> EntityList<T> {
        EntityList::from_slice(slice, self)
    }

    fn view(&self, list: EntityList<T>) -> &[T] {
        list.as_slice(self)
    }
}

pub struct Lexer<'a> {
    chars: Chars<'a>,
    str: &'a str,
    source: Source,
}

impl<'a> Lexer<'a> {
    pub fn new(progress: usize, source: &'a str, id: Source) -> Self {
        Self {
            chars: source[progress..].chars(),
            str: source,
            source: id,
        }
    }

    pub fn next_token(&mut self) -> Token {
        loop {
            let start = self.progress();
            let c = match self.peek() {
                Some(c) => c,
                None => return self.new_token(token::Kind::Eof, start),
            };

            self.next();

            let kind = match c {
                '\n' | ';' => token::Kind::NewLine,

                '{' => token::Kind::LeftCurly,
                '}' => token::Kind::RightCurly,

                '(' => token::Kind::LeftParen,
                ')' => token::Kind::RightParen,

                '[' => token::Kind::LeftBracket,
                ']' => token::Kind::RightBracket,

                ',' => token::Kind::Comma,
                '.' => token::Kind::Dot,

                '0'..='9' => {
                    while let Some('0'..='9') = self.peek() {
                        self.next();
                    }
                    token::Kind::Int(-1)
                }

                '"' => {
                    while let Some(true) = self.next().map(|c| c != '"') {}
                    token::Kind::String
                }

                _ if c.is_whitespace() => continue,
                _ if c.is_operator() => {
                    while let Some(true) = self.peek().map(|c| c.is_operator()) {
                        self.next();
                    }

                    match self.view(start) {
                        "->" => token::Kind::RightArrow,
                        ":" => token::Kind::Colon,
                        "::" => token::Kind::DoubleColon,
                        _ => token::Kind::Operator,
                    }
                }
                _ if c.is_alphabetic() || c == '_' => {
                    while let Some(true) = self.peek().map(|c| c.is_alphanumeric() || c == '_') {
                        self.next();
                    }

                    match self.view(start) {
                        "fn" => token::Kind::Fn,
                        "ret" => token::Kind::Ret,
                        "use" => token::Kind::Use,
                        "extern" => token::Kind::Extern,
                        "if" => token::Kind::If,
                        "else" => token::Kind::Else,
                        "true" => token::Kind::Bool(true),
                        "false" => token::Kind::Bool(false),
                        "pin" => token::Kind::Pin,
                        "loop" => token::Kind::Loop,
                        "break" => token::Kind::Break,
                        "struct" => token::Kind::Struct,
                        _ => token::Kind::Ident,
                    }
                }

                char => todo!("{char:?}"),
            };

            return self.new_token(kind, start);
        }
    }

    fn new_token(&mut self, kind: token::Kind, start: usize) -> Token {
        Token::new(kind, Span::new(self.source(), start, self.progress()))
    }

    fn next(&mut self) -> Option<char> {
        self.chars.next()
    }

    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    pub fn progress(&self) -> usize {
        self.str.len() - self.chars.as_str().len()
    }

    fn view(&self, start: usize) -> &str {
        &self.str[start..self.progress()]
    }

    fn source(&self) -> Source {
        self.source
    }

    pub fn display(&self, span: Span) -> &str {
        &self.str[span.range()]
    }

    pub fn pretty_print(&self, span: Span) -> SpanDisplay {
        SpanDisplay::new(self.source_content(), span)
    }

    pub fn source_content(&self) -> &str {
        self.str
    }
}

pub fn int_value(sources: &Sources, span: Span) -> u64 {
    let mut chars = sources.display(span).chars();
    let mut value = 0;
    while let Some(c @ '0'..='9') = chars.next() {
        value = value * 10 + (c as u64 - '0' as u64);
    }

    match chars.next() {
        None => return value,
        _ => todo!("unhandled int literal {:?}", sources.display(span)),
    }
}

impl IsOperator for char {
    fn self_(self) -> char {
        self
    }
}

pub trait IsOperator: Sized {
    fn self_(self) -> char;

    fn is_operator(self) -> bool {
        matches!(
            self.self_(),
            '+' | '-' | '*' | '/' | '%' | '^' | '=' | '<' | '>' | '!' | '&' | '|' | '?' | ':'
        )
    }
}

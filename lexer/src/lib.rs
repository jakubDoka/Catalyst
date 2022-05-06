#![feature(auto_traits)]
#![feature(negative_impls)]
#![feature(int_log)]
#![feature(bool_to_option)]

use lexer_types::*;
use std::str::Chars;

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
                None => return self.new_token(TokenKind::Eof, start),
            };

            self.next();

            let kind = match c {
                '\n' | ';' => TokenKind::NewLine,

                '{' => TokenKind::LeftCurly,
                '}' => TokenKind::RightCurly,

                '(' => TokenKind::LeftParen,
                ')' => TokenKind::RightParen,

                '[' => TokenKind::LeftBracket,
                ']' => TokenKind::RightBracket,

                ',' => TokenKind::Comma,
                '.' => TokenKind::Dot,
                '\'' => {
                    while let Some(true) = self.next().map(|c| c != '\'') {}
                    TokenKind::Char
                }

                '#' => TokenKind::Hash,

                '0'..='9' => {
                    while let Some('0'..='9') = self.peek() {
                        self.next();
                    }
                    TokenKind::Int(-1)
                }

                '"' => {
                    while let Some(true) = self.next().map(|c| c != '"') {}
                    TokenKind::String
                }

                _ if c.is_whitespace() => continue,
                _ if c.is_operator() => {
                    while let Some(true) = self.peek().map(|c| c.is_operator()) {
                        self.next();
                    }

                    match self.view(start) {
                        "->" => TokenKind::RightArrow,
                        ":" => TokenKind::Colon,
                        "::" => TokenKind::DoubleColon,
                        "/*" => {
                            let mut depth = 1;
                            while depth > 0 {
                                match (self.next(), self.next()) {
                                    (Some('*'), Some('/')) => {
                                        depth -= 1;
                                    }
                                    (Some('/'), Some('*')) => {
                                        depth += 1;
                                    }
                                    (None, _) => {
                                        break;
                                    }
                                    _ => {}
                                }
                            }
                            continue;
                        }
                        "//" => {
                            loop {
                                if let Some('\n') = self.next() {
                                    break;
                                }
                            }
                            continue;
                        }
                        _ => TokenKind::Operator,
                    }
                }
                _ if c.is_alphabetic() || c == '_' => {
                    while let Some(true) = self.peek().map(|c| c.is_alphanumeric() || c == '_') {
                        self.next();
                    }

                    match self.view(start) {
                        "fn" => TokenKind::Fn,
                        "return" => TokenKind::Return,
                        "use" => TokenKind::Use,
                        "extern" => TokenKind::Extern,
                        "if" => TokenKind::If,
                        "else" => TokenKind::Else,
                        "true" => TokenKind::Bool(true),
                        "false" => TokenKind::Bool(false),
                        "let" => TokenKind::Let,
                        "loop" => TokenKind::Loop,
                        "break" => TokenKind::Break,
                        "struct" => TokenKind::Struct,
                        "bound" => TokenKind::Bound,
                        "mut" => TokenKind::Mut,
                        "impl" => TokenKind::Impl,
                        "as" => TokenKind::As,
                        _ => TokenKind::Ident,
                    }
                }

                char => todo!("{char:?}"),
            };

            return self.new_token(kind, start);
        }
    }

    fn new_token(&mut self, kind: TokenKind, start: usize) -> Token {
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

    pub fn source_content(&self) -> &str {
        self.str
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

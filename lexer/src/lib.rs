#![feature(auto_traits)]
#![feature(negative_impls)]
#![feature(int_log)]
#![feature(bool_to_option)]

pub(self) use lexer_types::*;
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
                '\'' => {
                    while let Some(true) = self.next().map(|c| c != '\'') {}
                    token::Kind::Char
                }

                '#' => token::Kind::Hash,

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
                        _ => token::Kind::Operator,
                    }
                }
                _ if c.is_alphabetic() || c == '_' => {
                    while let Some(true) = self.peek().map(|c| c.is_alphanumeric() || c == '_') {
                        self.next();
                    }

                    match self.view(start) {
                        "fn" => token::Kind::Fn,
                        "return" => token::Kind::Return,
                        "use" => token::Kind::Use,
                        "extern" => token::Kind::Extern,
                        "if" => token::Kind::If,
                        "else" => token::Kind::Else,
                        "true" => token::Kind::Bool(true),
                        "false" => token::Kind::Bool(false),
                        "let" => token::Kind::Let,
                        "loop" => token::Kind::Loop,
                        "break" => token::Kind::Break,
                        "struct" => token::Kind::Struct,
                        "bound" => token::Kind::Bound,
                        "mut" => token::Kind::Mut,
                        "impl" => token::Kind::Impl,
                        "as" => token::Kind::As,
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

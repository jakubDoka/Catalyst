use std::str::Chars;

use crate::{
    prelude::{Span, SpanDisplay},
    source_info::Source,
    token::{self, Token},
};

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
                ':' => token::Kind::Colon,

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
                        _ => token::Kind::Operator,
                    }
                }
                _ if c.is_alphabetic() => {
                    while let Some(true) = self.peek().map(|c| c.is_alphanumeric()) {
                        self.next();
                    }

                    match self.view(start) {
                        "fn" => token::Kind::Fn,
                        "ret" => token::Kind::Ret,
                        "use" => token::Kind::Use,
                        _ => token::Kind::Ident,
                    }
                }

                _ => todo!(),
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

    pub fn display(&self, span: Span) -> SpanDisplay {
        SpanDisplay::new(self.source_content(), span)
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

#[cfg(test)]
mod test {
    use crate::{source_info::Source, token};

    use super::Lexer;
    use std::fmt::Write;

    #[test]
    fn test() {
        let test_string = "
            ident
            0 \"
        string
            \"
            fn ret use 
            () {}
            ;,
        ";

        let mut chars = Lexer::new(0, test_string, Source::default());
        let mut f = String::new();

        loop {
            let token = chars.next_token();
            if token.kind() == token::Kind::Eof {
                break;
            }
            token.range();
            writeln!(f, "{:?}", token.kind()).unwrap();
            token.span().pretty_print(&mut f, test_string).unwrap();
            writeln!(f).unwrap();
        }

        std::fs::write("test_out.txt", f).unwrap();
    }
}

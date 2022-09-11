use logos::Logos;

pub struct Lexer<'a> {
    inner: logos::Lexer<'a, Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            inner: Token::lexer(source),
        }
    }

    pub fn contains_line_comments(mut self) -> bool {
        self.inner.find(|t| t == &Token::LineComment).is_some()
    }

    pub fn translate(
        self,
        buffer: &mut String,
        last_n_line: &mut usize,
        indent: usize,
        preserve_newlines: bool,
    ) -> Option<()> {
        let str = self.inner.source();
        let mut lexer = self.inner.spanned().peekable();

        loop {
            let (tok, span) = lexer.next()?;
            match tok {
                Token::NewLine if preserve_newlines => {
                    if lexer.peek().map_or(true, |tok| tok.0 != Token::NewLine) {
                        *last_n_line += buffer.len();
                        buffer.push('\n');
                        for _ in 0..indent {
                            buffer.push('\t');
                        }
                    }
                }
                Token::MultiComment | Token::LineComment => {
                    if buffer
                        .chars()
                        .rev()
                        .next()
                        .map_or(false, |c| !c.is_whitespace())
                    {
                        buffer.push(' ');
                    }
                    buffer.push_str(&str[span].trim_end());
                }
                _ => (),
            }
        }
    }
}

#[derive(Logos, PartialEq, Eq)]
enum Token {
    #[token("\n")]
    NewLine,
    #[regex(r"/\*([^*]/|\*[^/]|[^*/])*\*/")]
    MultiComment,
    #[regex(r"//[^\n]*")]
    LineComment,
    #[regex(r".", logos::skip)]
    Stuff,

    #[error]
    Error,
}

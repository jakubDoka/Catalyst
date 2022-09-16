use logos::Logos;

use crate::Fmt;

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

    pub fn folded_len_between(self, line_length: usize) -> usize {
        let len = self
            .inner
            .spanned()
            .map(|(t, span)| match t {
                Token::MultiComment => span.len(),
                Token::LineComment => line_length,
                _ => 0,
            })
            .intersperse(1)
            .sum::<usize>();
        len + 2 * (len != 0) as usize
    }

    pub fn write_between(self, fmt: &mut Fmt, fold: bool, pos: usize) {
        let source = self.inner.source();
        let mut last_multi_comment = false;
        for (t, span) in self.inner.spanned() {
            match t {
                Token::NewLine if !fold => {
                    let newline_count = fmt.buffer.chars().rev().take_while(|&c| c == '\n').count();

                    if newline_count < 2 {
                        fmt.newline();
                    }
                }
                Token::MultiComment => {
                    if fmt.buffer.ends_with('\n') {
                        fmt.write_indent();
                    } else {
                        fmt.buffer.push(' ');
                    }

                    let last_new_line = source[..span.end].rfind('\n').map_or(0, |i| i + pos);
                    fmt.last_newline = last_new_line;
                    fmt.buffer.push_str(&source[span]);
                }
                Token::LineComment if !fold => unreachable!(),
                Token::LineComment => {
                    if fmt.buffer.ends_with('\n') {
                        fmt.write_indent();
                    } else {
                        fmt.buffer.push(' ');
                    }

                    fmt.buffer.push_str(source[span].trim_end());
                }
                _ => (),
            }

            last_multi_comment =
                (t == Token::MultiComment && !fold) || (last_multi_comment && t == Token::NewLine);
        }

        if last_multi_comment {
            fmt.buffer.push(' ');
        }
    }
}

#[derive(Logos, PartialEq, Eq, Clone, Copy)]
enum Token {
    #[token("\n")]
    NewLine,
    #[regex(r"/\*([^*]/|\*[^/]|[^*/])*\*/")]
    MultiComment,
    #[regex(r"//[^\n]*")]
    LineComment,

    #[error]
    Error,
}

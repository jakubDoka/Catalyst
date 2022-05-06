use lexer_types::*;
use ast::*;

pub enum AstError {
    ExpectedAssign {
        got: TokenKind,
        loc: Span,
    },
    UnexpectedToken {
        got: TokenKind,
        expected: Vec<TokenKind>,
        loc: Span,
    },
}

impl AstError {
    pub fn display(&self, sources: &Sources, to: &mut String) -> std::fmt::Result {
        use std::fmt::Write;
        match self {
            AstError::ExpectedAssign { got, loc } => {
                loc.loc_to(sources, to)?;
                loc.underline_error(sources, to, &|to| {
                    write!(to, "expected '=' but got {}", got.as_str())
                })?;
            }
            AstError::UnexpectedToken { got, expected, loc } => {
                loc.loc_to(sources, to)?;
                loc.underline_error(sources, to, &|to| {
                    write!(to, "expected ")?;
                    for (i, expected) in expected.iter().enumerate() {
                        if i > 0 {
                            write!(to, " | ")?;
                        }
                        write!(to, "{}", expected.as_str())?;
                    }
                    write!(to, " but got {}", got.as_str())
                })?;
            }
        }

        writeln!(to)?;

        Ok(())
    }
}

pub struct FileDisplay<'a> {
    file: &'a AstData,
    source: &'a str,
}

impl<'a> FileDisplay<'a> {
    pub fn new(file: &'a AstData, source: &'a str) -> Self {
        Self { file, source }
    }
}

impl std::fmt::Display for FileDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.file.fmt(f, Some(self.source))
    }
}
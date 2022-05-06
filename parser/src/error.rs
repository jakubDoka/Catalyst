use lexer_types::*;

pub enum Error {
    ExpectedAssign {
        got: token::Kind,
        loc: Span,
    },
    UnexpectedToken {
        got: token::Kind,
        expected: Vec<token::Kind>,
        loc: Span,
    },
}

impl Error {
    pub fn display(&self, sources: &Sources, to: &mut String) -> std::fmt::Result {
        use std::fmt::Write;
        match self {
            Error::ExpectedAssign { got, loc } => {
                loc.loc_to(sources, to)?;
                loc.underline_error(sources, to, &|to| {
                    write!(to, "expected '=' but got {}", got.as_str())
                })?;
            }
            Error::UnexpectedToken { got, expected, loc } => {
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
    file: &'a ast::Data,
    source: &'a str,
}

impl<'a> FileDisplay<'a> {
    pub fn new(file: &'a ast::Data, source: &'a str) -> Self {
        Self { file, source }
    }
}

impl std::fmt::Display for FileDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.file.fmt(f, Some(self.source))
    }
}
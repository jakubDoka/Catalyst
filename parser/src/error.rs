use errors::Palette;
use lexer::{token, Span, Sources};

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
                loc.underline_to(
                    Palette::error().bold(), 
                    '^', 
                    sources, 
                    to,
                    &|to| write!(to, "expected '=' but got {}", got.as_str())
                )?;
            },
            Error::UnexpectedToken { got, expected, loc } => {
                loc.loc_to(sources, to)?;
                loc.underline_to(
                    Palette::error().bold(), 
                    '^', 
                    sources, 
                    to,
                    &|to| {
                        write!(to, "expected ")?;
                        for (i, expected) in expected.iter().enumerate() {
                            if i > 0 {
                                write!(to, " | ")?;
                            }
                            write!(to, "{}", expected.as_str())?;
                        }
                        write!(to, " but got {}", got.as_str())
                    }
                )?;
            },
        }

        writeln!(to)?;

        Ok(())
    } 
}
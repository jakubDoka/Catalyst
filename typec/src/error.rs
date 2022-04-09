use parser::AnyError;

use crate::Ty;

pub type Error = AnyError<Kind>;

#[derive(Debug)]
pub enum Kind {
    Modules(modules::error::Kind),
    TypeMismatch(Ty, Ty),
    ArgCountMismatch(usize, usize),
    ExpectedValue,
    UnexpectedValue,
}

impl From<modules::error::Kind> for Kind {
    fn from(other: modules::error::Kind) -> Self {
        Kind::Modules(other)
    }
}

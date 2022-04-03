use parser::prelude::AnyError;

pub type Error = AnyError<Kind>;

#[derive(Debug)]
pub enum Kind {
    Modules(modules::error::Kind),
}

impl From<modules::error::Kind> for Kind {
    fn from(other: modules::error::Kind) -> Self {
        Kind::Modules(other)
    }
}

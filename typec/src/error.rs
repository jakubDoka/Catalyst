use modules::{
    logic::{Modules, Units},
    scope::ScopeItemLexicon,
};
use parser::AnyError;

use crate::{ty, Ty, Types};

pub type Error = AnyError<Kind>;

#[derive(Debug)]
pub enum Kind {
    Modules(modules::error::Kind),
    TypeMismatch(Ty, Ty),
    ArgCountMismatch(usize, usize),
    ExpectedValue,
    UnexpectedValue,
    InvalidBreak,
}

parser::impl_error_display!((self, sources, {
    modules: Modules,
    units: Units,
    types: Types,
    scope_item_lexicon: ScopeItemLexicon,
}, f) => {
    match self {
        Kind::Modules(kind) => {
            kind.print(sources, &(modules, units, scope_item_lexicon), f)?;
        }
        &Kind::TypeMismatch(expected, found) => {
            write!(f, "expected {}, found {}", ty::Display::new(types, sources, expected), ty::Display::new(types, sources, found))?;
        }
        Kind::ArgCountMismatch(found, expected) => {
            write!(f, "expected {} arguments, found {}", expected, found)?;
        }
        Kind::ExpectedValue => {
            write!(f, "expected value")?;
        }
        Kind::UnexpectedValue => {
            write!(f, "unexpected value")?;
        }
        Kind::InvalidBreak => {
            write!(f, "invalid break, allowed only inside loop")?;
        }
    };
});

impl From<modules::error::Kind> for Kind {
    fn from(other: modules::error::Kind) -> Self {
        Kind::Modules(other)
    }
}

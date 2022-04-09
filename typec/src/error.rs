use modules::{logic::{Modules, Units}, scope::ScopeItemLexicon};
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

parser::impl_error_display!((self, sources, {
    modules: Modules, 
    units: Units,
    scope_item_lexicon: ScopeItemLexicon, 
}, f) => {
    match self {
        Kind::Modules(kind) => {
            kind.print(sources, &(modules, units, scope_item_lexicon), f)?;
        }
        Kind::TypeMismatch(expected, found) => {
            write!(f, "expected {:?}, found {:?}", expected, found)?;
        }
        Kind::ArgCountMismatch(expected, found) => {
            write!(f, "expected {} arguments, found {}", expected, found)?;
        }
        Kind::ExpectedValue => {
            write!(f, "expected value")?;
        }
        Kind::UnexpectedValue => {
            write!(f, "unexpected value")?;
        }
    };
});

impl From<modules::error::Kind> for Kind {
    fn from(other: modules::error::Kind) -> Self {
        Kind::Modules(other)
    }
}

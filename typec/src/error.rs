use crate::*;
use modules::*;
use parser::*;

pub type Error = AnyError<Kind>;

#[derive(Debug)]
pub enum Kind {
    Modules(modules::error::Kind),
    /// (expected, found)
    TypeMismatch(Ty, Ty),
    /// (found, expected) // TODO: be more consistent
    ArgCountMismatch(usize, usize),
    ExpectedValue,
    UnexpectedValue,
    InvalidBreak,
    ExpectedStruct,
    UninitializedFields,
    UnknownField,
    ExpectedType,
    NotAssignable,
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
        Kind::ExpectedStruct => {
            write!(f, "expected struct type")?;
        }
        Kind::UninitializedFields => {
            // TODO: re-parse struct ast and print fields
            write!(f, "some fields of struct are uninitialized fields")?;
        }
        Kind::UnknownField => {
            write!(f, "type does not have this field")?;
        }
        Kind::ExpectedType => {
            write!(f, "expected this expression to have type")?;
        }
        Kind::NotAssignable => {
            write!(f, "expression is not assignable")?;
        }
    };
});

impl From<modules::error::Kind> for Kind {
    fn from(other: modules::error::Kind) -> Self {
        Kind::Modules(other)
    }
}

use crate::*;
use storage::*;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct Loc {
    pub name: VRef<str>,
    pub file: Maybe<VRef<str>>,
    pub span: Maybe<Span>,
    pub whole_span: Maybe<Span>,
}

impl Loc {
    pub fn new(
        name: VRef<str>,
        file: impl Into<Maybe<VRef<str>>>,
        span: impl Into<Maybe<Span>>,
        whole_span: impl Into<Maybe<Span>>,
    ) -> Self {
        Self {
            name,
            file: file.into(),
            span: span.into(),
            whole_span: whole_span.into(),
        }
    }
}

impl Invalid for Loc {
    unsafe fn invalid() -> Self {
        Self {
            name: VRef::invalid(),
            file: Maybe::none(),
            span: Maybe::none(),
            whole_span: Maybe::none(),
        }
    }

    fn is_invalid(&self) -> bool {
        self.name.is_invalid()
    }
}

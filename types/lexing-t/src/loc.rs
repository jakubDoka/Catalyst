use crate::*;
use storage::*;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct Loc {
    pub name: VRef<str>,
    pub file: Option<VRef<str>>,
    pub span: Option<Span>,
    pub whole_span: Option<Span>,
}

impl Loc {
    pub fn new(
        name: VRef<str>,
        file: impl Into<Option<VRef<str>>>,
        span: impl Into<Option<Span>>,
        whole_span: impl Into<Option<Span>>,
    ) -> Self {
        Self {
            name,
            file: file.into(),
            span: span.into(),
            whole_span: whole_span.into(),
        }
    }
}

use lexer::{
    prelude::{Source, SourceEnt, Span},
    token,
};

pub type Error = AnyError<Kind>;

#[derive(Debug)]
pub enum Kind {
    Expected(token::Kind, token::Kind),
    ExpectedFork(Vec<token::Kind>, token::Kind),
}

#[derive(Debug)]
pub struct AnyError<K> {
    kind: K,
    span: Span,
}

impl<K> AnyError<K> {
    pub fn new(kind: K, span: Span) -> Self {
        AnyError { kind, span }
    }

    pub fn kind(&self) -> &K {
        &self.kind
    }

    pub fn spanless(kind: K) -> Self {
        AnyError {
            kind,
            span: Span::default(),
        }
    }

    pub fn print_into<T: ErrorState>(
        &self,
        state: &T,
        f: &mut impl std::fmt::Write,
    ) -> std::fmt::Result
    where
        K: AnyErrorKind<T>,
    {
        let source_ent = state.source_data(self.span.source());
        self.span.pretty_print_with_line_info(
            f,
            source_ent.content(),
            source_ent.path(),
            source_ent.line_mapping(),
        )?;
        self.kind.print(state, f)
    }
}

impl<A, B: Into<A>> Convert<AnyError<A>> for AnyError<B> {
    fn convert(self) -> AnyError<A> {
        AnyError {
            kind: self.kind.into(),
            span: self.span,
        }
    }
}

pub trait Convert<T> {
    fn convert(self) -> T;
}

pub trait ErrorState {
    fn source_data(&self, source: Source) -> &SourceEnt;
}

pub trait AnyErrorKind<T> {
    fn print(&self, state: &T, f: &mut impl std::fmt::Write) -> std::fmt::Result;
}

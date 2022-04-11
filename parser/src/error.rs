use lexer::{token, Sources, Span};

pub type Error = AnyError<Kind>;

#[derive(Debug)]
pub enum Kind {
    Expected(token::Kind, token::Kind),
    ExpectedFork(Vec<token::Kind>, token::Kind),
    ExpectedAssignment,
}

#[macro_export]
macro_rules! impl_error_display {
    (($self:ident, $sources:ident, {$($name:ident: $ty:ty,)*}, $f:ident) => $body:tt) => {
        pub struct Display {
            $($name: $ty,)*
            error: Error,
            sources: lexer::Sources,
        }

        impl<'a> Display {
            pub fn new(sources: lexer::Sources, error: Error, $($name: $ty),*) -> Self {
                Self {
                    $($name,)*
                    error,
                    sources,
                }
            }
        }

        impl<'a> std::fmt::Display for Display {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                self.error.print_into(&($(&self.$name),*), &self.sources, f)
            }
        }

        impl $crate::error::AnyErrorKind<($(&$ty),*)> for Kind {
            fn print(&$self, $sources: &lexer::Sources, ($($name),*): &($(&$ty),*), $f: &mut impl std::fmt::Write) -> std::fmt::Result {
                $body

                Ok(())
            }
        }
    };
}

impl_error_display!((self, _sources, {}, f) => {
    match self {
        Kind::Expected(expected, found) => {
            write!(f, "expected {:?}, found {:?}", expected, found)?;
        }
        Kind::ExpectedFork(expected, found) => {
            let expected = expected.iter().map(|kind| format!("{:?}", kind)).collect::<Vec<_>>().join(" | ");
            write!(f, "expected one of {:?}, found {:?}", expected, found)?;
        }
        Kind::ExpectedAssignment => {
            write!(f, "expected assignment")?;
        }
    }
});

#[derive(Debug)]
pub struct AnyError<K> {
    pub kind: K,
    pub span: Span,
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

    pub fn print_into<T>(
        &self,
        state: &T,
        sources: &Sources,
        f: &mut impl std::fmt::Write,
    ) -> std::fmt::Result
    where
        K: AnyErrorKind<T>,
    {
        let source_ent = &sources[self.span.source()];
        self.span.pretty_print_with_line_info(
            f,
            source_ent.content(),
            source_ent.path(),
            source_ent.line_mapping(),
        )?;
        self.kind.print(sources, state, f)
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

pub trait AnyErrorKind<T> {
    fn print(&self, sources: &Sources, state: &T, f: &mut impl std::fmt::Write)
        -> std::fmt::Result;
}

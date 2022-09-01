use crate::*;
use storage::*;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct Loc {
    pub pos: Maybe<Pos>,
    pub name: Ident,
    pub file: Maybe<Ident>,
}

impl Loc {
    #[inline]
    pub fn new(pos: impl Into<Pos>, file: impl Into<Maybe<Ident>>, name: Ident) -> Self {
        Self {
            pos: pos.into().into(),
            name,
            file: file.into(),
        }
    }

    #[inline]
    pub fn builtin(name: Ident) -> Self {
        Self {
            pos: Maybe::none(),
            name,
            file: Maybe::none(),
        }
    }

    pub fn span(&self, interner: &Interner) -> Maybe<Span> {
        self.pos
            .expand()
            .map(|pos| Span::new(pos.0 as usize..pos.0 as usize + interner[self.name].len()))
            .into()
    }
}

impl Invalid for Loc {
    unsafe fn invalid() -> Self {
        Self {
            pos: Maybe::none(),
            name: Ident::invalid(),
            file: Maybe::none(),
        }
    }

    fn is_invalid(&self) -> bool {
        self.name.is_invalid()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Pos(u32);

impl Pos {
    #[inline]
    pub const fn new(pos: u32) -> Self {
        Self(pos)
    }

    #[inline]
    pub fn to_span(self, len: usize) -> Span {
        Span::new(self.0 as usize..self.0 as usize + len)
    }
}

impl From<Span> for Pos {
    #[inline]
    fn from(span: Span) -> Self {
        Self(span.start)
    }
}

impl Invalid for Pos {
    unsafe fn invalid() -> Self {
        Pos(u32::MAX)
    }
    fn is_invalid(&self) -> bool {
        self.0 == u32::MAX
    }
}

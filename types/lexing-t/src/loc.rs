use crate::*;
use storage::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Loc {
    pos: u32,
    ident: Ident,
}

impl Loc {
    #[inline]
    pub fn new(
        pos: Option<u32>,
        file: impl Into<Option<Ident>>,
        span_content: &str,
        interner: &mut Interner,
    ) -> Self {
        let ident = file
            .into()
            .map(|file| interner.intern(ident!((file.index() as u32), "`", span_content)))
            .unwrap_or_else(|| interner.intern(ident!(span_content)));
        Self {
            pos: pos.unwrap_or(u32::MAX),
            ident,
        }
    }

    #[inline]
    pub fn expand<'a>(&self, interner: &'a Interner) -> ExpandedLoc<'a> {
        let (file, content) = interner[self.ident]
            .split_once("`")
            .map(|(file, name)| {
                (
                    Some(
                        file.as_bytes()
                            .iter()
                            .rev()
                            .fold(0, |acc, &e| acc * 10 + (e - b'0') as usize),
                    ),
                    name,
                )
            })
            .unwrap_or((None, &interner[self.ident]));
        let file = file.map(Ident::new).into();
        let span = self
            .pos()
            .map(|pos| Span::new(pos as usize..pos as usize + content.len()));
        ExpandedLoc {
            span,
            file,
            content,
        }
    }

    #[inline]
    pub fn ident(&self) -> Ident {
        self.ident
    }

    #[inline]
    pub fn pos(&self) -> Option<u32> {
        (self.pos != u32::MAX).then_some(self.pos)
    }
}

impl Invalid for Loc {
    unsafe fn invalid() -> Self {
        Loc {
            pos: 0,
            ident: Ident::invalid(),
        }
    }

    fn is_invalid(&self) -> bool {
        self.ident.is_invalid()
    }
}

impl Default for Loc {
    fn default() -> Self {
        Self {
            pos: u32::MAX,
            ident: Ident::empty(),
        }
    }
}

pub struct ExpandedLoc<'a> {
    pub file: Option<Ident>,
    pub span: Option<Span>,
    pub content: &'a str,
}

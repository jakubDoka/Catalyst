use std::{
    fmt::Debug,
    ops::{Deref, Range},
};

use lexing_t::*;
use storage::*;

pub type AstData = Arena;

use lexing::TokenKind;

use crate::*;

#[derive(Clone, Copy, Debug)]
pub struct NameAst {
    pub ident: Ident,
    pub span: Span,
}

impl NameAst {
    pub fn from_path(ctx: &mut ParsingCtx<'_, '_>, path: Span) -> Self {
        let path_str = ctx.lexer.inner_span_str(path);
        let last_slash = path_str.rfind('/').map_or(0, |i| i + 1);
        let span = path.sliced(last_slash..);
        Self::new(ctx, span)
    }

    pub fn new(ctx: &mut ParsingCtx<'_, '_>, span: Span) -> Self {
        Self {
            ident: ctx.interner.intern_str(ctx.lexer.inner_span_str(span)),
            span,
        }
    }
}

impl<'a> Ast<'a> for NameAst {
    type Args = ();

    const NAME: &'static str = "name";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Result<Self, ()> {
        let span = ctx.expect_advance(TokenKind::Ident)?.span;
        Ok(Self::new(ctx, span))
    }

    fn span(&self) -> Span {
        self.span
    }
}

pub trait Ast<'a>: Copy {
    type Args = ();

    const NAME: &'static str;

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, args: Self::Args) -> Result<Self, ()>;
    fn span(&self) -> Span;

    fn parse(ctx: &mut ParsingCtx<'_, 'a>) -> Result<Self, ()>
    where
        Self::Args: Default,
    {
        Self::parse_args(ctx, Default::default())
    }

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a>, args: Self::Args) -> Result<Self, ()> {
        ctx.state.parse_stack.push(Self::NAME);
        let res = Self::parse_args_internal(ctx, args);
        ctx.state.parse_stack.pop().unwrap();
        res
    }
}

pub struct ListAst<'a, T, META> {
    pub start: Span,
    pub elements: &'a [ListElement<T>],
    pub end: Span,
    _ph: std::marker::PhantomData<META>,
}

impl<'a, T: Debug, META> Debug for ListAst<'a, T, META> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ListAst")
            .field("start", &self.start)
            .field("elements", &self.elements)
            .field("end", &self.end)
            .finish()
    }
}

impl<'a, T: Debug, META: ListAstMeta> ListAst<'a, T, META> {
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.elements.iter().map(|e| &e.value)
    }

    pub fn first_gap(&self) -> Range<usize>
    where
        T: Ast<'a>,
    {
        self.start.end()
            ..self
                .elements
                .first()
                .map(|f| f.value.span().start())
                .unwrap_or(self.end.start())
    }
}

impl<'a, T, META: ListAstMeta> Clone for ListAst<'a, T, META> {
    fn clone(&self) -> Self {
        Self {
            start: self.start.clone(),
            elements: self.elements.clone(),
            end: self.end.clone(),
            _ph: std::marker::PhantomData,
        }
    }
}

impl<'a, T, META: ListAstMeta> Copy for ListAst<'a, T, META> {}

impl<'a, T, META: ListAstMeta> Default for ListAst<'a, T, META> {
    fn default() -> Self {
        Self {
            start: Default::default(),
            elements: Default::default(),
            end: Default::default(),
            _ph: Default::default(),
        }
    }
}

impl<'a, T: Ast<'a>, META: ListAstMeta> Ast<'a> for ListAst<'a, T, META>
where
    T::Args: Default,
{
    const NAME: &'static str = "list";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Result<Self, ()> {
        let on_delim = ctx.at(META::START);
        let pos = ctx.state.current.span.sliced(..0);
        if META::OPTIONAL && !on_delim && !META::START.is_empty() {
            return Ok(Self {
                start: pos,
                elements: &[],
                end: pos,
                _ph: std::marker::PhantomData,
            });
        }

        let start = if META::START.is_empty() {
            pos
        } else {
            ctx.state.current.span
        };

        if on_delim {
            ctx.advance();
        }

        let mut elements: BumpVec<ListElement<T>> = bumpvec![];
        let end = loop {
            ctx.skip(TokenKind::NewLine);
            if ctx.at(META::END) {
                break ctx.advance().span;
            }

            let Ok(element) = T::parse(ctx) else {
                if let Some(span) = META::recover(ctx)? {
                    break span;
                } else {
                    continue;
                }
            };
            let element_span = element.span();

            if let Some(last) = elements.last_mut() && !META::SEP.is_empty() {
                if let Some(after_delim) = last.after_delim.as_mut_option() {
                    after_delim.end = element_span.start;
                } else {
                    last.after_value.end = element_span.start;
                }
            }

            let mut after_value = Span::new(element_span.end()..element_span.end());
            let mut after_delim = Maybe::none();

            let has_sep = ctx.at(META::SEP);
            if has_sep {
                after_value = after_value.joined(ctx.state.current.span.sliced(..0));
                after_delim =
                    Span::new(ctx.state.current.span.end()..ctx.state.current.span.end()).into();
                ctx.advance();
            }

            elements.push(ListElement {
                value: element,
                after_value,
                after_delim,
            });

            if !META::SEP.is_empty() && !has_sep && !ctx.at(META::END) {
                if META::END.is_empty() {
                    break element_span.sliced(element_span.end()..);
                }

                if let Some(span) = META::recover(ctx)? {
                    break span;
                } else {
                    continue;
                }
            }
        };

        if let Some(elem) = elements.last_mut() && !META::END.is_empty() {
            if let Some(after_delim) = elem.after_delim.as_mut_option() {
                after_delim.end = end.start;
            } else {
                elem.after_value.end = end.start;
            }
        }

        let elements = ctx.arena.alloc_slice(&elements);

        Ok(Self {
            start,
            elements,
            end,
            _ph: std::marker::PhantomData,
        })
    }

    fn span(&self) -> Span {
        self.start.joined(self.end)
    }
}

impl<'a, T, META: ListAstMeta> Deref for ListAst<'a, T, META> {
    type Target = [ListElement<T>];

    fn deref(&self) -> &Self::Target {
        self.elements
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ListElement<T> {
    pub value: T,
    pub after_value: Span,
    pub after_delim: Maybe<Span>,
}

pub trait ListAstMeta {
    const START: &'static [TokenPattern<'static>];
    const SEP: &'static [TokenPattern<'static>];
    const END: &'static [TokenPattern<'static>];
    const OPTIONAL: bool;

    fn recover(ctx: &mut ParsingCtx) -> Result<Option<Span>, ()> {
        let ending = ctx.recover(Self::SEP.iter().chain(Self::END))?;
        Ok(ctx.matches(Self::END, ending).then_some(ending.span))
    }
}

#[macro_export]
macro_rules! list_meta {
    ($name:ident ? $start:tt $sep:tt $end:tt) => {
        $crate::list_meta!($name true $start $sep $end);
    };

    ($name:ident $start:tt $sep:tt $end:tt) => {
        $crate::list_meta!($name false $start $sep $end);
    };

    ($name:ident $optional:literal $start:tt $sep:tt $end:tt) => {
        pub struct $name;
        impl $crate::ListAstMeta for $name {
            const START: &'static [$crate::TokenPattern<'static>] = $crate::list_meta!(__arg__ $start);
            const SEP: &'static [$crate::TokenPattern<'static>] = $crate::list_meta!(__arg__ $sep);
            const END: &'static [$crate::TokenPattern<'static>] = $crate::list_meta!(__arg__ $end);
            const OPTIONAL: bool = $optional;
        }
        const _: () = {
            assert!(!$name::SEP.is_empty() || !$name::END.is_empty());
            assert!(!$optional || !$name::START.is_empty());
        };
    };

    (__arg__ [$($elem:tt)+]) => {
        &[$($crate::list_meta!(__element__ $elem)),+]
    };

    (__arg__ none) => {
        &[]
    };

    (__arg__ $elem:tt) => {
        &[$crate::list_meta!(__element__ $elem)]
    };

    (__element__ $elem:ident) => {
        $crate::TokenPattern::Kind(lexing::TokenKind::$elem)
    };

    (__element__ $elem:literal) => {
        $crate::TokenPattern::Str($elem)
    };

}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum TokenPattern<'a> {
    Str(&'a str),
    Kind(TokenKind),
}

impl<'a> Into<TokenPattern<'a>> for TokenKind {
    fn into(self) -> TokenPattern<'a> {
        TokenPattern::Kind(self)
    }
}

impl<'a> Into<TokenPattern<'a>> for &'a str {
    fn into(self) -> TokenPattern<'a> {
        TokenPattern::Str(self)
    }
}

impl<'a> AsRef<TokenPattern<'a>> for TokenPattern<'a> {
    fn as_ref(&self) -> &Self {
        self
    }
}

impl TokenPattern<'_> {
    pub fn as_string(&self) -> String {
        match self {
            Self::Str(s) => format!("'{}'", s),
            Self::Kind(kind) => kind.as_str().to_string(),
        }
    }
}

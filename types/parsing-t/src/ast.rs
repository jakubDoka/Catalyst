use std::{any, default::default, fmt::Debug, ops::Deref};

use diags::*;
use lexing_t::*;
use storage::*;

pub type AstData = Arena;

use lexing::{Token, TokenKind};

use crate::{token_pattern::TokenPattern, *};

#[derive(Clone, Copy, Debug)]
pub struct WrappedAst<T> {
    pub start: Span,
    pub value: T,
    pub end: Span,
}

impl<'a, T: Ast<'a>> Ast<'a> for WrappedAst<T>
where
    T::Args: Default,
{
    type Args = (TokenPat<'static>, TokenPat<'static>);

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (start, end): Self::Args) -> Option<Self> {
        Some(Self {
            start: ctx
                .expect_advance(start, |ctx| WrapperMissing {
                    pattern: start.to_str(ctx),
                    value: any::type_name::<T>(),
                    loc: ctx.loc(),
                })?
                .span,
            value: ctx.parse()?,
            end: ctx
                .expect_advance(end, |ctx| WrapperMissing {
                    pattern: end.to_str(ctx),
                    value: any::type_name::<T>(),
                    loc: ctx.loc(),
                })?
                .span,
        })
    }

    fn span(&self) -> Span {
        self.start.joined(self.end)
    }
}

ctl_errors! {
    #[err => "expected {pattern} as the wrapper around {value}"]
    error WrapperMissing: fatal {
        #[err loc]
        pattern ref: String,
        value: &'static str,
        loc: SourceLoc,
    }
}

#[derive(Clone, Copy, Debug)]
pub struct NameAst {
    pub ident: Ident,
    pub span: Span,
}

impl NameAst {
    pub fn from_path(ctx: &mut ParsingCtx<'_, '_, '_>, path: Span) -> Self {
        let path_str = ctx.inner_span_str(path);
        let last_slash = path_str.rfind('/').map_or(0, |i| i + 1);
        let span = path.sliced(last_slash..);
        Self::new(ctx, span)
    }

    pub fn new(ctx: &mut ParsingCtx<'_, '_, '_>, span: Span) -> Self {
        Self {
            ident: ctx.interner.intern(&ctx.source_code[span.range()]),
            span,
        }
    }
}

impl<'a> Ast<'a> for NameAst {
    type Args = (bool, &'static str);

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (just_try, hint): Self::Args) -> Option<Self> {
        let span = if just_try {
            ctx.try_advance(TokenKind::Ident)?.span
        } else {
            ctx.expect_advance(TokenKind::Ident, |ctx| ExpectedName {
                hint,
                got: ctx.state.current.kind,
                loc: ctx.loc(),
            })?
            .span
        };
        Some(Self::new(ctx, span))
    }

    fn span(&self) -> Span {
        self.span
    }
}

ctl_errors! {
    #[err => "expected name of {hint} but got {got}"]
    error ExpectedName: fatal {
        #[err loc, "here"]
        hint: &'static str,
        got: TokenKind,
        loc: SourceLoc,
    }
}

pub trait Ast<'a>: Copy {
    type Args = ();

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, args: Self::Args) -> Option<Self>;
    fn span(&self) -> Span;

    fn parse(ctx: &mut ParsingCtx<'_, 'a, '_>) -> Option<Self>
    where
        Self::Args: Default,
    {
        Self::parse_args(ctx, default())
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ListAst<'a, T> {
    pub start: Span,
    pub elements: &'a [T],
    pub end: Span,
}

impl<'a, T> Default for ListAst<'a, T> {
    fn default() -> Self {
        Self {
            start: Span::default(),
            elements: &[],
            end: Span::default(),
        }
    }
}

impl<'a, T> ListAst<'a, T> {
    pub fn empty(pos: Span) -> Self {
        Self {
            start: pos,
            elements: &[],
            end: pos,
        }
    }
}

impl<'a, T: Ast<'a>> Ast<'a> for ListAst<'a, T>
where
    T::Args: Default + Clone,
{
    type Args = (ListAstSyntax<'a>, T::Args);

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (syntax, args): Self::Args) -> Option<Self> {
        let on_delim = ctx.at(syntax.start);
        let pos = ctx.state.current.span.as_start();

        if syntax.optional && !on_delim && !syntax.without_prefix() {
            return Some(Self::empty(pos));
        }

        let start = if syntax.without_prefix() {
            pos
        } else {
            ctx.state.current.span
        };

        if on_delim {
            ctx.advance();
        }

        let mut elements = bumpvec![];
        let end = loop {
            ctx.skip(TokenKind::NewLine);
            if ctx.at(syntax.end) {
                break ctx.advance().span;
            }

            let Some(element) = T::parse_args(ctx, args.clone()) else {
                if let Some(span) = syntax.recover(ctx)? {
                    break span;
                }
                continue;
            };

            elements.push(element);

            let has_sep = ctx.at(syntax.sep);

            if !syntax.without_infix() && !has_sep && !ctx.at(syntax.end) {
                if syntax.without_suffix() {
                    break element.span().as_end();
                }

                ctx.workspace.push(MissingListSep {
                    sep: syntax.sep.to_str(ctx),
                    item: any::type_name::<T>(),
                    loc: ctx.loc(),
                });
                if let Some(span) = syntax.recover(ctx)? {
                    break span;
                }
                continue;
            }
        };

        Some(Self {
            start,
            elements: ctx.arena.alloc_slice(&elements),
            end,
        })
    }

    fn span(&self) -> Span {
        self.start.joined(self.end)
    }
}

ctl_errors! {
    #[err => "missing {item} list separator '{sep}'"]
    error MissingListSep: fatal {
        #[err loc]
        sep ref: String,
        item: &'static str,
        loc: SourceLoc,
    }
}

impl<'a, T> Deref for ListAst<'a, T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        self.elements
    }
}

pub struct ListAstSyntax<'a> {
    pub start: &'a [TokenPat<'static>],
    pub sep: &'a [TokenPat<'static>],
    pub end: &'a [TokenPat<'static>],
    pub optional: bool,
}

impl<'a> ListAstSyntax<'a> {
    pub fn recover(&self, ctx: &mut ParsingCtx) -> Option<Option<Span>> {
        let ending = ctx.recover([self.sep, self.end])?;
        Some(ctx.matches(self.end, ending).then_some(ending.span))
    }

    fn without_prefix(&self) -> bool {
        self.start.is_empty()
    }

    fn without_suffix(&self) -> bool {
        self.end.is_empty()
    }

    fn without_infix(&self) -> bool {
        self.sep.is_empty()
    }
}

impl<'a> From<ListAstSyntax<'a>> for (ListAstSyntax<'a>, ()) {
    fn from(value: ListAstSyntax<'a>) -> Self {
        (value, ())
    }
}

#[macro_export]
macro_rules! list_syntax {
    (@arg [$($elem:tt)+]) => {
        &[$($crate::list_syntax!(@element $elem)),+]
    };

    (@arg none) => {
        &[]
    };

    (@arg $elem:tt) => {
        &[$crate::list_syntax!(@element $elem)]
    };

    (@element $elem:ident) => {
        $crate::TokenPat::Kind(lexing::TokenKind::$elem)
    };

    (@element $elem:literal) => {
        $crate::TokenPat::Str($elem)
    };

    (
        $(const $name:ident = ($($tt:tt)*);)*
    ) => {
        $(
            pub const $name: $crate::ListAstSyntax = $crate::list_syntax!($($tt)*);
        )*
    };

    (? $start:tt $sep:tt $end:tt) => {
        $crate::list_syntax!(true $start $sep $end);
    };

    ($start:tt $sep:tt $end:tt) => {
        $crate::list_syntax!(false $start $sep $end);
    };

    ($optional:literal $start:tt $sep:tt $end:tt) => {
        $crate::ListAstSyntax {
            start: $crate::list_syntax!(@arg $start),
            sep: $crate::list_syntax!(@arg $sep),
            end: $crate::list_syntax!(@arg $end),
            optional: $optional,
        }
    };
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum TokenPat<'a> {
    Str(&'a str),
    Kind(TokenKind),
}

impl TokenPattern for TokenPat<'_> {
    fn matches(&self, ctx: &ParsingCtx, token: Token) -> bool {
        match self {
            Self::Str(s) => <&str as TokenPattern>::matches(s, ctx, token),
            Self::Kind(kind) => kind.matches(ctx, token),
        }
    }

    fn to_str(&self, ctx: &ParsingCtx) -> String {
        match self {
            Self::Str(s) => <&str as TokenPattern>::to_str(s, ctx),
            Self::Kind(kind) => kind.to_str(ctx),
        }
    }
}

impl TokenPat<'_> {
    pub fn as_string(&self) -> String {
        match self {
            Self::Str(s) => format!("'{s}'"),
            Self::Kind(kind) => kind.as_str().to_string(),
        }
    }
}

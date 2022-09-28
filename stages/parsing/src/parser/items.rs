use super::*;

list_meta!(ItemsMeta none NewLine [Break Eof]);
pub type ItemsAst<'a> = ListAst<'a, ItemAst<'a>, ItemsMeta>;
pub type GroupedItemSlice<'a, T> = &'a [(T, &'a [TopLevelAttributeAst])];

#[derive(Clone, Copy)]
pub struct GroupedItemsAst<'a> {
    pub structs: GroupedItemSlice<'a, StructAst<'a>>,
    pub funcs: GroupedItemSlice<'a, FuncDefAst<'a>>,
    pub last: bool,
    pub span: Span,
}

impl<'a> Ast<'a> for GroupedItemsAst<'a> {
    type Args = ();

    const NAME: &'static str = "grouped items";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Option<Self> {
        let items = ctx.parse::<ItemsAst>()?;

        let last = items.end.is_empty();
        let span = items.span();

        let mut attrs = bumpvec![];
        let mut funcs = bumpvec![];
        let mut structs = bumpvec![];

        for &item in items.iter() {
            match item {
                ItemAst::Struct(&s) => structs.push((s, ctx.arena.alloc_iter(attrs.drain(..)))),
                ItemAst::Func(&f) => funcs.push((f, ctx.arena.alloc_iter(attrs.drain(..)))),
                ItemAst::Attribute(&a) => attrs.push(a),
            }
        }

        Some(Self {
            structs: ctx.arena.alloc_slice(structs.as_slice()),
            funcs: ctx.arena.alloc_slice(funcs.as_slice()),
            last,
            span,
        })
    }

    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ItemAst<'a> {
    Struct(&'a StructAst<'a>),
    Func(&'a FuncDefAst<'a>),
    Attribute(&'a TopLevelAttributeAst),
}

impl<'a> Ast<'a> for ItemAst<'a> {
    type Args = ();

    const NAME: &'static str = "item";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Option<Self> {
        let start = ctx.state.current.span;
        let vis = ctx.visibility();
        branch! { ctx => {
            Struct => ctx.parse_args((vis, start))
                .map(|s| ctx.arena.alloc(s))
                .map(ItemAst::Struct),
            Func => ctx.parse_args((vis, start))
                .map(|s| ctx.arena.alloc(s))
                .map(ItemAst::Func),
            Hash => ctx.parse()
                .map(|s| ctx.arena.alloc(s))
                .map(ItemAst::Attribute),
        }}
    }

    fn span(&self) -> Span {
        match self {
            ItemAst::Struct(s) => s.span(),
            ItemAst::Func(f) => f.span(),
            ItemAst::Attribute(a) => a.span(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct TopLevelAttributeAst {
    pub hash: Span,
    pub value: WrappedAst<TopLevelAttributeKindAst>,
}

impl<'a> Ast<'a> for TopLevelAttributeAst {
    type Args = ();

    const NAME: &'static str = "top level attribute";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Option<Self> {
        Some(Self {
            hash: ctx.advance().span,
            value: ctx.parse_args((
                TokenKind::LeftBracket.into(),
                TokenKind::RightBracket.into(),
            ))?,
        })
    }

    fn span(&self) -> Span {
        self.hash.joined(self.value.span())
    }
}

#[derive(Clone, Copy, Debug)]
pub enum TopLevelAttributeKindAst {
    Entry(Span),
    Inline(Option<WrappedAst<InlineModeAst>>),
}

impl<'a> Ast<'a> for TopLevelAttributeKindAst {
    type Args = ();

    const NAME: &'static str = "top level attribute";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Option<Self> {
        branch! {str ctx => {
            "entry" => Some(TopLevelAttributeKindAst::Entry(ctx.advance().span)),
            "inline" => {
                if ctx.at_tok(TokenKind::LeftParen) {
                    Some(TopLevelAttributeKindAst::Inline(None))
                } else {
                    ctx.parse_args((TokenKind::LeftParen.into(), TokenKind::RightParen.into()))
                        .map(Some)
                        .map(TopLevelAttributeKindAst::Inline)
                }
            },
        }}
    }

    fn span(&self) -> Span {
        match *self {
            TopLevelAttributeKindAst::Entry(span) => span,
            TopLevelAttributeKindAst::Inline(mode) => mode.map_or(Span::default(), |m| m.span()),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum InlineModeAst {
    Always(Span),
    Never(Span),
}

impl<'a> Ast<'a> for InlineModeAst {
    type Args = ();

    const NAME: &'static str = "inline mode";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Option<Self> {
        branch! {str ctx => {
            "always" => Some(InlineModeAst::Always(ctx.advance().span)),
            "never" => Some(InlineModeAst::Never(ctx.advance().span)),
        }}
    }

    fn span(&self) -> Span {
        match *self {
            InlineModeAst::Always(span) => span,
            InlineModeAst::Never(span) => span,
        }
    }
}

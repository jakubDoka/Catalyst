use super::*;

list_meta!(ItemsMeta none NewLine [Break Eof]);
pub type ItemsAst<'a> = ListAst<'a, ItemAst<'a>, ItemsMeta>;

#[derive(Clone, Copy)]
pub struct GroupedItemsAst<'a> {
    pub structs: &'a [StructAst<'a>],
    pub funcs: &'a [FuncDefAst<'a>],
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

        macro_rules! gen_groups {
            (
                $(
                    $name:ident => $field:ident,
                )*
            ) => {
                $(
                    let $field = ctx.arena.alloc_iter(items
                        .iter()
                        .filter_map(|&item| match item {
                            ItemAst::$name(&item) => Some(item),
                            _ => None,
                        }));
                )*

                Some(Self {
                    $(
                        $field,
                    )*
                    last,
                    span,
                })
            };
        }

        gen_groups! {
            Struct => structs,
            Func => funcs,
        }
    }

    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ItemAst<'a> {
    Struct(&'a StructAst<'a>),
    Func(&'a FuncDefAst<'a>),
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
        }}
    }

    fn span(&self) -> Span {
        match self {
            ItemAst::Struct(s) => s.span(),
            ItemAst::Func(f) => f.span(),
        }
    }
}

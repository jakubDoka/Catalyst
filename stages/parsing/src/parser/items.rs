use super::*;

list_meta!(ItemsMeta none NewLine [Break Eof]);
pub type ItemsAst<'a> = ListAst<'a, ItemAst<'a>, ItemsMeta>;

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

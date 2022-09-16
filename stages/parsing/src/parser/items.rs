use super::*;

list_meta!(ItemsMeta none NewLine [Break Eof]);
pub type ItemsAst<'a> = ListAst<'a, ItemAst<'a>, ItemsMeta>;

#[derive(Clone, Copy, Debug)]
pub enum ItemAst<'a> {
    Struct(&'a StructAst<'a>),
}

impl<'a> Ast<'a> for ItemAst<'a> {
    type Args = ();

    const NAME: &'static str = "item";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Result<Self, ()> {
        let start = ctx.state.current.span;
        let vis = ctx.visibility();
        branch! { ctx => {
            Struct => StructAst::parse_args(ctx, (vis, start))
                .map(|s| ctx.ast_data.alloc(s))
                .map(ItemAst::Struct),
        }}
    }

    fn span(&self) -> Span {
        match self {
            ItemAst::Struct(s) => s.span(),
        }
    }
}

// impl Parser<'_> {
//     pub fn parse_items(&mut self) -> (VSlice<Ast>, bool) {
//         self.parse_with(Self::take_items)
//     }

//     pub fn parse_items_spanned(&mut self) -> (VSlice<Ast>, Span, bool) {
//         let start = self.state.last_break.unwrap_or(0);
//         let (items, finished) = self.parse_items();
//         let end = self
//             .state
//             .last_break
//             .unwrap_or(self.state.current.span.end());
//         (items, Span::new(start..end), finished)
//     }

//     pub fn take_items(&mut self) -> errors::Result {
//         self.skip_newlines();
//         self.list(
//             None,
//             TokenKind::NewLine,
//             [TokenKind::Break, TokenKind::Eof],
//             Self::item,
//         )
//         .map(|span| self.state.last_break = Some(span.end()))
//     }

//     pub fn item(&mut self) -> errors::Result {
//         branch! { self => {
//             Func => self.r#fn()?,
//             Struct => self.r#struct()?,
//             Bound => self.bound()?,
//             Impl => self.r#impl()?,
//         }};
//         Ok(())
//     }
// }

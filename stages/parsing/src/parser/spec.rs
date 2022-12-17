use super::*;

#[derive(Clone, Copy, Debug)]
pub struct SpecExprAst<'a> {
    pub path: PathAst<'a>,
}

impl<'a> Ast<'a> for SpecExprAst<'a> {
    type Args = ();

    fn parse_args(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        Some(Self { path: ctx.parse()? })
    }

    fn span(&self) -> Span {
        self.path.span()
    }
}

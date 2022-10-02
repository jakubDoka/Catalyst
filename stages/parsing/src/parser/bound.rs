use super::*;

#[derive(Clone, Copy, Debug)]
pub enum SpecExprAst<'a> {
    Path(PathExprAst<'a>),
}

impl<'a> Ast<'a> for SpecExprAst<'a> {
    type Args = ();

    const NAME: &'static str = "bound expr";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a>, (): Self::Args) -> Option<Self> {
        let chain = PathExprAst::parse(ctx)?;
        Some(Self::Path(chain))
    }

    fn span(&self) -> Span {
        todo!()
    }
}

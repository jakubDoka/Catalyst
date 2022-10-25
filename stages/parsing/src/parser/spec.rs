use super::*;

#[derive(Clone, Copy, Debug)]
pub enum SpecExprAst<'a> {
    Path(PathExprAst<'a>),
    Instance(TyInstanceAst<'a>),
}

impl<'a> Ast<'a> for SpecExprAst<'a> {
    type Args = ();

    const NAME: &'static str = "bound expr";

    fn parse_args_internal(ctx: &mut ParsingCtx<'_, 'a, '_>, (): Self::Args) -> Option<Self> {
        branch! {ctx => {
            Ident => {
                let ident = ctx.parse();
                if ctx.at_tok(TokenKind::LeftBracket) {
                    Ast::parse_args(ctx, (ident?,)).map(Self::Instance)
                } else {
                    ident.map(Self::Path)
                }
            },
        }}
    }

    fn span(&self) -> Span {
        match self {
            SpecExprAst::Path(p) => p.span(),
            SpecExprAst::Instance(i) => i.span(),
        }
    }
}

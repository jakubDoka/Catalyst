use super::*;

#[derive(Clone, Copy, Debug)]
pub struct FuncDefAst<'a, M = NoTokenMeta> {
    pub vis: Option<VisAst<M>>,
    pub signature: FuncSigAst<'a, M>,
    pub body: FuncBodyAst<'a, M>,
}

impl<'a, M> Spanned for FuncDefAst<'a, M> {
    fn span(&self) -> Span {
        let vis = self.vis.as_ref().map(|vis| vis.source_meta.span);
        let start = vis.unwrap_or_else(|| self.signature.span());
        start.joined(self.body.span())
    }
}

#[derive(Clone, Copy, Debug)]
pub struct FuncSigAst<'a, M = NoTokenMeta> {
    pub keyword: SourceInfo<M>,
    pub cc: Option<SourceInfo<M>>,
    pub generics: Option<ListAst<'a, ParamAst<'a, M>, M>>,
    pub name: NameAst<M>,
    pub args: Option<ListAst<'a, FuncArgAst<'a, M>, M>>,
    pub ret: Option<(SourceInfo<M>, TyAst<'a, M>)>,
}

impl<'a, M> Spanned for FuncSigAst<'a, M> {
    fn span(&self) -> Span {
        let r#fn = self.keyword.span;
        let name = self.name.span();
        let args = self.args.as_ref().map(|args| args.span());
        let ret = self
            .ret
            .as_ref()
            .map(|(arrow, ty)| arrow.span.joined(ty.span()));
        let start = r#fn;
        let end = ret.or(args).unwrap_or(name);
        start.joined(end)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct FuncArgAst<'a, M = NoTokenMeta> {
    pub pat: PatAst<'a, M>,
    pub colon: SourceInfo<M>,
    pub ty: TyAst<'a, M>,
}

impl<'a, M> Spanned for FuncArgAst<'a, M> {
    fn span(&self) -> Span {
        self.pat.span().joined(self.ty.span())
    }
}

#[derive(Clone, Copy, Debug)]
pub enum FuncBodyAst<'a, M = NoTokenMeta> {
    Arrow(SourceInfo<M>, ExprAst<'a, M>),
    Block(ListAst<'a, ExprAst<'a, M>, M>),
    Extern(SourceInfo<M>),
}

impl<'a, M> Spanned for FuncBodyAst<'a, M> {
    fn span(&self) -> Span {
        match self {
            Self::Arrow(arrow, e) => arrow.span.joined(e.span()),
            Self::Block(b) => b.span(),
            Self::Extern(e) => e.span,
        }
    }
}

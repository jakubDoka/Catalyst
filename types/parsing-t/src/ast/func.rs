use super::*;

#[derive(Clone, Copy, Debug)]
pub struct FuncDefAst<'a, M = NoTokenMeta> {
    pub vis: Option<VisAst<M>>,
    pub signature: FuncSigAst<'a, M>,
    pub body: FuncBodyAst<'a, M>,
}

#[derive(Clone, Copy, Debug)]
pub struct FuncSigAst<'a, M = NoTokenMeta> {
    pub r#fn: SourceInfo<M>,
    pub cc: Option<SourceInfo<M>>,
    pub generics: Option<ListAst<'a, ParamAst<'a, M>, M>>,
    pub name: NameAst<M>,
    pub args: Option<ListAst<'a, FuncArgAst<'a, M>, M>>,
    pub ret: Option<(SourceInfo<M>, TyAst<'a, M>)>,
}

#[derive(Clone, Copy, Debug)]
pub struct FuncArgAst<'a, M = NoTokenMeta> {
    pub pat: PatAst<'a, M>,
    pub colon: SourceInfo<M>,
    pub ty: TyAst<'a, M>,
}

#[derive(Clone, Copy, Debug)]
pub enum FuncBodyAst<'a, M = NoTokenMeta> {
    Arrow(SourceInfo<M>, ExprAst<'a, M>),
    Block(ListAst<'a, ExprAst<'a, M>, M>),
    Extern(SourceInfo<M>),
}

impl<'a, M> FuncBodyAst<'a, M> {
    pub fn span(&self) -> Span {
        match self {
            Self::Arrow(arrow, e) => arrow.span.joined(e.span()),
            Self::Block(b) => b.span(),
            Self::Extern(e) => e.span,
        }
    }
}

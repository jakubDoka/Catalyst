use lexing::*;

use crate::*;

#[derive(Clone, Copy, Debug)]
pub struct SpecExprAst<'a, M = NoTokenMeta> {
    pub path: PathAst<'a, M>,
}

impl<'a, M> Spanned for SpecExprAst<'a, M> {
    fn span(&self) -> Span {
        self.path.span()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ParamAst<'a, M = NoTokenMeta> {
    pub name: NameAst<M>,
    pub specs: Option<ParamSpecsAst<'a, M>>,
}

impl<'a, M> Spanned for ParamAst<'a, M> {
    fn span(&self) -> Span {
        self.specs
            .as_ref()
            .map_or(self.name.span(), |e| self.name.span().joined(e.span()))
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ParamSpecsAst<'a, M = NoTokenMeta> {
    pub colon: SourceInfo<M>,
    pub first: SpecExprAst<'a, M>,
    pub rest: &'a [(SourceInfo<M>, SpecExprAst<'a, M>)],
}

impl<'a, M> ParamSpecsAst<'a, M> {
    pub fn specs(&self) -> impl Iterator<Item = &SpecExprAst<'a, M>> {
        std::iter::once(&self.first).chain(self.rest.iter().map(|(.., e)| e))
    }
}

impl<'a, M> Spanned for ParamSpecsAst<'a, M> {
    fn span(&self) -> Span {
        self.colon.span.joined(
            self.rest
                .last()
                .map_or(self.first.span(), |(.., e)| e.span()),
        )
    }
}

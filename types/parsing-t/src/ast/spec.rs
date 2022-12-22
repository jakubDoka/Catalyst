use lexing::SourceMeta;

use crate::*;

#[derive(Clone, Copy, Debug)]
pub struct SpecExprAst<'a, M> {
    pub path: PathAst<'a, M>,
}

#[derive(Clone, Copy, Debug)]
pub struct ParamAst<'a, M> {
    pub name: NameAst<M>,
    pub specs: Option<ParamSpecsAst<'a, M>>,
}

#[derive(Clone, Copy, Debug)]
pub struct ParamSpecsAst<'a, M> {
    pub colon: SourceMeta<M>,
    pub first: SpecExprAst<'a, M>,
    pub rest: &'a [(SourceMeta<M>, SpecExprAst<'a, M>)],
}

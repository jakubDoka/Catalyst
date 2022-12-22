use lexing::SourceInfo;

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
    pub colon: SourceInfo<M>,
    pub first: SpecExprAst<'a, M>,
    pub rest: &'a [(SourceInfo<M>, SpecExprAst<'a, M>)],
}

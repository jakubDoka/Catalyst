use lexing::*;

use crate::{ListAst, NameAst};

#[derive(Clone, Copy, Debug)]
pub enum PathSegment<'a, M> {
    Name(NameAst<M>),
    Params(ListAst<'a, TyAst<'a, M>, M>),
}

#[derive(Clone, Copy, Debug)]
pub struct PathAst<'a, M> {
    pub leading_slash: Option<SourceMeta<M>>,
    pub start: PathSegment<'a, M>,
    pub segments: &'a [PathSegment<'a, M>],
}

#[derive(Clone, Copy, Debug)]
pub enum TyAst<'a, M> {
    Path(PathAst<'a, M>),
    Pointer(&'a TyPointerAst<'a, M>),
    Tuple(ListAst<'a, TyAst<'a, M>, M>),
    Wildcard(SourceMeta<M>),
}

#[derive(Clone, Copy, Debug)]
pub struct TyPointerAst<'a, M> {
    pub carrot: SourceMeta<M>,
    pub mutability: MutabilityAst<'a, M>,
    pub ty: TyAst<'a, M>,
}

#[derive(Clone, Copy, Debug)]
pub enum MutabilityAst<'a, M> {
    Mut(SourceMeta<M>),
    None,
    Generic(SourceMeta<M>, PathAst<'a, M>),
}

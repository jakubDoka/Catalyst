pub mod expr;
pub mod func;
pub mod items;
pub mod manifest;
pub mod pat;
pub mod spec;
pub mod ty;

use std::ops::Deref;

use crate::*;
use lexing::*;
use storage::*;

#[derive(Clone, Copy, Debug)]
pub struct ListAst<'a, T, M> {
    pub start: SourceInfo<M>,
    pub elements: &'a [ListElemAst<T, M>],
    pub end: SourceInfo<M>,
}

impl<'a, T, M> ListAst<'a, T, M> {
    pub fn span(&self) -> Span {
        self.start.span.joined(self.end.span)
    }
}

impl<T, M> Deref for ListAst<'_, T, M> {
    type Target = [ListElemAst<T, M>];

    fn deref(&self) -> &Self::Target {
        self.elements
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ListElemAst<T, M> {
    pub value: T,
    pub delim: Option<SourceInfo<M>>,
}

impl<T, M> Deref for ListElemAst<T, M> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

#[derive(Clone, Copy, Debug)]
pub struct WrappedAst<T, M> {
    pub start: SourceInfo<M>,
    pub value: T,
    pub end: SourceInfo<M>,
}

impl<T, M> WrappedAst<T, M> {
    pub fn span(&self) -> Span {
        self.start.span.joined(self.end.span)
    }
}

impl<T, M> Deref for WrappedAst<T, M> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

#[derive(Clone, Copy, Debug)]
pub struct NameAst<M> {
    pub source_meta: SourceInfo<M>,
    pub ident: Ident,
}

impl<M> Deref for NameAst<M> {
    type Target = SourceInfo<M>;

    fn deref(&self) -> &Self::Target {
        &self.source_meta
    }
}

#[derive(Clone, Copy, Debug)]
pub struct VisAst<M> {
    pub source_meta: SourceInfo<M>,
    pub vis: Vis,
}

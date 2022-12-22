use super::*;

#[derive(Debug, Clone, Copy)]
pub enum ExprAst<'a, M> {
    Unit(&'a UnitExprAst<'a, M>),
    Binary(&'a BinaryExprAst<'a, M>),
}

#[derive(Debug, Clone, Copy)]
pub struct BinaryExprAst<'a, M> {
    pub lhs: ExprAst<'a, M>,
    pub op: NameAst<M>,
    pub rhs: ExprAst<'a, M>,
}

#[derive(Debug, Clone, Copy)]
pub enum UnitExprAst<'a, M> {
    StructCtor(StructCtorAst<'a, M>),
    EnumCtor(EnumCtorAst<'a, M>),
    DotExpr(&'a DotExprAst<'a, M>),
    Call(&'a CallExprAst<'a, M>),
    Path(PathAst<'a, M>),
    Return(ReturnExprAst<'a, M>),
    Int(SourceMeta<M>),
    Char(SourceMeta<M>),
    Bool(SourceMeta<M>),
    Match(MatchExprAst<'a, M>),
    If(IfAst<'a, M>),
    Loop(LoopAst<'a, M>),
    Break(BreakAst<'a, M>),
    Continue(ContinueAst<M>),
    Let(LetAst<'a, M>),
    Deref(SourceMeta<M>, &'a UnitExprAst<'a, M>),
    Ref(SourceMeta<M>, MutabilityAst<'a, M>, &'a UnitExprAst<'a, M>),
    Block(ListAst<'a, ExprAst<'a, M>, M>),
}

#[derive(Debug, Clone, Copy)]
pub struct ReturnExprAst<'a, M> {
    pub return_span: SourceMeta<M>,
    pub expr: Option<ExprAst<'a, M>>,
}

#[derive(Debug, Clone, Copy)]
pub struct MatchArmAst<'a, M> {
    pub pattern: PatAst<'a, M>,
    pub body: IfBlockAst<'a, M>,
}

#[derive(Debug, Clone, Copy)]
pub struct MatchExprAst<'a, M> {
    pub r#match: SourceMeta<M>,
    pub expr: ExprAst<'a, M>,
    pub body: ListAst<'a, MatchArmAst<'a, M>, M>,
}

#[derive(Debug, Clone, Copy)]
pub enum IfBlockAst<'a, M> {
    Block(ListAst<'a, ExprAst<'a, M>, M>),
    Arrow(SourceMeta<M>, ExprAst<'a, M>),
}

#[derive(Debug, Clone, Copy)]
pub struct ElifAst<'a, M> {
    pub elif: SourceMeta<M>,
    pub cond: ExprAst<'a, M>,
    pub body: IfBlockAst<'a, M>,
}

#[derive(Debug, Clone, Copy)]
pub struct IfAst<'a, M> {
    pub r#if: SourceMeta<M>,
    pub cond: ExprAst<'a, M>,
    pub body: IfBlockAst<'a, M>,
    pub elifs: &'a [ElifAst<'a, M>],
    pub r#else: Option<(SourceMeta<M>, IfBlockAst<'a, M>)>,
}

#[derive(Debug, Clone, Copy)]
pub struct ContinueAst<M> {
    pub r#continue: SourceMeta<M>,
    pub label: Option<NameAst<M>>,
}

#[derive(Debug, Clone, Copy)]
pub struct LoopAst<'a, M> {
    pub r#loop: SourceMeta<M>,
    pub label: Option<NameAst<M>>,
    pub body: ExprAst<'a, M>,
}

#[derive(Debug, Clone, Copy)]
pub struct BreakAst<'a, M> {
    pub r#break: SourceMeta<M>,
    pub label: Option<NameAst<M>>,
    pub value: Option<ExprAst<'a, M>>,
}

#[derive(Debug, Clone, Copy)]
pub struct CallExprAst<'a, M> {
    pub callable: UnitExprAst<'a, M>,
    pub args: ListAst<'a, ExprAst<'a, M>, M>,
}

#[derive(Debug, Clone, Copy)]
pub struct DotExprAst<'a, M> {
    pub lhs: UnitExprAst<'a, M>,
    pub dot: SourceMeta<M>,
    pub rhs: PathAst<'a, M>,
}

#[derive(Debug, Clone, Copy)]
pub struct StructCtorAst<'a, M> {
    pub path: Option<PathAst<'a, M>>,
    pub slash: SourceMeta<M>,
    pub body: ListAst<'a, StructCtorFieldAst<'a, M>, M>,
}

#[derive(Debug, Clone, Copy)]
pub struct StructCtorFieldAst<'a, M> {
    pub name: NameAst<M>,
    pub colon: SourceMeta<M>,
    pub value: ExprAst<'a, M>,
}

#[derive(Debug, Clone, Copy)]
pub struct EnumCtorAst<'a, M> {
    pub path: PathAst<'a, M>,
    pub value: Option<(SourceMeta<M>, ExprAst<'a, M>)>,
}

#[derive(Debug, Clone, Copy)]
pub struct LetAst<'a, M> {
    pub r#let: SourceMeta<M>,
    pub pat: PatAst<'a, M>,
    pub ty: Option<(SourceMeta<M>, TyAst<'a, M>)>,
    pub equal: SourceMeta<M>,
    pub value: ExprAst<'a, M>,
}

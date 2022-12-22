use super::*;

#[derive(Debug, Clone, Copy)]
pub enum ExprAst<'a, M> {
    Unit(&'a UnitExprAst<'a, M>),
    Binary(&'a BinaryExprAst<'a, M>),
}

impl<'a, M> ExprAst<'a, M> {
    pub fn span(&self) -> Span {
        match self {
            ExprAst::Unit(e) => e.span(),
            ExprAst::Binary(e) => e.span(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BinaryExprAst<'a, M> {
    pub lhs: ExprAst<'a, M>,
    pub op: NameAst<M>,
    pub rhs: ExprAst<'a, M>,
}

impl<'a, M> BinaryExprAst<'a, M> {
    pub fn span(&self) -> Span {
        self.lhs.span().joined(self.rhs.span())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnitExprAst<'a, M> {
    StructCtor(StructCtorAst<'a, M>),
    EnumCtor(EnumCtorAst<'a, M>),
    DotExpr(&'a DotExprAst<'a, M>),
    Call(&'a CallAst<'a, M>),
    Path(PathAst<'a, M>),
    Return(ReturnExprAst<'a, M>),
    Int(SourceInfo<M>),
    Char(SourceInfo<M>),
    Bool(SourceInfo<M>),
    Match(MatchExprAst<'a, M>),
    If(IfAst<'a, M>),
    Loop(LoopAst<'a, M>),
    Break(BreakAst<'a, M>),
    Continue(ContinueAst<M>),
    Let(LetAst<'a, M>),
    Deref(SourceInfo<M>, &'a UnitExprAst<'a, M>),
    Ref(
        SourceInfo<M>,
        Option<MutabilityAst<'a, M>>,
        &'a UnitExprAst<'a, M>,
    ),
    Block(ListAst<'a, ExprAst<'a, M>, M>),
}

impl<'a, M> UnitExprAst<'a, M> {
    pub fn span(&self) -> Span {
        match self {
            UnitExprAst::StructCtor(e) => e.span(),
            UnitExprAst::EnumCtor(e) => e.span(),
            UnitExprAst::DotExpr(e) => e.span(),
            UnitExprAst::Call(e) => e.span(),
            UnitExprAst::Path(e) => e.span(),
            UnitExprAst::Return(e) => e.span(),
            UnitExprAst::Int(e) => e.span,
            UnitExprAst::Char(e) => e.span,
            UnitExprAst::Bool(e) => e.span,
            UnitExprAst::Match(e) => e.span(),
            UnitExprAst::If(e) => e.span(),
            UnitExprAst::Loop(e) => e.span(),
            UnitExprAst::Break(e) => e.span(),
            UnitExprAst::Continue(e) => e.span(),
            UnitExprAst::Let(e) => e.span(),
            UnitExprAst::Deref(e, expr) => e.span.joined(expr.span()),
            UnitExprAst::Ref(e, _, expr) => e.span.joined(expr.span()),
            UnitExprAst::Block(e) => e.span(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ReturnExprAst<'a, M> {
    pub r#return: SourceInfo<M>,
    pub expr: Option<ExprAst<'a, M>>,
}

impl<'a, M> ReturnExprAst<'a, M> {
    pub fn span(&self) -> Span {
        self.expr
            .as_ref()
            .map_or(self.r#return.span, |e| self.r#return.span.joined(e.span()))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MatchArmAst<'a, M> {
    pub pattern: PatAst<'a, M>,
    pub body: BranchAst<'a, M>,
}

impl<'a, M> MatchArmAst<'a, M> {
    pub fn span(&self) -> Span {
        self.pattern.span().joined(self.body.span())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MatchExprAst<'a, M> {
    pub r#match: SourceInfo<M>,
    pub expr: ExprAst<'a, M>,
    pub body: ListAst<'a, MatchArmAst<'a, M>, M>,
}

impl<'a, M> MatchExprAst<'a, M> {
    pub fn span(&self) -> Span {
        self.r#match.span.joined(self.body.span())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BranchAst<'a, M> {
    Block(ListAst<'a, ExprAst<'a, M>, M>),
    Arrow(SourceInfo<M>, ExprAst<'a, M>),
}

impl<'a, M> BranchAst<'a, M> {
    pub fn span(&self) -> Span {
        match self {
            BranchAst::Block(e) => e.span(),
            BranchAst::Arrow(e, expr) => e.span.joined(expr.span()),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ElifAst<'a, M> {
    pub elif: SourceInfo<M>,
    pub cond: ExprAst<'a, M>,
    pub body: BranchAst<'a, M>,
}

impl<'a, M> ElifAst<'a, M> {
    pub fn span(&self) -> Span {
        self.elif.span.joined(self.body.span())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IfAst<'a, M> {
    pub r#if: SourceInfo<M>,
    pub cond: ExprAst<'a, M>,
    pub body: BranchAst<'a, M>,
    pub elifs: &'a [ElifAst<'a, M>],
    pub r#else: Option<(SourceInfo<M>, BranchAst<'a, M>)>,
}

impl<'a, M> IfAst<'a, M> {
    pub fn span(&self) -> Span {
        let mut span = self.r#if.span;
        if let Some((r#else, body)) = &self.r#else {
            span = span.joined(r#else.span.joined(body.span()));
        } else if let Some(elif) = self.elifs.last() {
            span = span.joined(elif.span());
        } else {
            span = span.joined(self.body.span());
        }
        span
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ContinueAst<M> {
    pub r#continue: SourceInfo<M>,
    pub label: Option<NameAst<M>>,
}

impl<M> ContinueAst<M> {
    pub fn span(&self) -> Span {
        self.label.as_ref().map_or(self.r#continue.span, |e| {
            self.r#continue.span.joined(e.span)
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LoopAst<'a, M> {
    pub r#loop: SourceInfo<M>,
    pub label: Option<NameAst<M>>,
    pub body: ExprAst<'a, M>,
}

impl<'a, M> LoopAst<'a, M> {
    pub fn span(&self) -> Span {
        self.r#loop.span.joined(self.body.span())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BreakAst<'a, M> {
    pub r#break: SourceInfo<M>,
    pub label: Option<NameAst<M>>,
    pub value: Option<ExprAst<'a, M>>,
}

impl<'a, M> BreakAst<'a, M> {
    pub fn span(&self) -> Span {
        if let Some(value) = &self.value {
            self.r#break.span.joined(value.span())
        } else {
            self.label
                .as_ref()
                .map_or(self.r#break.span, |e| self.r#break.span.joined(e.span))
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CallAst<'a, M> {
    pub callable: UnitExprAst<'a, M>,
    pub args: ListAst<'a, ExprAst<'a, M>, M>,
}

impl<'a, M> CallAst<'a, M> {
    pub fn span(&self) -> Span {
        self.callable.span().joined(self.args.span())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DotExprAst<'a, M> {
    pub lhs: UnitExprAst<'a, M>,
    pub dot: SourceInfo<M>,
    pub rhs: PathAst<'a, M>,
}

impl<'a, M> DotExprAst<'a, M> {
    pub fn span(&self) -> Span {
        self.lhs.span().joined(self.rhs.span())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StructCtorAst<'a, M> {
    pub path: Option<PathAst<'a, M>>,
    pub slash: SourceInfo<M>,
    pub body: ListAst<'a, StructCtorFieldAst<'a, M>, M>,
}

impl<'a, M> StructCtorAst<'a, M> {
    pub fn span(&self) -> Span {
        self.path
            .as_ref()
            .map_or(self.slash.span, |e| e.span().joined(self.slash.span))
            .joined(self.body.span())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StructCtorFieldAst<'a, M> {
    pub name: NameAst<M>,
    pub colon: SourceInfo<M>,
    pub value: ExprAst<'a, M>,
}

impl<'a, M> StructCtorFieldAst<'a, M> {
    pub fn span(&self) -> Span {
        self.name.span.joined(self.value.span())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct EnumCtorAst<'a, M> {
    pub path: PathAst<'a, M>,
    pub value: Option<(SourceInfo<M>, ExprAst<'a, M>)>,
}

impl<'a, M> EnumCtorAst<'a, M> {
    pub fn span(&self) -> Span {
        self.value
            .as_ref()
            .map_or(self.path.span(), |(_, e)| self.path.span().joined(e.span()))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LetAst<'a, M> {
    pub r#let: SourceInfo<M>,
    pub pat: PatAst<'a, M>,
    pub ty: Option<(SourceInfo<M>, TyAst<'a, M>)>,
    pub equal: SourceInfo<M>,
    pub value: ExprAst<'a, M>,
}

impl<'a, M> LetAst<'a, M> {
    pub fn span(&self) -> Span {
        self.r#let.span.joined(self.value.span())
    }
}

use super::*;

#[derive(Debug, Clone, Copy)]
pub enum ExprAst<'a, M = NoTokenMeta> {
    Unit(&'a UnitExprAst<'a, M>),
    Binary(&'a BinaryExprAst<'a, M>),
}

impl<'a, M> Spanned for ExprAst<'a, M> {
    fn span(&self) -> Span {
        match self {
            ExprAst::Unit(e) => e.span(),
            ExprAst::Binary(e) => e.span(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BinaryExprAst<'a, M = NoTokenMeta> {
    pub lhs: ExprAst<'a, M>,
    pub op: NameAst<M>,
    pub rhs: ExprAst<'a, M>,
}

impl<'a, M> Spanned for BinaryExprAst<'a, M> {
    fn span(&self) -> Span {
        self.lhs.span().joined(self.rhs.span())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnitExprAst<'a, M = NoTokenMeta> {
    StructCtor(StructCtorAst<'a, M>),
    EnumCtor(EnumCtorAst<'a, M>),
    DotExpr(&'a DotExprAst<'a, M>),
    Call(&'a CallAst<'a, M>),
    Path(PathAst<'a, M>),
    Return(ReturnAst<'a, M>),
    Int(SourceInfo<M>),
    Float(SourceInfo<M>),
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
    Array(ListAst<'a, ExprAst<'a, M>, M>),
}

impl<'a, M> Spanned for UnitExprAst<'a, M> {
    fn span(&self) -> Span {
        use UnitExprAst::*;
        match self {
            StructCtor(e) => e.span(),
            EnumCtor(e) => e.span(),
            DotExpr(e) => e.span(),
            Call(e) => e.span(),
            Path(e) => e.span(),
            Return(e) => e.span(),
            Int(e) | Float(e) | Char(e) | Bool(e) => e.span,
            Match(e) => e.span(),
            If(e) => e.span(),
            Loop(e) => e.span(),
            Break(e) => e.span(),
            Continue(e) => e.span(),
            Let(e) => e.span(),
            Deref(e, expr) => e.span.joined(expr.span()),
            Ref(e, _, expr) => e.span.joined(expr.span()),
            Block(e) => e.span(),
            Array(e) => e.span(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ReturnAst<'a, M = NoTokenMeta> {
    pub keyword: SourceInfo<M>,
    pub expr: Option<ExprAst<'a, M>>,
}

impl<'a, M> Spanned for ReturnAst<'a, M> {
    fn span(&self) -> Span {
        self.expr
            .as_ref()
            .map_or(self.keyword.span, |e| self.keyword.span.joined(e.span()))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MatchArmAst<'a, M = NoTokenMeta> {
    pub pattern: PatAst<'a, M>,
    pub body: BranchAst<'a, M>,
}

impl<'a, M> Spanned for MatchArmAst<'a, M> {
    fn span(&self) -> Span {
        self.pattern.span().joined(self.body.span())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MatchExprAst<'a, M = NoTokenMeta> {
    pub keyword: SourceInfo<M>,
    pub expr: ExprAst<'a, M>,
    pub body: ListAst<'a, MatchArmAst<'a, M>, M>,
}

impl<'a, M> Spanned for MatchExprAst<'a, M> {
    fn span(&self) -> Span {
        self.keyword.span.joined(self.body.span())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BranchAst<'a, M = NoTokenMeta> {
    Block(ListAst<'a, ExprAst<'a, M>, M>),
    Arrow(SourceInfo<M>, ExprAst<'a, M>),
}

impl<'a, M> Spanned for BranchAst<'a, M> {
    fn span(&self) -> Span {
        match self {
            BranchAst::Block(e) => e.span(),
            BranchAst::Arrow(e, expr) => e.span.joined(expr.span()),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ElifAst<'a, M = NoTokenMeta> {
    pub keyword: SourceInfo<M>,
    pub cond: ExprAst<'a, M>,
    pub body: BranchAst<'a, M>,
}

impl<'a, M> Spanned for ElifAst<'a, M> {
    fn span(&self) -> Span {
        self.keyword.span.joined(self.body.span())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IfAst<'a, M = NoTokenMeta> {
    pub keyword: SourceInfo<M>,
    pub cond: ExprAst<'a, M>,
    pub body: BranchAst<'a, M>,
    pub elifs: &'a [ElifAst<'a, M>],
    pub r#else: Option<(SourceInfo<M>, BranchAst<'a, M>)>,
}

impl<'a, M> Spanned for IfAst<'a, M> {
    fn span(&self) -> Span {
        let mut span = self.keyword.span;
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
pub struct ContinueAst<M = NoTokenMeta> {
    pub keyword: SourceInfo<M>,
    pub label: Option<NameAst<M>>,
}

impl<M> Spanned for ContinueAst<M> {
    fn span(&self) -> Span {
        self.label
            .as_ref()
            .map_or(self.keyword.span, |e| self.keyword.span.joined(e.span))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LoopAst<'a, M = NoTokenMeta> {
    pub keyword: SourceInfo<M>,
    pub label: Option<NameAst<M>>,
    pub body: ExprAst<'a, M>,
}

impl<'a, M> Spanned for LoopAst<'a, M> {
    fn span(&self) -> Span {
        self.keyword.span.joined(self.body.span())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BreakAst<'a, M = NoTokenMeta> {
    pub keyword: SourceInfo<M>,
    pub label: Option<NameAst<M>>,
    pub value: Option<ExprAst<'a, M>>,
}

impl<'a, M> Spanned for BreakAst<'a, M> {
    fn span(&self) -> Span {
        if let Some(value) = &self.value {
            self.keyword.span.joined(value.span())
        } else {
            self.label
                .as_ref()
                .map_or(self.keyword.span, |e| self.keyword.span.joined(e.span))
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CallAst<'a, M = NoTokenMeta> {
    pub callable: UnitExprAst<'a, M>,
    pub args: ListAst<'a, ExprAst<'a, M>, M>,
}

impl<'a, M> Spanned for CallAst<'a, M> {
    fn span(&self) -> Span {
        self.callable.span().joined(self.args.span())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DotExprAst<'a, M = NoTokenMeta> {
    pub lhs: UnitExprAst<'a, M>,
    pub infix: SourceInfo<M>,
    pub rhs: PathAst<'a, M>,
}

impl<'a, M> Spanned for DotExprAst<'a, M> {
    fn span(&self) -> Span {
        self.lhs.span().joined(self.rhs.span())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StructCtorAst<'a, M = NoTokenMeta> {
    pub path: Option<PathAst<'a, M>>,
    pub slash: SourceInfo<M>,
    pub body: ListAst<'a, StructCtorFieldAst<'a, M>, M>,
}

impl<'a, M> Spanned for StructCtorAst<'a, M> {
    fn span(&self) -> Span {
        self.path
            .as_ref()
            .map_or(self.slash.span, |e| e.span().joined(self.slash.span))
            .joined(self.body.span())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StructCtorFieldAst<'a, M = NoTokenMeta> {
    pub name: NameAst<M>,
    pub value: Option<(SourceInfo<M>, ExprAst<'a, M>)>,
}

impl<'a, M> Spanned for StructCtorFieldAst<'a, M> {
    fn span(&self) -> Span {
        self.value
            .as_ref()
            .map_or(self.name.span, |(.., e)| self.name.span.joined(e.span()))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct EnumCtorAst<'a, M = NoTokenMeta> {
    pub path: PathAst<'a, M>,
    pub value: Option<(SourceInfo<M>, ExprAst<'a, M>)>,
}

impl<'a, M> Spanned for EnumCtorAst<'a, M> {
    fn span(&self) -> Span {
        self.value
            .as_ref()
            .map_or(self.path.span(), |(_, e)| self.path.span().joined(e.span()))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LetAst<'a, M = NoTokenMeta> {
    pub keyword: SourceInfo<M>,
    pub pat: PatAst<'a, M>,
    pub ty: Option<(SourceInfo<M>, TyAst<'a, M>)>,
    pub equal: SourceInfo<M>,
    pub value: ExprAst<'a, M>,
}

impl<'a, M> Spanned for LetAst<'a, M> {
    fn span(&self) -> Span {
        self.keyword.span.joined(self.value.span())
    }
}

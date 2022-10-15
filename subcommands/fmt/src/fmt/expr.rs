use super::*;

impl<'a> FmtAst for ExprAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        match *self {
            ExprAst::Unit(unit) => unit.display(fmt),
            ExprAst::Binary(binary) => binary.display(fmt),
        }
    }

    fn flat_len(&self, fmt: &Fmt) -> usize {
        match *self {
            ExprAst::Unit(unit) => unit.flat_len(fmt),
            ExprAst::Binary(binary) => binary.flat_len(fmt),
        }
    }
}

impl<'a> FmtAst for BinaryExprAst<'a> {
    fn display_low(&self, fold: bool, fmt: &mut Fmt) {
        self.lhs.display_low(fold, fmt);
        write!(fmt, " ");
        self.op.display(fmt);
        if fold {
            fmt.newline();
            fmt.indent();
            fmt.write_indent();
            fmt.unindent();
        } else {
            write!(fmt, " ");
        }
        self.rhs.display_low(fold, fmt);
    }

    fn flat_len(&self, fmt: &Fmt) -> usize {
        self.lhs.flat_len(fmt)
            + " ".len()
            + self.op.flat_len(fmt)
            + " ".len()
            + self.rhs.flat_len(fmt)
    }
}

impl<'a> FmtAst for UnitExprAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        match *self {
            UnitExprAst::StructCtor(ctor) => ctor.display(fmt),
            UnitExprAst::DotExpr(dot) => dot.display(fmt),
            UnitExprAst::Call(call) => call.display(fmt),
            UnitExprAst::Path(path) => path.display(fmt),
            UnitExprAst::Return(ret) => ret.display(fmt),
            UnitExprAst::Int(span) | UnitExprAst::Char(span) => fmt.write_span(span),
            UnitExprAst::Const(run) => run.display(fmt),
            UnitExprAst::PathInstance(path) => path.display(fmt),
            UnitExprAst::TypedPath(path) => path.display(fmt),
            UnitExprAst::Match(r#match) => r#match.display(fmt),
            UnitExprAst::EnumCtor(ctor) => ctor.display(fmt),
            UnitExprAst::If(r#if) => r#if.display(fmt),
        }
    }

    fn flat_len(&self, fmt: &Fmt) -> usize {
        match *self {
            UnitExprAst::StructCtor(ctor) => ctor.flat_len(fmt),
            UnitExprAst::DotExpr(dot) => dot.flat_len(fmt),
            UnitExprAst::Call(call) => call.flat_len(fmt),
            UnitExprAst::Path(path) => path.flat_len(fmt),
            UnitExprAst::Return(ret) => ret.flat_len(fmt),
            UnitExprAst::Int(int) | UnitExprAst::Char(int) => int.len(),
            UnitExprAst::Const(run) => run.flat_len(fmt),
            UnitExprAst::PathInstance(path) => path.flat_len(fmt),
            UnitExprAst::TypedPath(path) => path.flat_len(fmt),
            UnitExprAst::Match(r#match) => r#match.flat_len(fmt),
            UnitExprAst::EnumCtor(ctor) => ctor.flat_len(fmt),
            UnitExprAst::If(r#if) => r#if.flat_len(fmt),
        }
    }
}

impl<'a> FmtAst for IfAst<'a> {
    fn display_low(&self, fold: bool, fmt: &mut Fmt) {
        fmt.write_span(self.r#if);
        write!(fmt, " ");
        self.cond.display(fmt);
        write!(fmt, " ");
        self.body.display(fmt);
        if !fold {
            if let Some((r#else, else_body)) = self.r#else {
                write!(fmt, " ");
                fmt.write_span(r#else);
                write!(fmt, " ");
                else_body.display(fmt);
            }
            return;
        }
        if let IfBlock::Arrow(..) = self.body && (!self.elifs.is_empty() || self.r#else.is_some()) {
            fmt.newline();
            fmt.write_indent();
        } else {
            write!(fmt, " ");
        }
        if let Some((&last, others)) = self.elifs.split_last() {
            for &ElifAst { elif, cond, body } in others {
                fmt.write_span(elif);
                write!(fmt, " ");
                cond.display(fmt);
                write!(fmt, " ");
                body.display(fmt);
                if let IfBlock::Arrow(..) = body {
                    fmt.newline();
                    fmt.write_indent();
                } else {
                    write!(fmt, " ");
                }
            }
            fmt.write_span(last.elif);
            write!(fmt, " ");
            last.cond.display(fmt);
            write!(fmt, " ");
            last.body.display(fmt);
            if let IfBlock::Arrow(..) = last.body && self.r#else.is_some() {
                fmt.newline();
                fmt.write_indent();
            } else {
                write!(fmt, " ");
            }
        }
        if let Some((r#else, body)) = self.r#else {
            fmt.write_span(r#else);
            write!(fmt, " ");
            body.display(fmt);
        }
    }

    fn flat_len(&self, fmt: &Fmt) -> usize {
        self.r#if.len()
            + " ".len()
            + self.cond.flat_len(fmt)
            + " ".len()
            + self.body.flat_len(fmt)
            + !self.elifs.is_empty() as usize * fmt.line_length
            + self
                .r#else
                .map(|(r#else, body)| " ".len() + r#else.len() + body.flat_len(fmt))
                .unwrap_or(0)
    }
}

impl<'a> FmtAst for IfBlock<'a> {
    fn display_low(&self, fold: bool, fmt: &mut Fmt) {
        match *self {
            IfBlock::Block(block) => block.display_low(true, fmt),
            IfBlock::Arrow(arrow, expr) => {
                fmt.write_span(arrow);
                if fold {
                    fmt.indent();
                    fmt.newline();
                    fmt.write_indent();
                } else {
                    write!(fmt, " ");
                }
                expr.display(fmt);
                if fold {
                    fmt.unindent();
                }
            }
        }
    }

    fn flat_len(&self, fmt: &Fmt) -> usize {
        match *self {
            IfBlock::Block(..) => fmt.line_length,
            IfBlock::Arrow(arrow, expr) => arrow.len() + " ".len() + expr.flat_len(fmt),
        }
    }
}

impl<'a> FmtAst for EnumCtorAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        self.path.display(fmt);
        if let Some((tilde, value)) = self.value {
            fmt.write_span(tilde);
            value.display(fmt);
        }
    }
}

impl<'a> FmtAst for MatchExprAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        write!(fmt, "match ");
        self.expr.display_low(false, fmt);
        write!(fmt, " ");
        self.body.display_low(true, fmt);
    }
}

impl<'a> FmtAst for MatchArmAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        self.pattern.display(fmt);
        write!(fmt, " ");
        fmt.write_span(self.arrow);
        write!(fmt, " ");
        self.body.display_low(false, fmt);
    }
}

impl<'a> FmtAst for PatAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        match *self {
            PatAst::Binding(name) => name.display(fmt),
            PatAst::StructCtor(ctor) => ctor.display(fmt),
            PatAst::Int(span) => fmt.write_span(span),
            PatAst::EnumCtor(ctor) => ctor.display(fmt),
        }
    }
}

impl<'a> FmtAst for EnumCtorPatAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        fmt.write_span(self.slash);
        self.name.display(fmt);
        if let Some((tilde, pat)) = self.value {
            fmt.write_span(tilde);
            pat.display(fmt);
        }
    }
}

impl<'a> FmtAst for StructCtorPatAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        fmt.write_span(self.slash);
        self.fields.display(fmt);
    }
}

impl<'a> FmtAst for StructCtorPatFieldAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        use StructCtorPatFieldAst::*;
        match *self {
            Simple { name } => name.display(fmt),
            Named { name, colon, pat } => {
                name.display(fmt);
                fmt.write_span(colon);
                write!(fmt, " ");
                pat.display(fmt);
            }
            DoubleDot(span) => fmt.write_span(span),
        }
    }
}

impl<'a> FmtAst for TypedPathAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        match self.ty {
            TyAst::Path(path) => path.display(fmt),
            TyAst::Instance(instance) => {
                instance.path.display(fmt);
                fmt.write_span(self.slash);
                instance.params.display(fmt);
            }
            _ => unreachable!(),
        };
        fmt.write_span(self.slash);
        self.path.display(fmt);
    }

    fn flat_len(&self, fmt: &Fmt) -> usize {
        self.ty.flat_len(fmt) + self.slash.len() + self.slash.len() + self.path.flat_len(fmt)
    }
}

impl<'a> FmtAst for PathInstanceAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        self.path.display(fmt);
        if let Some((slash, params)) = self.params {
            fmt.write_span(slash);
            params.display(fmt);
        }
    }

    fn flat_len(&self, fmt: &Fmt) -> usize {
        self.path.flat_len(fmt)
            + self
                .params
                .map_or(0, |(slash, params)| slash.len() + params.flat_len(fmt))
    }
}

impl<'a> FmtAst for StructCtorAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        if let Some(path) = self.path {
            path.display(fmt);
        }
        fmt.write_span(self.slash);
        self.body.display(fmt);
    }
}

impl<'a> FmtAst for DotExprAst<'a> {
    fn display_low(&self, fold: bool, fmt: &mut Fmt) {
        self.lhs.display_low(fold, fmt);
        if fold {
            fmt.newline();
            fmt.indent();
            fmt.write_indent();
            fmt.unindent();
        }
        fmt.write_span(self.dot);
        self.rhs.display(fmt);
    }

    fn flat_len(&self, fmt: &Fmt) -> usize {
        self.lhs.flat_len(fmt) + ".".len() + self.rhs.flat_len(fmt)
    }
}

impl<'a> FmtAst for ConstAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        fmt.write_span(self.r#const);
        write!(fmt, " ");
        self.value.display(fmt);
    }

    fn flat_len(&self, fmt: &Fmt) -> usize {
        self.r#const.len() + " ".len() + self.value.flat_len(fmt)
    }
}

impl<'a> FmtAst for CallExprAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        self.callable.display(fmt);
        self.args.display(fmt);
    }

    fn flat_len(&self, fmt: &Fmt) -> usize {
        self.callable.flat_len(fmt) + self.args.flat_len(fmt)
    }
}

impl<'a> FmtAst for ReturnExprAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        fmt.write_span(self.return_span);
        if let Some(expr) = self.expr {
            write!(fmt, " ");
            expr.display(fmt);
        }
    }

    fn flat_len(&self, fmt: &Fmt) -> usize {
        self.return_span.len()
            + self
                .expr
                .map(|expr| " ".len() + expr.flat_len(fmt))
                .unwrap_or(0)
    }
}

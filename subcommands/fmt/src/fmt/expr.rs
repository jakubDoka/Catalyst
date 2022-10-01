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
            UnitExprAst::Call(call) => call.display(fmt),
            UnitExprAst::Path(path) => path.display(fmt),
            UnitExprAst::Return(ret) => ret.display(fmt),
            UnitExprAst::Int(span) | UnitExprAst::Char(span) => fmt.write_span(span),
            UnitExprAst::Const(run) => run.display(fmt),
        }
    }

    fn flat_len(&self, fmt: &Fmt) -> usize {
        match *self {
            UnitExprAst::Call(call) => call.flat_len(fmt),
            UnitExprAst::Path(path) => path.flat_len(fmt),
            UnitExprAst::Return(ret) => ret.flat_len(fmt),
            UnitExprAst::Int(int) | UnitExprAst::Char(int) => int.len(),
            UnitExprAst::Const(run) => run.flat_len(fmt),
        }
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

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
        use UnitExprAst::*;
        match *self {
            StructCtor(ctor) => ctor.display(fmt),
            DotExpr(dot) => dot.display(fmt),
            Call(call) => call.display(fmt),
            Path(path) => path.display(fmt),
            Return(ret) => ret.display(fmt),
            Int(span) | Char(span) | Bool(span) => fmt.write_span(span),
            Match(r#match) => r#match.display(fmt),
            EnumCtor(ctor) => ctor.display(fmt),
            If(r#if) => r#if.display(fmt),
            Loop(r#loop) => r#loop.display(fmt),
            Break(r#break) => r#break.display(fmt),
            Continue(r#continue) => r#continue.display(fmt),
            Let(r#let) => r#let.display(fmt),
            Deref(prefix, target) => {
                fmt.write_span(prefix);
                target.display(fmt);
            }
            Ref(prefix, mutability, target) => {
                fmt.write_span(prefix);
                mutability.display(fmt);
                target.display(fmt);
            }
            Block(block) => block.display_low(true, fmt),
        }
    }

    fn flat_len(&self, fmt: &Fmt) -> usize {
        use UnitExprAst::*;
        match *self {
            StructCtor(ctor) => ctor.flat_len(fmt),
            DotExpr(dot) => dot.flat_len(fmt),
            Call(call) => call.flat_len(fmt),
            Path(path) => path.flat_len(fmt),
            Return(ret) => ret.flat_len(fmt),
            Int(span) | Char(span) | Bool(span) => span.len(),
            Match(r#match) => r#match.flat_len(fmt),
            EnumCtor(ctor) => ctor.flat_len(fmt),
            If(r#if) => r#if.flat_len(fmt),
            Loop(r#loop) => r#loop.flat_len(fmt),
            Break(r#break) => r#break.flat_len(fmt),
            Continue(r#continue) => r#continue.flat_len(fmt),
            Let(r#let) => r#let.flat_len(fmt),
            Deref(span, target) | Ref(span, .., target) => span.len() + target.flat_len(fmt),
            Block(block) => block.flat_len(fmt),
        }
    }
}

impl<'a> FmtAst for BreakAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        fmt.write_span(self.r#break);
        if let Some(label) = self.label {
            fmt.buffer.push(' ');
            label.display(fmt);
        }
        if let Some(expr) = self.value {
            fmt.buffer.push(' ');
            expr.display(fmt);
        }
    }
}

impl FmtAst for ContinueAst {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        fmt.write_span(self.r#continue);
        if let Some(label) = self.label {
            fmt.buffer.push(' ');
            label.display(fmt);
        }
    }
}

impl<'a> FmtAst for LoopAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        fmt.write_span(self.r#loop);
        if let Some(label) = self.label {
            label.display(fmt);
        }
        fmt.buffer.push(' ');
        self.body.display(fmt);
    }
}

impl<'a> FmtAst for LetAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        fmt.write_span(self.r#let);
        write!(fmt, " ");
        self.pat.display(fmt);
        write!(fmt, " ");
        fmt.write_span(self.equal);
        write!(fmt, " ");
        self.value.display(fmt);
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
        if let IfBlockAst::Arrow(..) = self.body && (!self.elifs.is_empty() || self.r#else.is_some()) {
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
                if let IfBlockAst::Arrow(..) = body {
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
            if let IfBlockAst::Arrow(..) = last.body && self.r#else.is_some() {
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

impl<'a> FmtAst for IfBlockAst<'a> {
    fn display_low(&self, fold: bool, fmt: &mut Fmt) {
        match *self {
            IfBlockAst::Block(block) => block.display_low(true, fmt),
            IfBlockAst::Arrow(arrow, expr) => {
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
            IfBlockAst::Block(..) => fmt.line_length,
            IfBlockAst::Arrow(arrow, expr) => arrow.len() + " ".len() + expr.flat_len(fmt),
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
        self.body.display(fmt);
    }
}

impl<'a> FmtAst for PatAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        match *self {
            PatAst::Binding(mutable, name) => {
                if let Some(mutable) = mutable {
                    fmt.write_span(mutable);
                    write!(fmt, " ");
                }
                name.display(fmt)
            }
            PatAst::StructCtor(ctor) => ctor.display(fmt),
            PatAst::Wildcard(span) | PatAst::Int(span) => fmt.write_span(span),
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
            Simple { mutable, name } => {
                if let Some(mutable) = mutable {
                    fmt.write_span(mutable);
                    write!(fmt, " ");
                }
                name.display(fmt);
            }
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

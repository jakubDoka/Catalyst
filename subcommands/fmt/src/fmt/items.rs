use super::*;

impl<'a> FmtAst for ItemAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        match *self {
            ItemAst::Struct(s) => s.display(fmt),
            ItemAst::Func(func) => func.display(fmt),
            ItemAst::Attribute(attr) => attr.display(fmt),
        }
    }
}

impl FmtAst for TopLevelAttributeAst {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        fmt.write_span(self.hash);
        self.value.display(fmt);
    }
}

impl FmtAst for TopLevelAttributeKindAst {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        match *self {
            TopLevelAttributeKindAst::Entry(span) => fmt.write_span(span),
            TopLevelAttributeKindAst::Inline(mode) => {
                write!(fmt, "inline");
                if let Some(mode) = mode {
                    mode.display(fmt);
                }
            }
        }
    }
}

impl FmtAst for InlineModeAst {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        match *self {
            InlineModeAst::Always(span) | InlineModeAst::Never(span) => fmt.write_span(span),
        }
    }
}

impl<'a> FmtAst for StructAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        fmt.vis(self.vis);
        self.generics.display(fmt);
        if !self.generics.is_empty() {
            write!(fmt, " ");
        }
        write!(fmt, "struct ");
        fmt.write_span(self.name.span);
        if !self.body.elements.is_empty() {
            write!(fmt, " ");
        }
        self.body.display_low(true, fmt);
    }
}

impl<'a> FmtAst for StructFieldAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        fmt.vis(self.vis);
        if self.used {
            write!(fmt, "use ");
        }
        if self.mutable {
            write!(fmt, "mut ");
        }
        fmt.write_span(self.name.span);
        write!(fmt, ": ");
        self.ty.display(fmt);
    }
}

impl<'a> FmtAst for StructConstructorFieldAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        self.name.display(fmt);
        if let Some(expr) = self.expr {
            write!(fmt, ": ");
            expr.display(fmt);
        }
    }
}

use super::*;

impl<'a> FmtAst for ItemAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        match *self {
            ItemAst::Struct(s) => s.display(fmt),
            ItemAst::Func(func) => func.display(fmt),
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

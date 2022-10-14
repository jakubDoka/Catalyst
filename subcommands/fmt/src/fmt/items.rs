use super::*;

impl<'a> FmtAst for ItemAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        match *self {
            ItemAst::Struct(s) => s.display(fmt),
            ItemAst::Func(func) => func.display(fmt),
            ItemAst::Spec(spec) => spec.display(fmt),
            ItemAst::Impl(r#impl) => r#impl.display(fmt),
            ItemAst::Attribute(attr) => attr.display(fmt),
            ItemAst::Enum(r#enum) => r#enum.display(fmt),
        }
    }
}

impl<'a> FmtAst for EnumAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        fmt.vis(self.vis);
        fmt.write_span(self.r#enum);
        write!(fmt, " ");
        self.generics.display(fmt);
        if !self.generics.is_empty() {
            write!(fmt, " ");
        }
        self.name.display(fmt);
        if !self.body.is_empty() {
            write!(fmt, " ");
            self.body.display(fmt);
        }
    }
}

impl<'a> FmtAst for EnumVariantAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        self.name.display(fmt);
        if let Some((colon, ty)) = self.ty {
            fmt.write_span(colon);
            write!(fmt, " ");
            ty.display(fmt);
        }
    }
}

impl<'a> FmtAst for ImplAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        fmt.vis(self.vis);
        fmt.write_span(self.r#impl);
        write!(fmt, " ");
        self.generics.display(fmt);
        if !self.generics.is_empty() {
            write!(fmt, " ");
        }
        self.target.display(fmt);
        if !self.body.is_empty() {
            write!(fmt, " ");
        }
        self.body.display_low(true, fmt);
    }
}

impl<'a> FmtAst for ImplTarget<'a> {
    fn display_low(&self, fold: bool, fmt: &mut Fmt) {
        if fold {
            fmt.newline();
        }
        match *self {
            ImplTarget::Direct(ty) => ty.display(fmt),
            ImplTarget::Spec(ty, r#as, spec) => {
                ty.display(fmt);
                write!(fmt, " ");
                fmt.write_span(r#as);
                write!(fmt, " ");
                spec.display(fmt);
            }
        }
    }

    fn flat_len(&self, fmt: &Fmt) -> usize {
        match *self {
            ImplTarget::Direct(ty) => ty.flat_len(fmt),
            ImplTarget::Spec(ty, r#as, spec) => {
                ty.flat_len(fmt) + " ".len() + r#as.len() + " ".len() + spec.flat_len(fmt)
            }
        }
    }
}

impl<'a> FmtAst for SpecExprAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        match *self {
            SpecExprAst::Path(p) => p.display(fmt),
            SpecExprAst::Instance(i) => i.display(fmt),
        }
    }

    fn flat_len(&self, fmt: &Fmt) -> usize {
        match *self {
            SpecExprAst::Path(p) => p.flat_len(fmt),
            SpecExprAst::Instance(i) => i.flat_len(fmt),
        }
    }
}

impl<'a> FmtAst for ImplItemAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        match *self {
            ImplItemAst::Func(func) => func.display(fmt),
        }
    }
}

impl<'a> FmtAst for SpecAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        fmt.vis(self.vis);
        fmt.write_span(self.spec);
        if !self.generics.is_empty() {
            write!(fmt, " ");
        }
        self.generics.display(fmt);
        write!(fmt, " ");
        self.name.display(fmt);
        if !self.body.is_empty() {
            write!(fmt, " ");
            self.body.display_low(true, fmt);
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
        write!(fmt, "struct ");
        self.generics.display(fmt);
        if !self.generics.is_empty() {
            write!(fmt, " ");
        }
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

impl<'a> FmtAst for StructCtorFieldAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        self.name.display(fmt);
        if let Some(expr) = self.expr {
            write!(fmt, ": ");
            expr.display(fmt);
        }
    }
}

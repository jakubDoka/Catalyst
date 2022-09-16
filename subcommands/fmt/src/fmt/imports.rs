use super::*;

impl<'a> FmtAst for UseAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        fmt.write_span(self.use_span);
        write!(fmt, " ");
        self.items.display_low(true, fmt);
    }
}

impl FmtAst for ImportAst {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        fmt.vis(self.vis);
        if !self.name.span.within(self.path) {
            fmt.write_span(self.name.span);
            write!(fmt, " ");
        }

        fmt.write_wrapped_span(self.path, '"');
    }
}

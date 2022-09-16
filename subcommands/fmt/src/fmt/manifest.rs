use super::*;

impl FmtAst for NameAst {
    fn len(&self, _: &Fmt) -> usize {
        self.span.len()
    }

    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        fmt.buffer.push_str(&fmt.interner[self.ident]);
    }
}

impl<'a> FmtAst for ManifestValueAst<'a> {
    fn len(&self, fmt: &Fmt) -> usize {
        match self {
            ManifestValueAst::String(span) => span.len(),
            ManifestValueAst::Object(list) => list.len(fmt),
            ManifestValueAst::Array(list) => list.len(fmt),
        }
    }

    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        match self {
            ManifestValueAst::String(span) => fmt.buffer.push_str(&fmt.source[span.range()]),
            ManifestValueAst::Object(list) => list.display_low(true, fmt),
            ManifestValueAst::Array(list) => list.display(fmt),
        }
    }
}

impl<'a> FmtAst for ManifestFieldAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        fmt.write_span(self.name.span);
        write!(fmt, ": ");
        self.value.display(fmt);
    }

    fn len(&self, fmt: &Fmt) -> usize {
        self.name.len(fmt) + ": ".len() + self.value.len(fmt)
    }
}

impl<'a> FmtAst for ManifestDepAst {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        if self.git {
            write!(fmt, "git ");
        }

        if !self.name.span.within(self.path) {
            fmt.write_span(self.name.span);
            write!(fmt, " ");
        }

        fmt.write_wrapped_span(self.path, '"');

        if let Some(version) = self.version.expand() {
            write!(fmt, " ");
            fmt.write_wrapped_span(version, '"');
        }
    }
}

impl<'a> FmtAst for ManifestAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        self.fields.iter().for_each(|a| a.display_low(false, fmt));
        fmt.optional_newline();
        if let Some(deps_span) = self.deps_span.expand() {
            fmt.write_span(deps_span);
            self.deps.display_low(true, fmt);
        }
    }
}

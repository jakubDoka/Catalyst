use super::*;

impl FmtAst for NameAst {
    fn flat_len(&self, _: &Fmt) -> usize {
        self.span.len()
    }

    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        fmt.buffer.push_str(&fmt.interner[self.ident]);
    }
}

impl<'a> FmtAst for ManifestValueAst<'a> {
    fn flat_len(&self, fmt: &Fmt) -> usize {
        match self {
            ManifestValueAst::String(span) => span.len(),
            ManifestValueAst::Object(list) => list.flat_len(fmt),
            ManifestValueAst::Array(list) => list.flat_len(fmt),
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

    fn flat_len(&self, fmt: &Fmt) -> usize {
        self.name.flat_len(fmt) + ": ".len() + self.value.flat_len(fmt)
    }
}

impl FmtAst for ManifestDepAst {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        if self.git {
            write!(fmt, "git ");
        }

        if !self.name.span.within(self.path) {
            fmt.write_span(self.name.span);
            write!(fmt, " ");
        }

        fmt.write_wrapped_span(self.path, '"');

        if let Some(version) = self.version {
            write!(fmt, " ");
            fmt.write_wrapped_span(version, '"');
        }
    }
}

impl<'a> FmtAst for ManifestAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        self.fields.iter().for_each(|a| a.display_low(false, fmt));
        fmt.optional_newline();
        if let Some(deps_span) = self.deps_span {
            fmt.write_span(deps_span);
            self.deps.display_low(true, fmt);
        }
    }
}

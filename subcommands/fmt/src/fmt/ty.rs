use super::*;

impl<'a> FmtAst for TyAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        match *self {
            TyAst::Path(ident) => ident.display(fmt),
            TyAst::Instance(instance) => instance.display(fmt),
            TyAst::Pointer(pointer) => pointer.display(fmt),
            TyAst::Tuple(tuple) => tuple.display(fmt),
            TyAst::Wildcard(span) => fmt.write_span(span),
        }
    }

    fn flat_len(&self, fmt: &Fmt) -> usize {
        match *self {
            TyAst::Path(ident) => ident.flat_len(fmt),
            TyAst::Instance(instance) => instance.flat_len(fmt),
            TyAst::Pointer(pointer) => pointer.flat_len(fmt),
            TyAst::Tuple(tuple) => tuple.flat_len(fmt),
            TyAst::Wildcard(span) => span.len(),
        }
    }
}

impl<'a> FmtAst for TyInstanceAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        self.path.display(fmt);
        self.params.display(fmt);
    }

    fn flat_len(&self, fmt: &Fmt) -> usize {
        self.path.flat_len(fmt) + self.params.flat_len(fmt)
    }
}

impl<'a> FmtAst for TyPointerAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        write!(fmt, "^");
        self.mutability.display(fmt);
        self.ty.display(fmt);
    }

    fn flat_len(&self, fmt: &Fmt) -> usize {
        1 + self.mutability.flat_len(fmt) + self.ty.flat_len(fmt)
    }
}

impl<'a> FmtAst for MutabilityAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        match *self {
            MutabilityAst::Mut(..) => write!(fmt, "mut "),
            MutabilityAst::None => (),
            MutabilityAst::Generic(.., ident) => {
                write!(fmt, "use ");
                ident.display(fmt)
            }
        }
    }

    fn flat_len(&self, fmt: &Fmt) -> usize {
        match *self {
            MutabilityAst::Mut(span) => span.len(),
            MutabilityAst::None => 0,
            MutabilityAst::Generic(.., ident) => "use ".len() + ident.flat_len(fmt),
        }
    }
}

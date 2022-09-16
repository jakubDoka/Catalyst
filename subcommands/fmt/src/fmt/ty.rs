use super::*;

impl<'a> FmtAst for TyAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        match self {
            TyAst::Ident(ident) => ident.display(fmt),
            TyAst::Instance(instance) => instance.display(fmt),
            TyAst::Pointer(pointer) => pointer.display(fmt),
        }
    }

    fn len(&self, fmt: &Fmt) -> usize {
        match *self {
            TyAst::Ident(ident) => ident.len(fmt),
            TyAst::Instance(instance) => instance.len(fmt),
            TyAst::Pointer(pointer) => pointer.len(fmt),
        }
    }
}

impl<'a> FmtAst for TyInstanceAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        self.ident.display(fmt);
        self.params.display(fmt);
    }

    fn len(&self, fmt: &Fmt) -> usize {
        self.ident.len(fmt) + self.params.len(fmt)
    }
}

impl<'a> FmtAst for TyPointerAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        write!(fmt, "^");
        self.mutability.display(fmt);
        self.ty.display(fmt);
    }

    fn len(&self, fmt: &Fmt) -> usize {
        1 + self.mutability.len(fmt) + self.ty.len(fmt)
    }
}

impl<'a> FmtAst for MutabilityAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        match *self {
            MutabilityAst::Mut(..) => write!(fmt, "mut "),
            MutabilityAst::None => (),
            MutabilityAst::Ident(.., ident) => {
                write!(fmt, "use ");
                ident.display(fmt)
            }
        }
    }

    fn len(&self, fmt: &Fmt) -> usize {
        match *self {
            MutabilityAst::Mut(span) => span.len(),
            MutabilityAst::None => 0,
            MutabilityAst::Ident(.., ident) => "use ".len() + ident.len(fmt),
        }
    }
}

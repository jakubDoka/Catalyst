use super::*;

impl<'a> FmtAst for FuncDefAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        fmt.vis(self.vis);
        self.signature.display(fmt);
        self.body.display(fmt);
    }
}

impl<'a> FmtAst for FuncSigAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        write!(fmt, "fn ");
        self.generics.display(fmt);
        if !self.generics.is_empty() {
            write!(fmt, " ");
        }
        self.name.display(fmt);
        self.args.display(fmt);
        if let Some(ret) = self.ret {
            write!(fmt, " -> ");
            ret.display(fmt);
        }
    }
}

impl<'a> FmtAst for FuncArgAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        self.name.display(fmt);
        write!(fmt, ": ");
        self.ty.display(fmt);
    }
}

impl<'a> FmtAst for FuncBodyAst<'a> {
    fn display_low(&self, fold: bool, fmt: &mut Fmt) {
        match self {
            FuncBodyAst::Arrow(.., expr) => {
                write!(fmt, " =>");
                fmt.buffer.push([' ', '\n'][fold as usize]);
                expr.display(fmt);
            }
            FuncBodyAst::Block(block) => {
                write!(fmt, " ");
                block.display_low(false, fmt);
            }
            FuncBodyAst::Exported(span) => {
                write!(fmt, " ");
                fmt.write_span(*span);
            }
        }
    }

    fn flat_len(&self, fmt: &Fmt) -> usize {
        match self {
            FuncBodyAst::Block(block) => " ".len() + block.flat_len(fmt),
            FuncBodyAst::Arrow(.., expr) => " => ".len() + expr.flat_len(fmt),
            FuncBodyAst::Exported(span) => " ".len() + span.len(),
        }
    }
}

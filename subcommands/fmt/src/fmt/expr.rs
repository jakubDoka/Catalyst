use super::*;

impl<'a> FmtAst for ExprAst<'a> {
    fn display_low(&self, _: bool, fmt: &mut Fmt) {
        match *self {
            ExprAst::Unit(_) => todo!(),
            ExprAst::Binary(_) => todo!(),
        }
    }

    fn flat_len(&self, fmt: &Fmt) -> usize {
        match *self {
            ExprAst::Unit(_) => todo!(),
            ExprAst::Binary(_) => todo!(),
        }
    }
}

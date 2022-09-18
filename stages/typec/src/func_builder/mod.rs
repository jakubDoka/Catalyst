use storage::{BumpVec, VRef};
use typec_t::Func;

use crate::{item_collector::FuncDefs, *};

pub type TypeCheckedFuncs<'a> = BumpVec<(VRef<Func>, FuncTir<'a>)>;

impl<'a> TyChecker<'_, 'a> {
    pub fn build_funcs(&mut self, funcs: &mut FuncDefs, compiled_funcs: &TypeCheckedFuncs<'a>) {}
}

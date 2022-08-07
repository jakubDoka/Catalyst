use crate::{tir::TirList, *};
use lexing_t::*;
use storage::*;

#[derive(Default)]
pub struct FuncParserCtx {
    pub current_fn: Maybe<Def>,
}

#[derive(Default)]
pub struct DefEnt {
    pub params: Maybe<TyList>,
    pub flags: FuncFlags,
    pub loc: Maybe<Loc>,
    pub body: Maybe<TirList>,
    pub tir_data: TirData,
    pub sig: Sig,
}

bitflags! {
    struct FuncFlags: u8 {
        GENERIC
    }
}

#[derive(Default, Clone, Copy, PartialEq, Eq)]
pub struct Sig {
    pub cc: Maybe<Ident>,
    pub args: Maybe<TyList>,
    pub ret: Maybe<Ty>,
}

gen_v_ptr!(Def DefList);

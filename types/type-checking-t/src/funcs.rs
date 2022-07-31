use crate::{tir::TirList, *};
use lexing_t::*;
use storage::*;

#[derive(Default)]
pub struct FuncParserCtx {
    pub current_fn: Maybe<Def>,
}

#[derive(Default)]
pub struct Funcs {
    pub defs: OrderedMap<DefEnt, Def>,
}

impl Funcs {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn params_of_def(&self, def: Def) -> Maybe<TyList> {
        self.defs[def].params
    }

    pub fn args_of(&self, def: Def) -> Maybe<TyList> {
        self.defs[def].sig.args
    }
}

pub struct DefEnt {
    pub params: Maybe<TyList>,
    pub flags: FuncFlags,
    pub source: Maybe<Ident>,
    pub span: Maybe<Span>,
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

gen_v_ptr!(Def);

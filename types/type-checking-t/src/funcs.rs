use crate::{tir::TirList, *};
use lexing_t::*;
use storage::*;

#[derive(Default)]
pub struct FuncParserCtx {
    pub current_fn: Maybe<Def>,
}

#[derive(Default)]
pub struct Funcs {
    pub ents: SparseMap<Ident, FuncEnt>,
    pub defs: PoolMap<Def, DefEnt>,
    pub slices: PoolBumpMap<FuncList, Ident>,
}

impl Funcs {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn params_of_def(&self, def: Def) -> Maybe<TyList> {
        self.ents[self.defs[def].true_func].params
    }

    pub fn args_of(&self, def: Def) -> Maybe<TyList> {
        self.defs[def].sig.args
    }
}

#[derive(Clone, Copy)]
pub struct FuncEnt {
    pub params: Maybe<TyList>,
    pub flags: FuncFlags,
    pub def: Def,
}

bitflags! {
    struct FuncFlags: u8 {
        GENERIC
    }
}

pub struct DefEnt {
    pub true_func: Ident,
    pub source: Maybe<Ident>,
    pub span: Maybe<Span>,
    pub body: Maybe<TirList>,
    pub tir_data: TirData,
    pub sig: Sig,
}

#[derive(Default, Clone, Copy, PartialEq, Eq)]
pub struct Sig {
    pub cc: Maybe<Ident>,
    pub args: Maybe<TyList>,
    pub ret: Maybe<Ty>,
}

gen_v_ptr!(Def FuncList Func);

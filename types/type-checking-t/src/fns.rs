use crate::{tir::TirList, *};
use lexing_t::*;
use storage::*;

#[derive(Default)]
pub struct FnParserCtx {
    pub current_fn: Maybe<Def>,
}

#[derive(Default)]
pub struct Fns {
    pub ents: SparseMap<Ident, FnEnt>,
    pub defs: PoolMap<Def, DefEnt>,
    pub slices: PoolBumpMap<FnList, Ident>,
}

impl Fns {
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
pub struct FnEnt {
    pub params: Maybe<TyList>,
    pub flags: FnFlags,
    pub def: Def,
}

bitflags! {
    struct FnFlags: u8 {
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

#[derive(Default, Clone, Copy)]
pub struct Sig {
    pub cc: Maybe<Ident>,
    pub args: Maybe<TyList>,
    pub ret: Maybe<Ty>,
}

gen_v_ptr!(Def FnList);

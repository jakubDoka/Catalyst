use crate::*;
use lexing_t::*;
use storage::*;

#[derive(Default)]
pub struct Funcs {
    pub ents: SparseMap<Ident, FuncEnt>,
    pub defs: PoolMap<Func, DefEnt>,
    pub slices: PoolBumpMap<FuncList, Ident>,
}

impl Funcs {
    pub fn new() -> Self {
        Self::default()
    }
}

#[derive(Default, Clone, Copy)]
pub struct FuncEnt {
    pub params: Maybe<TyList>,
    pub flags: FuncFlags,
}

bitflags! {
    #[derive(Default)]
    pub struct FuncFlags: u8 {

    }
}

#[derive(Default)]
pub struct DefEnt {
    pub source: Maybe<Ident>,
    pub span: Maybe<Span>,
    pub body: Maybe<Tir>,
    pub tir_data: TirData,
    pub sig: Sig,
}

#[derive(Clone, Copy)]
pub struct Sig {
    pub cc: Maybe<Ident>,
    pub data: Maybe<TyList>,
    pub ret: Ty,
}

impl Default for Sig {
    fn default() -> Self {
        Self {
            cc: Maybe::none(),
            data: Maybe::none(),
            ret: BuiltinTypes::NOTHING,
        }
    }
}

gen_v_ptr!(Func FuncList);

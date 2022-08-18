use crate::*;
use lexing_t::*;
use scope::Vis;
use storage::*;

#[derive(Default)]
pub struct FuncParserCtx {
    pub current_fn: Maybe<Def>,
}

#[derive(Default)]
pub struct DefEnt {
    pub params: Maybe<TyList>,
    pub flags: FuncFlags,
    pub loc: Loc,
    pub body: Maybe<TirList>,
    pub tir_data: TirData,
    pub sig: Sig,
}

pub struct FuncEnt {
    pub def: Def,
    pub params: Maybe<TyList>,
}

bitflags! {
    struct FuncFlags: u8 {
        GENERIC
        PUBLIC
        PRIVATE
    }
}

impl From<Vis> for FuncFlags {
    fn from(vis: Vis) -> Self {
        match vis {
            Vis::Pub => FuncFlags::PUBLIC,
            Vis::None => FuncFlags::empty(),
            Vis::Priv => FuncFlags::PRIVATE,
        }
    }
}

impl From<FuncFlags> for Vis {
    fn from(flags: FuncFlags) -> Self {
        if flags.contains(FuncFlags::PUBLIC) {
            Vis::Pub
        } else if flags.contains(FuncFlags::PRIVATE) {
            Vis::Priv
        } else {
            Vis::None
        }
    }
}

#[derive(Default, Clone, Copy, PartialEq, Eq, Debug)]
pub struct Sig {
    pub cc: Maybe<Ident>,
    pub args: Maybe<TyList>,
    pub ret: Maybe<Ty>,
}

gen_v_ptr!(
    Def DefList
    Func FuncList
);

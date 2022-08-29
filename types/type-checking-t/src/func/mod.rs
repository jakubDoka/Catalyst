use crate::*;
use lexing_t::*;
use scope::Vis;
use storage::*;

pub mod tir;

#[derive(Default)]
pub struct FuncParserCtx {
    pub current_fn: Maybe<VRef<Def>>,
}

#[derive(Default)]
pub struct Def {
    pub generics: VSlice<VRef<Bound>>,
    pub upper_generics: VSlice<VRef<Bound>>,
    pub flags: DefFlags,
    pub loc: Loc,
    pub body: VSlice<Tir>,
    pub tir_data: TirData,
    pub sig: Sig,
}

pub struct Func {
    pub def: VRef<Def>,
    pub params: VSlice<VRef<Ty>>,
}

bitflags! {
    struct DefFlags: u8 {
        GENERIC
        PUBLIC
        PRIVATE
    }
}

impl From<Vis> for DefFlags {
    fn from(vis: Vis) -> Self {
        match vis {
            Vis::Pub => DefFlags::PUBLIC,
            Vis::None => DefFlags::empty(),
            Vis::Priv => DefFlags::PRIVATE,
        }
    }
}

impl From<DefFlags> for Vis {
    fn from(flags: DefFlags) -> Self {
        if flags.contains(DefFlags::PUBLIC) {
            Vis::Pub
        } else if flags.contains(DefFlags::PRIVATE) {
            Vis::Priv
        } else {
            Vis::None
        }
    }
}

#[derive(Default, Clone, Copy, PartialEq, Eq, Debug)]
pub struct Sig {
    pub cc: Maybe<Ident>,
    pub args: VSlice<VRef<Ty>>,
    pub ret: Maybe<VRef<Ty>>,
}

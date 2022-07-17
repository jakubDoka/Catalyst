use crate::*;
use lexing_t::*;
use storage::*;

pub type TirData = CacheBumpMap<TirList, TirEnt, Tir>;

pub struct TirEnt {
    pub meta: TirMeta,
    pub children: Maybe<TirList>,
    pub ty: Maybe<Ty>,
    pub span: Maybe<Span>,
}

pub struct TirMeta {
    pub kind: TirKind,
    pub flags: TirFlags,
}

pub enum TirKind {}

bitflags! {
    pub struct TirFlags: u8 {

    }
}

gen_v_ptr!(Tir TirList);

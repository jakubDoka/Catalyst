use crate::*;
use lexing_t::*;
use storage::*;

pub type TirData = BumpMap<TirList, TirEnt, Tir>;

#[derive(Default, Clone, Copy)]
pub struct TirEnt {
    pub kind: TirKind,
    pub flags: TirFlags,
    pub ty: Maybe<Ty>,
    pub span: Maybe<Span>,
}

impl TirEnt {
    pub fn terminating(&self) -> bool {
        self.flags.contains(TirFlags::TERMINATING)
    }

    #[inline]
    pub fn new(kind: TirKind) -> Self {
        Self {
            kind,
            ..Default::default()
        }
    }

    #[inline]
    pub fn ty(self, ty: impl Into<Maybe<Ty>>) -> Self {
        Self {
            ty: ty.into(),
            ..self
        }
    }

    #[inline]
    pub fn span(self, span: impl Into<Maybe<Span>>) -> Self {
        Self {
            span: span.into(),
            ..self
        }
    }

    #[inline]
    pub fn flags(self, flags: TirFlags) -> Self {
        Self { flags, ..self }
    }
}

#[derive(Default, Clone, Copy)]
pub struct TirMeta {}

#[derive(Default, Clone, Copy)]
pub enum TirKind {
    Block {
        stmts: Maybe<TirList>,
    },
    Return {
        value: Maybe<Tir>,
    },
    Access {
        var: Tir,
    },
    String,
    Int,
    Argument(u8),
    Unreachable,
    #[default]
    Invalid,
}

bitflags! {
    struct TirFlags: u8 {
        TERMINATING
    }
}

gen_v_ptr!(Tir TirList);

use std::default::default;

use crate::*;
use lexing_t::*;
use storage::*;

pub mod ty_factory;
pub mod typec;

#[derive(Default, Clone, Copy, Debug)]
pub struct Ty {
    pub kind: TyKind,
    pub flags: TyFlags,
    pub loc: Loc,
}

gen_kind! {
    TyKind {
        Param = u32,
        Struct = TyStruct {
            generics: VSlice<VRef<Bound>>,
            fields: VSlice<Field>,
        },
        Enum = TyEnum {
            generics: VSlice<VRef<Bound>>,
            variants: VSlice<EnumVariant>,
        },
        Instance = TyInstance {
            base: VRef<Ty>,
            params: VSlice<VRef<Ty>>,
        },
        Ptr = TyPtr {
            base: VRef<Ty>,
            depth: u32,
        },
        FuncPtr = Sig,
        Int = TyInt {
            width: u8,
            signed: bool,
        },
        Bool,
        SelfBound,
        Inferrable,
    }
}

impl TyKind {
    pub fn inferrable(&self) -> bool {
        matches!(self, TyKind::Inferrable)
    }

    pub fn default_param() -> TyKind {
        TyKind::Param(default())
    }
}

impl Default for TyKind {
    fn default() -> Self {
        TyKind::Inferrable
    }
}

bitflags! {
    struct TyFlags: u8 {
        GENERIC
        MUTABLE
        BUILTIN
        ANON_BOUND
    }
}

#[derive(Clone, Copy)]
pub struct EnumVariant {
    pub ty: VRef<Ty>,
    pub span: Maybe<Span>,
}

pub struct Field {
    pub mutable: bool,
    pub exported: bool,
    pub ty: VRef<Ty>,
    pub name: Maybe<Span>,
}

impl Ty {
    gen_increasing_constants! {
        INFERRED
        SELF_BOUND
        STR STACK_TRACE
        BOOL
        CHAR
        INT I8 I16 I32 I64
        UINT U8 U16 U32 U64
    }

    gen_constant_groups! {
        NUMBERS = [I8 I16 I32 I64 U8 U16 U32 U64];
        INTEGERS = [I8 I16 I32 I64];
        UNSIGNED_INTEGERS = [U8 U16 U32 U64];
    }
}

impl VRefDefault for Ty {
    fn default_state() -> VRef<Self> {
        Ty::INFERRED
    }
}

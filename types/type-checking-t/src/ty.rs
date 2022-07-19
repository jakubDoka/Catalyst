use crate::*;
use lexing_t::*;
use storage::*;

pub struct Types {
    pub ents: OrderedMap<Ident, TyEnt, Ty>,
    pub slices: CachedPoolBumpMap<TyList, Ty>,
    pub comps: CachedPoolBumpMap<TyCompList, TyCompEnt, TyCompList>,
}

#[derive(Default, Clone, Copy)]
pub struct TyEnt {
    pub kind: TyKind,
    pub flags: TyFlags,
    pub file: Maybe<Ident>,
    pub span: Maybe<Span>,
}

#[derive(Clone, Copy)]
pub enum TyKind {
    Param {
        index: u32,
        bounds: TyList,
        next_clone: Maybe<Ty>,
    },
    Bound {
        funcs: FuncList,
    },
    Struct {
        fields: TyCompList,
    },
    Enum {
        tag: Ty,
        variants: TyCompList,
    },
    Instance {
        base: Ty,
        params: TyList,
    },
    Ptr {
        base: Ty,
        depth: u32,
    },
    FuncPtr {
        sig: Sig,
    },
    Int {
        width: u32,
        signed: bool,
    },
    Bool,
    Inferrable,
}

impl TyKind {
    pub fn inferrable(&self) -> bool {
        matches!(self, TyKind::Inferrable)
    }
}

impl Default for TyKind {
    fn default() -> Self {
        TyKind::Inferrable
    }
}

bitflags! {
    struct TyFlags: u8 {
        PUBLIC PRIVATE
        GENERIC
    }
}

pub struct TyCompEnt {
    pub ty: Ty,
    pub index: u32,
    pub span: Maybe<Span>,
}

pub struct BuiltinTypes;

macro_rules! gen_builtin_type_groups {
    ($($name:ident = [$($elem:ident)*];)*) => {
        $(
            pub const $name: &'static [Ty] = &[$(Self::$elem),*];
        )*
    };
}

macro_rules! gen_builtin_types {
    ($($ident:ident)*) => {
        gen_builtin_types!((0) $($ident)*);
        gen_builtin_type_groups!(ALL = [$($ident)*];);
    };

    (($prev:expr) $current:ident $($next:ident $($others:ident)*)?) => {
        pub const $current: Ty = Ty($prev);
        $(
            gen_builtin_types!((Self::$current.0 + 1) $next $($others)*);
        )?
    };
}

impl BuiltinTypes {
    gen_builtin_types! {
        NOTHING
        INFERRED
        DROP COPY
        STR STACK_TRACE
        TY_ANY ANY
        BOOL
        CHAR
        INT I8 I16 I32 I64
        UINT U8 U16 U32 U64
    }

    gen_builtin_type_groups! {
        NUMBERS = [I8 I16 I32 I64 U8 U16 U32 U64];
        INTEGERS = [I8 I16 I32 I64];
        UNSIGNED_INTEGERS = [U8 U16 U32 U64];
    }
}

gen_v_ptr!(Ty TyList TyComp TyCompList);

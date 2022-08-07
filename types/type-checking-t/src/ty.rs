use std::default::default;

use crate::*;
use diags::*;
use lexing_t::*;
use storage::*;

#[derive(Default)]
pub struct Typec {
    pub types: OrderedMap<TyEnt, Ty>,
    pub ty_lists: PoolBumpMap<TyList, Ty, Unused>,
    pub variants: CachedPoolBumpMap<EnumVariantList, EnumVariantEnt, EnumVariant>,
    pub fields: CachedPoolBumpMap<FieldList, FieldEnt, Field>,
    pub bound_funcs: CachedPoolBumpMap<BoundFuncList, BoundFuncEnt, BoundFunc>,
    pub impls: PartialOrderedMap<ImplEnt, Impl>,
    pub defs: OrderedMap<DefEnt, Def>,
    pub def_lists: PoolBumpMap<DefList, Def, Unused>,
    pub funcs: OrderedMap<FuncEnt, Func>,
}

gen_v_ptr!(Unused);

impl Typec {
    #[inline]
    pub fn ptr_depth(&self, ty: Ty) -> u32 {
        match self.types[ty].kind {
            TyKind::Ptr { depth, .. } => depth,
            _ => 0,
        }
    }

    #[inline]
    pub fn param_count(&self, ty: Ty) -> usize {
        self.types[ty].param_count as usize
    }

    #[inline]
    pub fn instance_base_of(&self, ty: Ty) -> Ty {
        if let TyKind::Instance { base, .. } = self.types[ty].kind {
            base
        } else {
            ty
        }
    }

    #[inline]
    pub fn pointer_leaf_of(&self, ty: Ty) -> Ty {
        if let TyKind::Ptr { base, .. } = self.types[ty].kind {
            self.pointer_leaf_of(base)
        } else {
            ty
        }
    }

    #[inline]
    pub fn absolute_base_of(&self, ty: Ty) -> Ty {
        self.pointer_leaf_of(self.instance_base_of(ty))
    }

    #[inline]
    pub fn has_param_base(&self, ty: Ty) -> bool {
        matches!(
            self.types[self.absolute_base_of(ty)].kind,
            TyKind::Param { .. }
        )
    }

    #[inline]
    pub fn assoc_ty_index(&self, ty: Ty) -> Option<usize> {
        if let TyKind::AssocType { index } = self.types[ty].kind {
            Some(index as usize)
        } else {
            None
        }
    }

    #[inline]
    pub fn assoc_ty_count_of_bound(&self, ty: Ty) -> usize {
        if let TyKind::Bound { assoc_types, .. } = self.types[ty].kind {
            self.ty_lists[assoc_types].len()
        } else {
            0
        }
    }

    #[inline]
    pub fn func_count_of_bound(&self, ty: Ty) -> usize {
        if let TyKind::Bound { funcs, .. } = self.types[ty].kind {
            self.bound_funcs[funcs].len()
        } else {
            0
        }
    }

    #[inline]
    pub fn params_of_def(&self, def: Def) -> Maybe<TyList> {
        self.defs[def].params
    }

    #[inline]
    pub fn args_of(&self, def: Def) -> Maybe<TyList> {
        self.defs[def].sig.args
    }

    #[inline]
    pub fn is_valid_bound(&self, ty: Ty) -> bool {
        matches!(
            self.types[self.instance_base_of(ty)].kind,
            TyKind::Bound { .. }
        )
    }

    #[inline]
    pub fn func_count_of_impl(&self, r#impl: Impl) -> usize {
        self.func_count_of_bound(self.instance_base_of(self.impls[r#impl].bound))
    }

    #[inline]
    pub fn func_of_bound(&self, ty: Ty, index: usize) -> BoundFuncEnt {
        let TyKind::Bound { funcs, .. } = self.types[ty].kind else {
            unreachable!();
        };

        self.bound_funcs[funcs][index]
    }

    #[inline]
    pub fn func_of_impl(&self, r#impl: Impl, index: usize) -> BoundFuncEnt {
        self.func_of_bound(self.instance_base_of(self.impls[r#impl].bound), index)
    }

    #[inline]
    pub fn func_index_of_impl(&self, r#impl: Impl, name: Ident) -> Option<usize> {
        let bound = self.impls[r#impl].bound;
        let TyKind::Bound { funcs, .. } = self.types[self.instance_base_of(bound)].kind else {
            unreachable!();
        };
        self.bound_funcs[funcs]
            .iter()
            .position(|func| func.name == name)
    }

    #[inline]
    pub fn loc_of(&self, ty: Ty, interner: &Interner) -> Maybe<DiagLoc> {
        let loc = self.types[ty].loc.expand(interner);
        loc.file
            .map(|file| DiagLoc {
                span: loc.span.into(),
                source: file,
            })
            .into()
    }

    #[inline]
    pub fn instance_params(&self, bound: Ty) -> Maybe<TyList> {
        if let TyKind::Instance { params, .. } = self.types[bound].kind {
            params.into()
        } else {
            Maybe::none()
        }
    }
}

#[derive(Default, Clone, Copy)]
pub struct TyEnt {
    pub kind: TyKind,
    pub flags: TyFlags,
    pub param_count: u8,
    pub loc: Loc,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TyKind {
    Param {
        index: u32,
        bound: Ty,
    },
    AssocType {
        index: u32,
    },
    Bound {
        inherits: Maybe<TyList>,
        assoc_types: Maybe<TyList>,
        funcs: Maybe<BoundFuncList>,
    },
    Struct {
        fields: Maybe<FieldList>,
    },
    Enum {
        tag: Ty,
        variants: Maybe<EnumVariantList>,
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

    pub fn default_bound() -> TyKind {
        TyKind::Bound {
            inherits: default(),
            assoc_types: default(),
            funcs: default(),
        }
    }

    pub fn default_param() -> TyKind {
        TyKind::Param {
            index: 0,
            bound: BuiltinTypes::ANY,
        }
    }
}

impl Default for TyKind {
    fn default() -> Self {
        TyKind::Inferrable
    }
}

#[derive(Clone, Copy, Default)]
pub struct BoundFuncEnt {
    pub sig: Sig,
    pub name: Ident,
    pub params: Maybe<TyList>,
    pub span: Maybe<Span>,
}

pub struct ImplEnt {
    pub id: Ident,
    pub params: Maybe<TyList>,
    pub bound: Ty,
    pub implementor: Ty,
    pub funcs: Maybe<DefList>,
    pub loc: Loc,
    pub next: Maybe<Impl>,
}

bitflags! {
    struct TyFlags: u8 {
        GENERIC
        MUTABLE
        BUILTIN
        TY_PARAM
    }
}

#[derive(Clone, Copy)]
pub struct EnumVariantEnt {
    pub ty: Ty,
    pub span: Maybe<Span>,
}

pub struct FieldEnt {
    pub mutable: bool,
    pub exported: bool,
    pub ty: Ty,
    pub name: Maybe<Span>,
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

gen_v_ptr!(
    EnumVariant EnumVariantList
    Ty TyList
    Field FieldList
    BoundFunc BoundFuncList
    Impl
);

impl Ty {
    pub const fn id(&self) -> u32 {
        self.0
    }
}

use std::ops::{IndexMut, Range};

use lexer_types::*;
use storage::*;

use crate::*;

pub type TyLists = FramedStackMap<TyList, Ty>;
pub type Types = PrimaryMap<Ty, TyEnt>;
pub type TFuncLists = StackMap<FuncList, Func>;
pub type TyCompLookup = Map<TyComp>;
pub type TyComps = StackMap<TyCompList, TyCompEnt, TyComp>;
pub type BoundImpls = Map<BoundImpl>;
pub type Instances = Map<Ty>;

impl TypeBase for Types {}

pub trait TypeBase: IndexMut<Ty, Output = TyEnt> {
    fn pattern_info(&self, ty: Ty, ty_lists: &TyLists, ty_comps: &TyComps) -> Option<(usize, Range<i128>)> {
        Some(match self[ty].kind {
            TyKind::Int(-1) => (0, i128::MIN..i128::MAX),
            TyKind::Int(base) => (0, -1 << base..1 << base),
            TyKind::Bound(_)
            | TyKind::Param(..) => return None,
            TyKind::Struct(fields) => (ty_comps.len(fields), 0..0),
            TyKind::Enum(.., variants) => (ty_comps.len(variants), 0..ty_comps.len(variants) as i128),
            TyKind::Instance(base, ..) => return self.pattern_info(base, ty_lists, ty_comps),
            TyKind::Ptr(..) => (1, 0..0),
            TyKind::Bool => (0, 0..1),
            TyKind::Unresolved => unreachable!(),           
        })
    }
    
    fn caller_id_of(&self, ty: Ty) -> ID {
        self[self.caller_of(ty)].id
    }

    fn caller_of(&self, mut ty: Ty) -> Ty {
        loop {
            match self[ty].kind {
                TyKind::Ptr(base, ..) => ty = base,
                TyKind::Instance(base, ..) => return base,
                _ => return ty,
            }
        }
    }

    fn base_id_of(&self, ty: Ty) -> ID {
        self[self.base_of(ty)].id
    }

    fn base_of(&self, mut ty: Ty) -> Ty {
        while let TyKind::Ptr(base, _) = self[ty].kind {
            ty = base;
        }

        ty
    }

    fn depth_of(&self, ty: Ty) -> usize {
        match self[ty].kind {
            TyKind::Ptr(_, depth) => depth as usize,
            _ => 0,
        }
    }

    fn deref(&self, ty: Ty) -> Ty {
        match self[ty].kind {
            TyKind::Ptr(base, _) => base,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Copy, Default, Debug)]
pub struct BoundImpl {
    pub span: Span,
    pub funcs: FuncList,
}

impl BoundImpl {
    pub fn new(span: Span) -> Self {
        BoundImpl {
            span,
            funcs: Default::default(),
        }
    }
}

impl ReservedValue for BoundImpl {
    fn reserved_value() -> Self {
        BoundImpl {
            span: Span::default(),
            funcs: FuncList::default(),
        }
    }

    fn is_reserved_value(&self) -> bool {
        self.span.is_reserved_value() && self.funcs.is_reserved_value()
    }
}

macro_rules! gen_builtin_table {
    ($($name:ident: $repr:expr,)*) => {
        #[derive(Default)]
        pub struct BuiltinTypes {
            $(
                pub $name: Ty,
            )*
            pub discriminant: Span,
        }

        impl BuiltinTypes {
            pub fn all(&self) -> [Ty; 10] {
                [$(self.$name),*]
            }
        }

        impl BuiltinTypes {
            pub fn new(
                sources: &mut Sources,
                builtin_source: &mut BuiltinSource,
                types: &mut Types,
            ) -> Self {
                $(
                    let ent = TyEnt {
                        id: ID::new(stringify!($name)),
                        name: builtin_source.make_span(sources, stringify!($name)),
                        kind: $repr,
                        flags: (TyFlags::GENERIC & matches!($repr, TyKind::Param(..)))
                            | TyFlags::BUILTIN,
                    };
                    let $name = types.push(ent);
                )*

                Self {
                    $(
                        $name,
                    )*
                    discriminant: builtin_source.make_span(sources, "discriminant"),
                }
            }
        }
    };
}

impl BuiltinTypes {
    pub fn signed_integers(&self) -> [Ty; 5] {
        [self.i8, self.i16, self.i32, self.i64, self.int]
    }

    pub fn integers(&self) -> [Ty; 5] {
        [self.i8, self.i16, self.i32, self.i64, self.int]
    }
}

gen_builtin_table!(
    nothing: TyKind::Struct(TyCompList::default()),
    ty_any: TyKind::Param(0, TyList::default(), None.into()),
    any: TyKind::Param(0, TyList::default(), None.into()),
    bool: TyKind::Bool,
    char: TyKind::Int(32),
    int: TyKind::Int(-1),
    i8: TyKind::Int(8),
    i16: TyKind::Int(16),
    i32: TyKind::Int(32),
    i64: TyKind::Int(64),
);

#[derive(Debug, Clone, Copy)]
pub struct TyCompEnt {
    pub ty: Ty,
    pub index: u32,
    pub span: Span,
}

impl ReservedValue for TyCompEnt {
    fn reserved_value() -> Self {
        TyCompEnt {
            ty: Ty::reserved_value(),
            span: Span::reserved_value(),
            index: u32::MAX,
        }
    }

    fn is_reserved_value(&self) -> bool {
        self.ty.is_reserved_value() && self.index == u32::MAX
    }
}

#[derive(Default, Clone, Copy, Debug)]
pub struct TyEnt {
    pub id: ID,
    pub name: Span,
    pub kind: TyKind,
    pub flags: TyFlags,
}

bitflags! {
    #[derive(Default)]
    pub struct TyFlags: u32 {
        const GENERIC = 1 << 0;
        const BUILTIN = 1 << 1;
    }
}

impl TyFlags {
    pub const MAX_PARAMS: u32 = 32;
    pub const PARAMS_WIDTH: u32 = 5;
    pub const FLAGS_WIDTH: u32 = 32;
    pub const PARAM_SHIFT: u32 = Self::FLAGS_WIDTH - Self::PARAMS_WIDTH;
    pub const CLEAR_PARAMS_MASK: u32 = (1 << Self::PARAM_SHIFT) - 1;

    pub fn add_param_count(mut self, count: usize) -> Self {
        self.bits &= Self::CLEAR_PARAMS_MASK;
        self.bits |= (count as u32) << Self::PARAM_SHIFT;
        self
    }

    pub fn param_count(&self) -> usize {
        (self.bits >> Self::PARAM_SHIFT) as usize
    }
}

impl_bool_bit_and!(TyFlags);

pub struct TyDisplay<'a> {
    pub types: &'a Types,
    pub ty_lists: &'a TyLists,
    pub sources: &'a Sources,
    pub ty: Ty,
}

impl<'a> TyDisplay<'a> {
    #[inline(never)]
    pub fn new(types: &'a Types, ty_lists: &'a TyLists, sources: &'a Sources, ty: Ty) -> Self {
        TyDisplay {
            types,
            ty_lists,
            sources,
            ty,
        }
    }
}

impl std::fmt::Display for TyDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut str = String::new();
        self.ty
            .display(self.types, self.ty_lists, self.sources, &mut str)?;
        write!(f, "{str}")
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TyKind {
    Param(u8, TyList, PackedOption<Ty>),
    Bound(FuncList),
    Struct(TyCompList),
    Enum(Ty, TyCompList),
    /// (base, params)
    Instance(Ty, TyList),
    /// (inner, depth)
    Ptr(Ty, u32),
    Int(i16),
    Bool,
    Unresolved,
}

impl Default for TyKind {
    fn default() -> Self {
        TyKind::Unresolved
    }
}

gen_entity!(Ty);
gen_entity!(TyList);
gen_entity!(TyComp);
gen_entity!(TyCompList);

impl Ty {
    pub fn display(
        self,
        types: &Types,
        ty_lists: &TyLists,
        sources: &Sources,
        to: &mut String,
    ) -> std::fmt::Result {
        use std::fmt::Write;
        match types[self].kind {
            TyKind::Struct(..)
            | TyKind::Bound(..)
            | TyKind::Int(..)
            | TyKind::Enum(..)
            | TyKind::Bool => {
                let name = types[self].name;
                write!(to, "{}", sources.display(name))?;
            }
            TyKind::Instance(base, params) => {
                let base = types[base].name;
                write!(to, "{}", sources.display(base))?;
                write!(to, "[")?;
                for (i, param) in ty_lists.get(params).iter().enumerate() {
                    if i != 0 {
                        write!(to, ", ")?;
                    }
                    param.display(types, ty_lists, sources, to)?;
                }
                write!(to, "]")?;
            }
            TyKind::Param(_, list, ..) => {
                if ty_lists.get(list).is_empty() {
                    write!(to, "any")?;
                }

                for (i, ty) in ty_lists.get(list).iter().enumerate() {
                    if i != 0 {
                        write!(to, " + ")?;
                    }
                    ty.display(types, ty_lists, sources, to)?;
                }
            }
            TyKind::Ptr(ty, ..) => {
                write!(to, "*")?;
                ty.display(types, ty_lists, sources, to)?;
            }
            TyKind::Unresolved => write!(to, "unresolved")?,
        }

        Ok(())
    }
}

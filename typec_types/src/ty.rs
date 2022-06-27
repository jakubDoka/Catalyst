use std::ops::IndexMut;

use lexer::*;
use matching::PatternRange;
use storage::*;

use crate::*;

pub type TyGraph = Graph<Ty>;
pub type TyLists = FramedStackMap<TyList, Ty>;
pub type Types = PrimaryMap<Ty, TyEnt>;
pub type FuncLists = StackMap<FuncList, Func>;
pub type TyComps = StackMap<TyCompList, TyCompEnt, TyComp>;
pub type BoundImpls = Map<BoundImpl>;
pub type TyInstances = Map<Ty>;

impl TypeBase for Types {}

pub trait TypeBase: IndexMut<Ty, Output = TyEnt> {
    fn may_drop(&self, ty: Ty) -> bool {
        (
            !self[ty].flags.contains(TyFlags::BUILTIN)
            && !matches!(self[ty].kind, TyKind::Ptr(..) | TyKind::FuncPtr(..))
        ) || matches!(self[ty].kind, TyKind::Param(..))
    }

    fn item_count(&self, ty: Ty, ty_comps: &TyComps) -> usize {
        match self[ty].kind {
            TyKind::Struct(fields) => ty_comps.len_of(fields),
            TyKind::Instance(base, ..) => self.item_count(base, ty_comps),
            TyKind::Ptr(_, _)
            | TyKind::FuncPtr(_)
            | TyKind::Int(_)
            | TyKind::Uint(_)
            | TyKind::Bool => 1,
            TyKind::Enum(_, _) => todo!(),
            kind => unimplemented!("{kind:?}"),
        }
    }

    fn item_ty(&self, ty: Ty, index: usize, ty_comps: &TyComps) -> Ty {
        assert!(index < self.item_count(ty, ty_comps));

        match self[ty].kind {
            TyKind::Struct(fields) => ty_comps.get(fields)[index].ty,
            TyKind::Instance(base, _) => self.item_ty(base, index, ty_comps),
            TyKind::Ptr(_, _)
            | TyKind::FuncPtr(_)
            | TyKind::Int(_)
            | TyKind::Uint(_)
            | TyKind::Bool => unreachable!(),
            TyKind::Enum(_, _) => todo!(),
            kind => unimplemented!("{kind:?}"),
        }
    }

    fn range_of(&self, ty: Ty, ty_comps: &TyComps) -> PatternRange {
        match self[ty].kind {
            TyKind::Instance(..) | TyKind::Struct(..) => PatternRange::default(),

            // enum has 1 extra field and since a..b is inclusive in this context
            // we subtract another 1.
            TyKind::Enum(.., variants) => PatternRange::new(0..ty_comps.len_of(variants) - 2),

            TyKind::Ptr(..) | TyKind::FuncPtr(..) => PatternRange::new(0..usize::MAX),
            TyKind::Int(base) => match base {
                8 => PatternRange::new(i8::MIN..i8::MAX),
                16 => PatternRange::new(i16::MIN..i16::MAX),
                32 => PatternRange::new(i32::MIN..i32::MAX),
                64 => PatternRange::new(i64::MIN..i64::MAX),
                _ => PatternRange::new(isize::MIN..isize::MAX),
            },
            TyKind::Uint(base) => match base {
                8 => PatternRange::new(u8::MIN..u8::MAX),
                16 => PatternRange::new(u16::MIN..u16::MAX),
                32 => PatternRange::new(u32::MIN..u32::MAX),
                64 => PatternRange::new(u64::MIN..u64::MAX),
                _ => PatternRange::new(usize::MIN..usize::MAX),
            },
            TyKind::Bool => PatternRange::new(false..true),

            TyKind::Unresolved | TyKind::Bound(..) | TyKind::Param(..) => unreachable!(),
        }
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

    fn ptr_leaf_id_of(&self, ty: Ty) -> ID {
        self[self.ptr_leaf_of(ty)].id
    }

    fn ptr_leaf_of(&self, mut ty: Ty) -> Ty {
        while let TyKind::Ptr(base, ..) = self[ty].kind {
            ty = base;
        }

        ty
    }

    fn base_of(&self, ty: Ty) -> Ty {
        match self[ty].kind {
            TyKind::Instance(base, ..) => base,
            _ => ty,
        }
    }

    fn depth_of(&self, ty: Ty) -> usize {
        match self[ty].kind {
            TyKind::Ptr(.., depth) => depth as usize,
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

#[derive(Clone, Copy, Default, Debug, PartialEq, Eq)]
pub struct BoundImpl {
    pub span: Span,
    pub ty: Ty,
    pub params: TyList,
    pub funcs: FuncList,
}

impl BoundImpl {
    pub fn new(span: Span, ty: Ty, params: TyList) -> Self {
        BoundImpl {
            span,
            ty,
            params,
            funcs: Default::default(),
        }
    }
}

impl ReservedValue for BoundImpl {
    fn reserved_value() -> Self {
        Default::default()
    }

    fn is_reserved_value(&self) -> bool {
        *self == Default::default()
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
            pub fn all(&self) -> [Ty; 18] {
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
                    let repr = $repr;
                    let ent = TyEnt {
                        id: ID::new(stringify!($name)),
                        name: builtin_source.make_span(sources, stringify!($name)),
                        kind: repr,
                        flags: (TyFlags::GENERIC & matches!(repr, TyKind::Param(..) | TyKind::Bound(..)))
                            | TyFlags::BUILTIN
                            | (TyFlags::SIGNED & matches!(repr, TyKind::Int(..)))
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

    pub fn integers(&self) -> [Ty; 10] {
        [
            self.i8, self.i16, self.i32, self.i64, self.int, self.u8, self.u16, self.u32, self.u64,
            self.uint,
        ]
    }

    pub fn numbers(&self) -> [Ty; 10] {
        [
            self.i8, self.i16, self.i32, self.i64, self.int, self.u8, self.u16, self.u32, self.u64,
            self.uint,
        ]
    }
}

gen_builtin_table!(
    nothing: TyKind::Struct(TyCompList::default()),
    drop: TyKind::Unresolved,
    copy: TyKind::Bound(Default::default()),
    str: TyKind::Unresolved,
    ty_any: TyKind::Param(TyList::default(), None.into()),
    any: TyKind::Param(TyList::default(), None.into()),
    bool: TyKind::Bool,
    char: TyKind::Int(32),
    int: TyKind::Int(-1),
    i8: TyKind::Int(8),
    i16: TyKind::Int(16),
    i32: TyKind::Int(32),
    i64: TyKind::Int(64),
    uint: TyKind::Uint(-1),
    u8: TyKind::Uint(8),
    u16: TyKind::Uint(16),
    u32: TyKind::Uint(32),
    u64: TyKind::Uint(64),
);

#[derive(Debug, Clone, Copy, Default)]
pub struct TyCompEnt {
    pub ty: Ty,
    pub index: u32,
    pub name: Span,
}

impl ReservedValue for TyCompEnt {
    fn reserved_value() -> Self {
        TyCompEnt {
            ty: Ty::reserved_value(),
            name: Span::reserved_value(),
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
        const SIGNED = 1 << 2;
        const MUTABLE = 1 << 3;
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TyKind {
    Param(TyList, PackedOption<Ty>),
    Bound(FuncList),
    Struct(TyCompList),
    Enum(Ty, TyCompList),
    /// (base, params)
    Instance(Ty, TyList),
    /// (inner, depth)
    Ptr(Ty, u32),
    FuncPtr(Sig),
    Int(i16),
    Uint(i16),
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

impl TyDisplay<'_> {
    pub fn write(&self, to: &mut String) -> std::fmt::Result {
        use std::fmt::Write;
        match self.types[self.ty].kind {
            TyKind::Struct(..)
            | TyKind::Bound(..)
            | TyKind::Int(..)
            | TyKind::Uint(..)
            | TyKind::Enum(..)
            | TyKind::Bool => {
                let name = self.types[self.ty].name;
                write!(to, "{}", self.sources.display(name))?;
            }
            TyKind::Instance(base, params) => {
                let base = self.types[base].name;
                write!(to, "{}", self.sources.display(base))?;
                write!(to, "[")?;
                for (i, &param) in self.ty_lists.get(params).iter().enumerate() {
                    if i != 0 {
                        write!(to, ", ")?;
                    }
                    ty_display!(self, param).write(to)?;
                }
                write!(to, "]")?;
            }
            TyKind::Param(list, ..) => {
                write!(to, "impl ")?;
                if self.ty_lists.get(list).is_empty() {
                    write!(to, "any")?;
                }

                for (i, &ty) in self.ty_lists.get(list).iter().enumerate() {
                    if i != 0 {
                        write!(to, " + ")?;
                    }
                    ty_display!(self, ty).write(to)?;
                }
            }
            TyKind::Ptr(ty, ..) => {
                write!(to, "^")?;
                if self.types[self.ty].flags.contains(TyFlags::MUTABLE) {
                    write!(to, "mut ")?;
                }
                ty_display!(self, ty).write(to)?;
            }
            TyKind::FuncPtr(sig) => {
                sig_display!(self, sig).write(to)?;
            }
            TyKind::Unresolved => write!(to, "unresolved")?,
        }

        Ok(())
    }
}

impl std::fmt::Display for TyDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut to = String::new();
        self.write(&mut to)?;
        write!(f, "{to}")
    }
}

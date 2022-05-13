use std::ops::IndexMut;

use lexer_types::*;
use storage::*;

use crate::*;

pub type TyLists = FramedStackMap<TyList, Ty>;
pub type Types = PrimaryMap<Ty, TyEnt>;
pub type TFuncLists = StackMap<FuncList, Func>;
pub type SFieldLookup = Map<SFieldRef>;
pub type SFields = StackMap<SFieldList, SFieldEnt, SField>;
pub type BoundImpls = Map<BoundImpl>;
pub type Instances = Map<Ty>;
pub type FuncInstances = Map<Func>;

impl TypeBase for Types {}

pub trait TypeBase: IndexMut<Ty, Output = TyEnt> {
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
    ($($name:ident: ($repr:expr, $size:expr),)*) => {
        #[derive(Default)]
        pub struct BuiltinTypes {
            $(
                pub $name: Ty,
            )*
        }

        impl BuiltinTypes {
            pub fn all(&self) -> [Ty; 10] {
                [$(self.$name),*]
            }
        }

        impl BuiltinTypes {
            pub fn new(
                graph: &mut GenericGraph,
                sources: &mut Sources,
                builtin_source: &mut BuiltinSource,
                types: &mut Types,
            ) -> Self {
                $(
                    let ent = TyEnt {
                        id: ID::new(stringify!($name)),
                        name: builtin_source.make_span(sources, stringify!($name)),
                        kind: $repr,
                        flags: (TyFlags::GENERIC & matches!($repr, TyKind::Param(..))),
                    };
                    let $name = types.push(ent);
                    graph.close_node();
                )*

                Self {
                    $(
                        $name,
                    )*
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
    nothing: (TyKind::Nothing, Offset::ZERO),
    ty_any: (TyKind::Param(0, TyList::default(), None.into()), Offset::ZERO),
    any: (TyKind::Param(0, TyList::default(), None.into()), Offset::ZERO),
    bool: (TyKind::Bool, Offset::new(1, 1)),
    char: (TyKind::Int(32), Offset::new(4, 4)),
    int: (TyKind::Int(-1), Offset::PTR),
    i8: (TyKind::Int(8), Offset::new(1, 1)),
    i16: (TyKind::Int(16), Offset::new(2, 2)),
    i32: (TyKind::Int(32), Offset::new(4, 4)),
    i64: (TyKind::Int(64), Offset::new(8, 8)),
);

#[derive(Clone, Copy, Default)]
pub struct SFieldRef {
    pub field: SField,
    pub ambiguous: bool,
    pub next: PackedOption<ID>,
}

impl SFieldRef {
    pub fn new(field: SField) -> Self {
        SFieldRef {
            field,
            ambiguous: false,
            next: None.into(),
        }
    }
}

impl ReservedValue for SFieldRef {
    fn reserved_value() -> Self {
        SFieldRef {
            field: ReservedValue::reserved_value(),
            ambiguous: false,
            next: None.into(),
        }
    }

    fn is_reserved_value(&self) -> bool {
        self.field.is_reserved_value()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SFieldEnt {
    pub ty: Ty,
    pub index: u32,
    pub span: Span,
}

impl ReservedValue for SFieldEnt {
    fn reserved_value() -> Self {
        SFieldEnt {
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
    Struct(SFieldList),
    /// (base, params)
    Instance(Ty, TyList),
    /// (inner, depth)
    Ptr(Ty, u32),
    Int(i16),
    Bool,
    Nothing,
    Unresolved,
}

impl Default for TyKind {
    fn default() -> Self {
        TyKind::Unresolved
    }
}

gen_entity!(Ty);
gen_entity!(TyList);
gen_entity!(SField);
gen_entity!(SFieldList);

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
            | TyKind::Nothing
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

use module_types::tree::GenericGraph;
use storage::*;
use lexer_types::*;

use crate::*;

pub struct Types {
    pub ents: PrimaryMap<Ty, TyEnt>,
    pub funcs: StackMap<FuncList, Func>,
    pub args: StackMap<TyList, Ty>,
    pub sfield_lookup: Map<SFieldRef>,
    pub sfields: StackMap<SFieldList, SFieldEnt, SField>,
    pub builtin: BuiltinTable,
    pub bound_cons: Map<BoundImpl>,
    pub instances: Map<Ty>,
    params: Vec<Ty>,
}

impl Types {
    pub const MAX_PARAMS: usize = 32;

    pub fn new(
        graph: &mut GenericGraph,
        sources: &mut Sources,
        builtin_source: &mut BuiltinSource,
    ) -> Self {
        let mut tys = Types {
            funcs: StackMap::new(),
            ents: PrimaryMap::new(),
            args: StackMap::new(),
            sfield_lookup: Map::new(),
            sfields: StackMap::new(),
            builtin: BuiltinTable::new(),
            bound_cons: Map::new(),
            params: Vec::new(),
            instances: Map::new(),
        };

        tys.init(graph, sources, builtin_source);

        tys
    }

    pub fn deref_ptr(&self, ty: Ty) -> Ty {
        match self.ents[ty].kind {
            TyKind::Ptr(ty, ..) => ty,
            _ => unreachable!(),
        }
    }

    pub fn ptr_depth_of(&self, ty: Ty) -> u32 {
        match self.ents[ty].kind {
            TyKind::Ptr(.., depth) => depth,
            _ => 0,
        }
    }

    pub fn base_id_of(&self, ty: Ty, params: TyList) -> ID {
        self.ents[self.base_of(ty, params)].id
    }

    pub fn base_of(&self, ty: Ty, params: TyList) -> Ty {
        self.base_of_low(ty, params).0
    }

    pub fn ensure_no_param(&self, ty: Ty, params: TyList) -> Ty {
        match self.ents[ty].kind {
            TyKind::Param(index) => self.args.get(params)[index as usize],
            _ => ty,
        }
    }

    pub fn base_of_low(&self, mut ty: Ty, params: TyList) -> (Ty, ID) {
        let mut id = ID(0);
        loop {
            let TyEnt { kind, .. } = self.ents[ty];
            match kind {
                TyKind::Ptr(inner, ..) => {
                    id = id + ID::new("*");
                    ty = inner;
                }
                TyKind::Param(index) => {
                    return (self.args.get(params)[index as usize], id);
                }
                TyKind::BoundCombo(..)
                | TyKind::Instance(..)
                | TyKind::Bound(..)
                | TyKind::Struct(..)
                | TyKind::Int(..)
                | TyKind::Bool
                | TyKind::Nothing
                | TyKind::Unresolved => return (ty, id),
            }
        }
    }

    fn init(
        &mut self,
        graph: &mut GenericGraph,
        sources: &mut Sources,
        builtin_source: &mut BuiltinSource,
    ) {
        self.init_builtin_table(graph, sources, builtin_source);

        self.params.reserve(Self::MAX_PARAMS);
        for i in 0..Self::MAX_PARAMS {
            let ty = {
                let ent = TyEnt {
                    id: ID::new("<param>") + ID(i as u64),
                    kind: TyKind::Param((i % (Self::MAX_PARAMS / 2)) as u32),
                    name: builtin_source.make_span(sources, "param"),
                    flags: TyFlags::GENERIC,
                    ..Default::default()
                };
                self.ents.push(ent)
            };
            self.params.push(ty);
            graph.close_node();
        }
    }

    pub fn active_params(&self, popper: &InstanceMarker) -> &[Ty] {
        &popper
            .is_ty
            .then_some(&self.params[Self::MAX_PARAMS / 2..])
            .unwrap_or(self.params.as_slice())[..popper.len]
    }

    pub fn push_params(&mut self, params: TyList, ty_params: bool) -> InstanceMarker {
        let params = self.args.get(params);
        assert!(params.len() <= Self::MAX_PARAMS / 2);
        let used = ty_params
            .then_some(&self.params[Self::MAX_PARAMS / 2..])
            .unwrap_or(self.params.as_slice());
        for (&ty, &param) in params.iter().zip(used) {
            self.ents[param] = self.ents[ty];
        }

        return InstanceMarker {
            len: params.len(),
            is_ty: ty_params,
        };
    }

    pub fn pop_params(&mut self, popper: InstanceMarker) {
        let params = popper
            .is_ty
            .then_some(&self.params[Self::MAX_PARAMS / 2..])
            .unwrap_or(self.params.as_slice());
        for (i, &param) in params[..popper.len].iter().enumerate() {
            self.ents[param] = self.ents[self.params[self.params.len() - 1]];
            self.ents[param].kind = TyKind::Param(i as u32);
        }
    }

    pub fn fn_params(&self) -> &[Ty] {
        &self.params[..Self::MAX_PARAMS / 2]
    }

    pub fn ty_params(&self) -> &[Ty] {
        &self.params[Self::MAX_PARAMS / 2..]
    }

    pub fn ptr_base_of(&self, mut ty: Ty) -> Ty {
        loop {
            match self.ents[ty].kind {
                TyKind::Ptr(base, ..) => ty = base,
                _ => return ty,
            }
        }
    }
}

#[derive(Clone, Copy, Default)]
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
        pub struct BuiltinTable {
            $(
                pub $name: Ty,
            )*
        }

        impl BuiltinTable {
            pub fn all(&self) -> [Ty; 9] {
                [$(self.$name),*]
            }
        }

        impl Types {
            pub fn init_builtin_table(&mut self, graph: &mut GenericGraph, sources: &mut Sources, builtin_source: &mut BuiltinSource) {
                $(
                    let ent = TyEnt {
                        id: ID::new(stringify!($name)),
                        name: builtin_source.make_span(sources, stringify!($name)),
                        kind: $repr,
                        flags: TyFlags::GENERIC & matches!($repr, TyKind::Bound(..)),
                    };
                    let ty = self.ents.push(ent);
                    graph.close_node();
                    self.builtin.$name = ty;
                )*
            }
        }
    };
}

impl BuiltinTable {
    pub fn signed_integers(&self) -> [Ty; 5] {
        [self.i8, self.i16, self.i32, self.i64, self.int]
    }

    pub fn integers(&self) -> [Ty; 5] {
        [self.i8, self.i16, self.i32, self.i64, self.int]
    }
}

gen_builtin_table!(
    nothing: TyKind::Nothing,
    any: TyKind::Bound(FuncList::default()),
    bool: TyKind::Bool,
    char: TyKind::Int(32),
    int: TyKind::Int(-1),
    i8: TyKind::Int(8),
    i16: TyKind::Int(16),
    i32: TyKind::Int(32),
    i64: TyKind::Int(64),
);

impl BuiltinTable {
    pub fn new() -> Self {
        BuiltinTable::default()
    }
}

#[derive(Debug)]
pub struct InstanceMarker {
    is_ty: bool,
    len: usize,
}

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

    pub fn set_param_count(&mut self, count: usize) {
        self.bits &= Self::CLEAR_PARAMS_MASK;
        self.bits |= (count as u32) << Self::PARAM_SHIFT;
    }

    pub fn param_count(&self) -> usize {
        (self.bits >> Self::PARAM_SHIFT) as usize
    }
}

impl_bool_bit_and!(TyFlags);

pub struct TyDisplay<'a> {
    types: &'a Types,
    sources: &'a Sources,
    ty: Ty,
}

impl<'a> TyDisplay<'a> {
    pub fn new(types: &'a Types, sources: &'a Sources, ty: Ty) -> Self {
        Self { types, ty, sources }
    }
}

impl std::fmt::Display for TyDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut str = String::new();
        self.ty.display(self.types, self.sources, &mut str)?;
        write!(f, "{str}")
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TyKind {
    BoundCombo(TyList),
    Bound(FuncList),
    Struct(SFieldList),
    /// (base, params)
    Instance(Ty, TyList),
    /// (inner, depth)
    Ptr(Ty, u32),
    Int(i16),
    /// (index)
    Param(u32),
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
    pub fn display(self, types: &Types, sources: &Sources, to: &mut String) -> std::fmt::Result {
        use std::fmt::Write;
        match types.ents[self].kind {
            TyKind::Struct(..) | TyKind::Bound(..) | TyKind::Int(..) | TyKind::Nothing | TyKind::Bool => {
                let name = types.ents[self].name;
                write!(to, "{}", sources.display(name))?;
            }
            TyKind::Instance(base, params) => {
                let base = types.ents[base].name;
                write!(to, "{}", sources.display(base))?;
                write!(to, "[")?;
                for (i, param) in types.args.get(params).iter().enumerate() {
                    if i != 0 {
                        write!(to, ", ")?;
                    }
                    param.display(types, sources, to)?;
                }
                write!(to, "]")?;
            }
            TyKind::Param(index) => {
                write!(to, "param{}", index)?;
            }
            TyKind::BoundCombo(list) => {
                for (i, ty) in types.args.get(list).iter().enumerate() {
                    if i != 0 {
                        write!(to, " + ")?;
                    }
                    ty.display(types, sources, to)?;
                }
            }
            TyKind::Ptr(ty, ..) => {
                write!(to, "*")?;
                ty.display(types, sources, to)?;
            }
            TyKind::Unresolved => write!(to, "unresolved")?,
        }

        Ok(())
    }
}

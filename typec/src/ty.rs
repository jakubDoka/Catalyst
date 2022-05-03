use crate::{Error, *};
use cranelift_entity::{
    packed_option::{PackedOption, ReservedValue},
    PrimaryMap,
};
use errors::Diagnostics;
use lexer::*;
use modules::*;
use parser::*;

pub struct Builder<'a> {
    pub scope: &'a mut Scope,
    pub types: &'a mut Types,
    pub sources: &'a Sources,
    pub ast: &'a ast::Data,
    pub ctx: &'a mut Context,
    pub graph: &'a mut GenericGraph,
    pub modules: &'a mut Modules,
    pub ty: Ty,
    pub diagnostics: &'a mut errors::Diagnostics,
}

impl<'a> Builder<'a> {
    pub fn build(&mut self) -> errors::Result {
        let Ent { id, .. } = self.types.ents[self.ty];
        let ast = self.ctx.type_ast[self.ty];
        if ast.is_reserved_value() {
            return Ok(());
        }
        let ast::Ent { kind, span, .. } = self.ast.nodes[ast];

        match kind {
            ast::Kind::Struct => self.build_struct(id, ast)?,
            ast::Kind::Bound => self.build_bound(id, ast)?,
            _ => todo!(
                "Unhandled type decl {:?}: {}",
                kind,
                self.sources.display(span)
            ),
        }

        Ok(())
    }

    pub fn build_bound(&mut self, _id: ID, _ast: Ast) -> errors::Result {
        Ok(())
    }

    pub fn build_struct(&mut self, id: ID, ast: Ast) -> errors::Result {
        let &[.., body] = self.ast.children(ast) else {
            unreachable!();
        };

        // fields are inserted into centralized hash map for faster lookup
        // and memory efficiency, though we still need field ordering when
        // calculating offsets
        let fields = {
            for (i, &field_ast) in self.ast.children(body).iter().enumerate() {
                let &[name, field_ty_ast] = self.ast.children(field_ast) else {
                    unreachable!();
                };
                let field_ty = self.parse_type(field_ty_ast)?;

                let span = self.ast.nodes[name].span;

                let id = {
                    let name = self.sources.id(span);
                    Self::field_id(id, name)
                };

                let field = {
                    let field = SFieldEnt {
                        span,
                        ty: field_ty,
                        index: i as u32,
                    };
                    self.types.sfields.push_one(field)
                };

                assert!(self
                    .types
                    .sfield_lookup
                    .insert(id, SFieldRef::new(field))
                    .map(|f| f.next.is_some())
                    .unwrap_or(true));
                self.graph.add_edge(field_ty.as_u32());
            }
            self.types.sfields.close_frame()
        };

        self.types.ents[self.ty].kind = Kind::Struct(fields);
        self.graph.close_node();

        Ok(())
    }

    pub fn field_id(ty: ID, name: ID) -> ID {
        ty + name
    }
}

impl TypeParser for Builder<'_> {
    fn state(
        &mut self,
    ) -> (
        &mut Scope,
        &mut Types,
        &Sources,
        &mut Modules,
        &ast::Data,
        &mut errors::Diagnostics,
    ) {
        (
            self.scope,
            self.types,
            self.sources,
            self.modules,
            self.ast,
            self.diagnostics,
        )
    }
}

impl AstIDExt for Builder<'_> {
    fn state(&self) -> (&Data, &Sources) {
        (self.ast, self.sources)
    }
}

pub struct Types {
    pub ents: PrimaryMap<Ty, Ent>,
    pub funcs: StackMap<FuncList, Func>,
    pub args: StackMap<TyList, Ty>,
    pub sfield_lookup: Map<SFieldRef>,
    pub sfields: StackMap<SFieldList, SFieldEnt, SField>,
    pub builtin: BuiltinTable,
    pub bound_cons: Map<BoundImpl>,
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
        };

        tys.init(graph, sources, builtin_source);

        tys
    }

    pub fn deref_ptr(&self, ty: Ty) -> Ty {
        match self.ents[ty].kind {
            Kind::Ptr(ty, ..) => ty,
            _ => unreachable!(),
        }
    }

    pub fn ptr_depth_of(&self, ty: Ty) -> u32 {
        match self.ents[ty].kind {
            Kind::Ptr(.., depth) => depth,
            _ => 0,
        }
    }

    pub fn implements(
        &self,
        input: Ty,
        bound: Ty,
        diagnostics: &mut Diagnostics,
        span: Span,
    ) -> errors::Result {
        // bound can be pain Bound or BoundCombo
        let a = &[bound];
        let bounds = match self.ents[bound].kind {
            Kind::Bound(..) => a,
            Kind::BoundCombo(combo) => self.args.get(combo),
            _ => unreachable!("{:?}", self.ents[bound].kind),
        };

        let input_id = self.ents[input].id;

        let mut result = Ok(());

        for &bound in bounds {
            let bound_id = self.ents[bound].id;
            let id = Collector::bound_impl_id(bound_id, input_id);
            if self.bound_cons.get(id).is_none() {
                diagnostics.push(Error::MissingBound {
                    input,
                    bound,
                    loc: span,
                });
                result = Err(());
            }
        }

        result
    }

    pub fn base_id_of(&self, ty: Ty, params: TyList) -> ID {
        self.ents[self.base_of(ty, params)].id
    }

    pub fn base_of(&self, ty: Ty, params: TyList) -> Ty {
        self.base_of_low(ty, params).0
    }

    pub fn base_of_low(&self, mut ty: Ty, params: TyList) -> (Ty, ID) {
        let mut id = ID(0);
        loop {
            match self.ents[ty].kind {
                Kind::Ptr(inner, ..) => {
                    id = id + ID::new("*");
                    ty = inner;
                }
                Kind::Param(index) => {
                    return (self.args.get(params)[index as usize], id);
                }
                Kind::BoundCombo(..)
                | Kind::Instance(..)
                | Kind::Bound(..)
                | Kind::Struct(..)
                | Kind::Int(..)
                | Kind::Bool
                | Kind::Nothing
                | Kind::Unresolved => return (ty, id),
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
                let ent = Ent {
                    id: ID::new("<param>") + ID(i as u64),
                    kind: Kind::Param(i as u32),
                    name: builtin_source.make_span(sources, "param"),
                    flags: ty::Flags::GENERIC,
                    ..Default::default()
                };
                self.ents.push(ent)
            };
            self.params.push(ty);
        }
    }

    pub fn active_params(&self, popper: &InstanceMarker) -> &[Ty] {
        &self.params[..popper.len]
    }

    pub fn push_params(&mut self, params: TyList) -> InstanceMarker {
        let params = self.args.get(params);
        for (&ty, &param) in params.iter().zip(&self.params) {
            self.ents[param] = self.ents[ty];
        }

        return InstanceMarker { len: params.len() };
    }

    pub fn parameters(&self) -> &[Ty] {
        &self.params
    }

    pub fn param_id(index: usize, ty: ID) -> ID {
        ID::new("<param>") + ID(index as u64) + ty
    }

    pub fn ptr_base_of(&self, mut ty: Ty) -> Ty {
        loop {
            match self.ents[ty].kind {
                Kind::Ptr(base, ..) => ty = base,
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
                    let ent = Ent {
                        id: ID::new(stringify!($name)),
                        name: builtin_source.make_span(sources, stringify!($name)),
                        kind: $repr,
                        flags: Flags::GENERIC & matches!($repr, Kind::Bound(..)),
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
    nothing: ty::Kind::Nothing,
    any: ty::Kind::Bound(FuncList::default()),
    bool: ty::Kind::Bool,
    char: ty::Kind::Int(32),
    int: ty::Kind::Int(-1),
    i8: ty::Kind::Int(8),
    i16: ty::Kind::Int(16),
    i32: ty::Kind::Int(32),
    i64: ty::Kind::Int(64),
);

impl BuiltinTable {
    pub fn new() -> Self {
        BuiltinTable::default()
    }
}

pub struct InstanceMarker {
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
pub struct Ent {
    pub id: ID,
    pub name: Span,
    pub kind: Kind,
    pub flags: Flags,
}

bitflags::bitflags! {
    #[derive(Default)]
    pub struct Flags: u32 {
        const GENERIC = 1 << 0;
    }
}

impl Flags {
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

crate::impl_bool_bit_and!(Flags);

pub struct Display<'a> {
    types: &'a Types,
    sources: &'a Sources,
    ty: Ty,
}

impl<'a> Display<'a> {
    pub fn new(types: &'a Types, sources: &'a Sources, ty: Ty) -> Self {
        Self { types, ty, sources }
    }
}

impl std::fmt::Display for Display<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut str = String::new();
        self.ty.display(self.types, self.sources, &mut str)?;
        write!(f, "{str}")
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Kind {
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

impl Default for Kind {
    fn default() -> Self {
        Kind::Unresolved
    }
}

lexer::gen_entity!(Ty);
lexer::gen_entity!(TyList);
lexer::gen_entity!(SField);
lexer::gen_entity!(SFieldList);

impl Ty {
    pub fn display(self, types: &Types, sources: &Sources, to: &mut String) -> std::fmt::Result {
        use std::fmt::Write;
        match types.ents[self].kind {
            Kind::Struct(..) | Kind::Bound(..) | Kind::Int(..) | Kind::Nothing | Kind::Bool => {
                let name = types.ents[self].name;
                write!(to, "{}", sources.display(name))?;
            }
            Kind::Instance(base, params) => {
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
            Kind::Param(index) => {
                write!(to, "param{}", index)?;
            }
            Kind::BoundCombo(list) => {
                for (i, ty) in types.args.get(list).iter().enumerate() {
                    if i != 0 {
                        write!(to, " + ")?;
                    }
                    ty.display(types, sources, to)?;
                }
            }
            Kind::Ptr(ty, ..) => {
                write!(to, "*")?;
                ty.display(types, sources, to)?;
            }
            Kind::Unresolved => write!(to, "unresolved")?,
        }

        Ok(())
    }
}

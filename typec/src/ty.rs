use crate::*;
use cranelift_entity::{
    packed_option::{ReservedValue, PackedOption},
    PrimaryMap, SecondaryMap,
};
use lexer::*;
use modules::*;
use parser::*;

pub struct Builder<'a> {
    pub scope: &'a mut Scope,
    pub types: &'a mut Types,
    pub sources: &'a Sources,
    pub ast: &'a ast::Data,
    pub type_ast: &'a SecondaryMap<Ty, Ast>,
    pub graph: &'a mut GenericGraph,
    pub modules: &'a mut Modules,
    pub ty: Ty,
    pub diagnostics: &'a mut errors::Diagnostics,
}

impl<'a> Builder<'a> {
    pub fn build(&mut self) -> errors::Result {
        let Ent { id, .. } = self.types.ents[self.ty];
        let ast = self.type_ast[self.ty];
        if ast.is_reserved_value() {
            return Ok(());
        }
        let ast::Ent { kind, span, .. } = self.ast.nodes[ast];

        match kind {
            ast::Kind::Struct => self.build_struct(id, ast)?,
            _ => todo!(
                "Unhandled type decl {:?}: {}",
                kind,
                self.sources.display(span)
            ),
        }

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
                    let str = self.sources.display(span);
                    Self::field_id(id, ID::new(str))
                };

                let field = {
                    let field = SFieldEnt {
                        span,
                        ty: field_ty,
                        index: i as u32,
                    };
                    self.types.sfields.push_one(field)
                };

                assert!(self.types.sfield_lookup.insert(id, SFieldRef::new(field))
                    .map(|f| f.next.is_some()).unwrap_or(true));
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
    fn state(&mut self) -> (&mut Scope, &mut Types, &Sources, &mut Modules, &ast::Data, &mut errors::Diagnostics) {
        (self.scope, self.types, self.sources, self.modules, self.ast, self.diagnostics)
    }
}

pub struct Types {
    pub ents: PrimaryMap<Ty, Ent>,
    pub funcs: StackMap<FuncList, Func>,
    pub args: StackMap<TyList, Ty>,
    pub sfield_lookup: Map<SFieldRef>,
    pub sfields: StackMap<SFieldList, SFieldEnt, SField>,
    pub builtin: BuiltinTable,
    params: Vec<Ty>,
}

impl Types {
    pub fn new(graph: &mut GenericGraph, sources: &mut Sources, builtin_source: &mut BuiltinSource) -> Self {
        let mut tys = Types {
            funcs: StackMap::new(),
            ents: PrimaryMap::new(),
            args: StackMap::new(),
            sfield_lookup: Map::new(),
            sfields: StackMap::new(),
            builtin: BuiltinTable::new(),
            params: Vec::new(),
        };

        tys.init(graph, sources, builtin_source);

        tys
    }

    pub fn base_of(&self, ty: Ty) -> Ty {
        match self.ents[ty].kind {
            ty::Kind::Pointer(base) => base,
            _ => ty,
        }
    }

    fn init(&mut self, graph: &mut GenericGraph, sources: &mut Sources, builtin_source: &mut BuiltinSource) {
        self.init_builtin_table(graph, sources, builtin_source);
    }

    pub fn active_params(&self, popper: &InstancePopper) -> &[Ty] {
        &self.params[..popper.len]
    }

    pub fn push_params(&mut self, params: TyList) -> InstancePopper {
        let params = self.args.get(params);
        for (&ty, &param) in params.iter().zip(&self.params) {
            self.ents[param] = self.ents[ty];
        }

        return InstancePopper {
            len: params.len(),
        };
    }

    pub fn pop_params(&mut self, popper: InstancePopper) {
        for (i, &param) in self.params[0..popper.len].iter().enumerate() {
            self.ents[param] = Ent {
                kind: Kind::Param(i as u32),
                ..Default::default()
            };
        }
    }

    pub fn get_parameter(&mut self, i: usize, name: Span) -> Ty {
        for i in self.params.len()..=i {
            let ty = {
                let ent = Ent {
                    kind: Kind::Param(i as u32),
                    ..Default::default()
                };
                self.ents.push(ent)
            };
            self.params.push(ty);
        }
        let ty = self.params[i];
        self.ents[ty].name = name;
        ty
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
            pub fn all(&self) -> [Ty; 8] {
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

pub struct InstancePopper {
    len: usize,
}

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
}

impl Ent {
    pub fn new(kind: Kind, id: ID) -> Self {
        Self {
            id,
            kind,
            name: Default::default(),
        }
    }
}

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
    Bound(FuncList),
    Struct(SFieldList),
    Pointer(Ty),
    Int(i16),
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
            Kind::Struct(..) 
            | Kind::Bound(..) 
            | Kind::Param(..)
            | Kind::Int(..)
            | Kind::Nothing
            | Kind::Bool => {
                let name = types.ents[self].name;
                write!(to, "{}", sources.display(name))?;
            }
            Kind::Pointer(ty) => {
                write!(to, "*")?;
                ty.display(types, sources, to)?;
            }
            Kind::Unresolved => write!(to, "unresolved")?,
        }

        Ok(())
    }
}

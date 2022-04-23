use crate::{TypeParser, Func};
use cranelift_entity::{
    packed_option::{ReservedValue, PackedOption},
    EntityList, ListPool, PrimaryMap, SecondaryMap,
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
    pub ty: Ty,
    pub diagnostics: &'a mut errors::Diagnostics,
}

impl<'a> Builder<'a> {
    pub fn build(&mut self) -> errors::Result {
        let Ent { id, .. } = self.types.ents[self.ty];
        let ast = self.type_ast[self.ty];
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

                assert!(self.types.sfield_lookup.insert(id, SFieldRef::new(field)).is_none());
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
    fn state(&mut self) -> (&mut Scope, &mut Types, &Sources, &ast::Data, &mut errors::Diagnostics) {
        (self.scope, self.types, self.sources, self.ast, self.diagnostics)
    }
}

pub struct Types {
    pub ents: PrimaryMap<Ty, Ent>,
    pub funcs: ListPool<Func>,
    pub cons: StackMap<TyList, Ty>,
    pub sfield_lookup: Map<SFieldRef>,
    pub sfields: StackMap<SFieldList, SFieldEnt, SField>,
}

impl Types {
    pub fn new() -> Self {
        Types {
            funcs: ListPool::new(),
            ents: PrimaryMap::new(),
            cons: StackMap::new(),
            sfield_lookup: Map::new(),
            sfields: StackMap::new(),
        }
    }
}

pub struct SFieldRef {
    pub field: SField,
    pub next: PackedOption<ID>,
}

impl SFieldRef {
    pub fn new(field: SField) -> Self {
        SFieldRef {
            field,
            next: None.into(),
        }
    }
}

impl ReservedValue for SFieldRef {
    fn reserved_value() -> Self {
        SFieldRef {
            field: ReservedValue::reserved_value(),
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

#[derive(Default)]
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
    Bound(EntityList<Func>),
    Struct(SFieldList),
    Int(i16),
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
            Kind::Struct(_) | Kind::Bound(_) => {
                let name = types.ents[self].name;
                write!(to, "{}", sources.display(name))?;
            }
            Kind::Int(base) => {
                if base > 0 {
                    write!(to, "i{}", base)?;
                } else {
                    write!(to, "int")?;
                }
            }
            Kind::Bool => write!(to, "bool")?,
            Kind::Nothing => write!(to, "nothing")?,
            Kind::Unresolved => write!(to, "unresolved")?,
        }

        Ok(())
    }
}

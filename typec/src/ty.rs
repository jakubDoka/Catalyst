use crate::Result;
use cranelift_entity::{
    packed_option::{PackedOption, ReservedValue},
    EntityList, ListPool, PrimaryMap,
};
use lexer::*;
use modules::*;
use parser::*;

pub struct Builder<'a> {
    pub scope: &'a mut Scope,
    pub types: &'a mut Types,
    pub sources: &'a Sources,
    pub ast: &'a ast::Data,
    pub graph: &'a mut GenericGraph,
    pub ty: Ty,
}

impl<'a> Builder<'a> {
    pub fn build(&mut self) -> Result {
        let Ent { id, ast, .. } = self.types.ents[self.ty];
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

    pub fn build_struct(&mut self, id: ID, ast: Ast) -> Result {
        let &[name, body] = self.ast.children(ast) else {
            unreachable!();
        };
        let name = self.ast.nodes[name].span;

        // fields are inserted into centralized hash map for faster lookup
        // and memory efficiency, though we still need field ordering when
        // calculating offsets
        let fields = {
            let mut fields = vec![]; // TODO: rather reuse allocation
            for &field in self.ast.children(body) {
                let children = self.ast.children(field);
                let &[.., ty] = children else {
                    unreachable!();
                };
                let ty = self.parse_type(ty)?;
                for &name in &children[..children.len() - 1] {
                    let id = {
                        let span = self.ast.nodes[name].span;
                        let str = self.sources.display(span);
                        Self::field_id(id, ID::new(str))
                    };
                    let field = Field::new(ty, fields.len() as u32, None);
                    assert!(self.types.fields.insert(id, field).is_none());
                    fields.push(ty);
                    self.graph.add_edge(ty.as_u32());
                }
            }
            self.types.cons.list(&fields)
        };

        let ent = Ent {
            id,
            kind: Kind::Struct(fields),
            ast,
            name,
        };
        self.types.ents[self.ty] = ent;
        self.graph.close_node();

        Ok(())
    }

    pub fn parse_type(&mut self /* mut on purpose */, ty: Ast) -> Result<Ty> {
        crate::parse_type(self.scope, self.ast, self.sources, ty)
    }

    pub fn field_id(ty: ID, name: ID) -> ID {
        ty + name
    }
}

pub struct Types {
    pub ents: PrimaryMap<Ty, Ent>,
    pub fields: Map<Field>,
    pub cons: ListPool<Ty>,
}

impl Types {
    pub fn new() -> Self {
        Types {
            ents: PrimaryMap::new(),
            cons: ListPool::new(),
            fields: Map::new(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Field {
    pub ty: Ty,
    pub index: u32,
    pub next: PackedOption<ID>,
}

impl Field {
    pub fn new(ty: Ty, index: u32, next: impl Into<PackedOption<ID>>) -> Self {
        Self {
            ty,
            index,
            next: next.into(),
        }
    }
}

impl ReservedValue for Field {
    fn reserved_value() -> Self {
        Field {
            ty: Ty::reserved_value(),
            index: u32::MAX,
            next: Default::default(),
        }
    }

    fn is_reserved_value(&self) -> bool {
        self.ty.is_reserved_value() && self.index == u32::MAX
    }
}

pub struct Ent {
    pub id: ID,
    pub name: Span,
    pub ast: Ast,
    pub kind: Kind,
}

impl Ent {
    pub fn new(kind: Kind, id: ID) -> Self {
        Self {
            id,
            kind,
            name: Default::default(),
            ast: Default::default(),
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
        match self.types.ents[self.ty].kind {
            Kind::Struct(_) => {
                let name = self.types.ents[self.ty].name;
                write!(f, "struct {}", self.sources.display(name))
            }
            Kind::Int(base) => {
                if base > 0 {
                    write!(f, "i{}", base)
                } else {
                    write!(f, "int")
                }
            }
            Kind::Bool => write!(f, "bool"),
            Kind::Nothing => write!(f, "nothing"),
            Kind::Unresolved => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Kind {
    Struct(EntityList<Ty>),
    Int(i16),
    Bool,
    Nothing,
    Unresolved,
}

lexer::gen_entity!(Ty);

use cranelift_entity::{
    packed_option::{PackedOption, ReservedValue},
    EntityList, ListPool, PrimaryMap,
};
use lexer::{map::ID, Map, Sources, SourcesExt, Span};
use parser::ast::Ast;

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
            Kind::Unresolved => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Kind {
    Struct(EntityList<Ty>),
    Int(i16),
    Bool,
    Unresolved,
}

lexer::gen_entity!(Ty);

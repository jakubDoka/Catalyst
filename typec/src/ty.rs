use cranelift_entity::{ListPool, PrimaryMap};
use lexer::map::ID;

pub struct Types {
    pub ents: PrimaryMap<Ty, Ent>,
    pub cons: ListPool<Ty>,
}

impl Types {
    pub fn new() -> Self {
        Types {
            ents: PrimaryMap::new(),
            cons: ListPool::new(),
        }
    }
}

pub struct Ent {
    pub id: ID,
    pub kind: Kind,
}

impl Ent {
    pub fn new(kind: Kind, id: ID) -> Self {
        Self { id, kind }
    }
}

pub struct Display<'a> {
    types: &'a Types,
    ty: Ty,
}

impl<'a> Display<'a> {
    pub fn new(types: &'a Types, ty: Ty) -> Self {
        Self { types, ty }
    }
}

impl std::fmt::Display for Display<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.types.ents[self.ty].kind {
            Kind::Int(base) => write!(f, "int{}", if base > 10 { base.to_string() } else { "".to_string() }),
            Kind::Bool => write!(f, "bool"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Kind {
    Int(i16),
    Bool,
}

lexer::gen_entity!(Ty);
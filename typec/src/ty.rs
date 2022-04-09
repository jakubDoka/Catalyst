use cranelift_entity::{EntityList, ListPool, PrimaryMap};
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

    pub fn slice(&self, slice: EntityList<Ty>) -> &[Ty] {
        slice.as_slice(&self.cons)
    }

    pub fn get(&self, ty: Ty) -> &Ent {
        &self.ents[ty]
    }

    pub fn add(&mut self, ty: Ent) -> Ty {
        self.ents.push(ty)
    }

    pub fn add_slice(&mut self, slice: &[Ty]) -> EntityList<Ty> {
        EntityList::from_slice(slice, &mut self.cons)
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

pub enum Kind {
    Int(i16),
    Bool,
}

lexer::gen_entity!(Ty);
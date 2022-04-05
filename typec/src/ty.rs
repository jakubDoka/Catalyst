use cranelift_entity::{EntityList, ListPool, PrimaryMap};
use lexer::map::ID;

pub struct Types {
    types: PrimaryMap<Ty, Ent>,
    ty_cons: ListPool<Ty>,
}

impl Types {
    pub fn new() -> Self {
        Types {
            types: PrimaryMap::new(),
            ty_cons: ListPool::new(),
        }
    }

    pub fn slice(&self, slice: EntityList<Ty>) -> &[Ty] {
        slice.as_slice(&self.ty_cons)
    }

    pub fn get(&self, ty: Ty) -> &Ent {
        &self.types[ty]
    }

    pub fn add(&mut self, ty: Ent) -> Ty {
        self.types.push(ty)
    }

    pub fn add_slice(&mut self, slice: &[Ty]) -> EntityList<Ty> {
        EntityList::from_slice(slice, &mut self.ty_cons)
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
}

lexer::gen_entity!(Ty);
modules::impl_item_data_for_entity!(Ty);

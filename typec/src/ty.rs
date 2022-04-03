use lexer::map::ID;

pub struct Ent {
    pub id: ID,
    pub kind: Kind,
}

pub enum Kind {
    Int(i16),
}

lexer::gen_entity!(Ty);
modules::impl_item_data_for_entity!(Ty);

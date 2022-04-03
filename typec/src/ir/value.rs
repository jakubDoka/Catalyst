use cranelift_entity::EntityList;
use parser::ast::Ast;

use crate::ty::Ty;

use super::inst::Inst;

pub struct Ent {
    pub ty: Ty,    
    pub ast: Ast,
    pub dep: EntityList<Inst>,
}

impl Ent {


    pub fn new(ty: Ty, ast: Ast) -> Ent {
        Ent {
            ty,
            ast,
            dep: EntityList::new(),
        }
    }
}

lexer::gen_entity!(Value);
modules::impl_item_data_for_entity!(Value);
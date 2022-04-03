use crate::ty::Ty;
use cranelift_entity::{packed_option::PackedOption, EntityList};
use parser::ast::Ast;

pub struct Ent {
    pub sig: Signature,
    pub ast: Ast,
}

#[derive(Clone, Copy, Default)]
pub struct Signature {
    //pub params: EntityList<Ty>,
    pub call_conv: Ast,
    pub args: EntityList<Ty>,
    pub ret: PackedOption<Ty>,
}

lexer::gen_entity!(Func);
modules::impl_item_data_for_entity!(Func);

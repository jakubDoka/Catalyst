use lexer::prelude::Span;

use crate::ty::Ty;

pub struct Ent {
    pub ty: Ty,
    pub span: Span,
}

impl Ent {
    pub fn new(ty: Ty, span: Span) -> Ent {
        Ent { ty, span }
    }
}

lexer::gen_entity!(Value);
modules::impl_item_data_for_entity!(Value);

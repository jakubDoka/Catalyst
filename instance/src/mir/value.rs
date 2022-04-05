use cranelift_codegen::ir::Type;

use crate::size::Size;

#[derive(Debug)]
pub struct Ent {
    pub repr: Type,
    pub offset: Size,
}

impl Ent {
    pub fn new(repr: Type) -> Ent {
        Ent {
            repr,
            offset: Size::ZERO,
        }
    }
}

lexer::gen_entity!(Value);

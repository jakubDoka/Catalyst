use cranelift_codegen::ir::Type;

use crate::size::Size;

pub struct Ent {
    pub repr: Type,
    pub offset: Size,
}

lexer::gen_entity!(Value);

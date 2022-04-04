use cranelift_codegen::packed_option::PackedOption;

pub struct Ent {
    pub prev: PackedOption<Block>,
    pub next: PackedOption<Block>,
}

typec::impl_linked_node!(inout Block, Ent);
lexer::gen_entity!(Block);

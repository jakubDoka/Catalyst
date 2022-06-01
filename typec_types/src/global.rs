use crate::*;
use lexer::*;
use storage::*;

pub type GlobalMap = Map<Global>;
pub type GlobalData = StackMap<GlobalBytes, u8>;
pub type Globals = PrimaryMap<Global, GlobalEnt>;

#[derive(Clone, Copy, Default)]
pub struct GlobalEnt {
    pub id: ID,
    pub name: Span,
    pub mutable: bool,
    pub ty: Ty,
    pub init: Func,
    pub bytes: PackedOption<GlobalBytes>,
}

gen_entity!(Global);
gen_entity!(GlobalBytes);
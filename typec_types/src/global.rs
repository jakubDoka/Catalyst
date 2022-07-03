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
    pub flags: GlobalFlags,
    pub ty: Ty,
    pub init: PackedOption<Func>,
    pub bytes: PackedOption<GlobalBytes>,
}

bitflags! {
    #[derive(Default)]
    pub struct GlobalFlags: u32 {
        const MUTABLE = 1 << 0;
        const THREAD_LOCAL = 1 << 1;
        const WRITABLE = 1 << 2;
    }
}

impl_bool_bit_and!(GlobalFlags);

gen_entity!(Global);
gen_entity!(GlobalBytes);

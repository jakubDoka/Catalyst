use crate::*;
use cranelift_codegen::ir::Type;
use storage::*;
use typec_types::*;

pub struct ReplaceCache {
    data: Vec<(Ty, TyEnt)>,
}

impl ReplaceCache {
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

    pub fn save(&mut self, from: Ty, to: Ty, types: &mut Types, reprs: &mut Reprs) {
        let to_ent = types[to];
        types[to] = types[from];
        reprs[to] = reprs[from];
        self.data.push((to, to_ent));
    }

    pub fn replace(&mut self, types: &mut Types, reprs: &mut Reprs) {
        for (id, ent) in self.data.drain(..) {
            types[id] = ent;
            reprs[id] = Default::default();
        }
    }
}

pub type Reprs = SecondaryMap<Ty, ReprEnt>;
pub type ReprFields = StackMap<ReprFieldList, ReprField>;

#[derive(Debug, Clone, Copy, Default)]
pub struct ReprEnt {
    pub repr: Type,
    pub fields: ReprFieldList,
    pub layout: Layout,
    pub flags: ReprFlags,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct ReprField {
    pub offset: Offset,
}

impl ReservedValue for ReprField {
    fn reserved_value() -> Self {
        Self {
            offset: Offset::new(i32::MAX, i32::MAX),
        }
    }

    fn is_reserved_value(&self) -> bool {
        self.offset == Offset::new(i32::MAX, i32::MAX)
    }
}

bitflags! {
    #[derive(Default)]
    pub struct ReprFlags: u32 {
        /// This type cannot fit into register.
        const ON_STACK = 1 << 0;
        /// This type can be safely copied.
        const COPYABLE = 1 << 1;
    }
}

impl_bool_bit_and!(ReprFlags);

gen_entity!(ReprFieldList);

use cranelift_codegen::{ir, packed_option::PackedOption};
use cranelift_entity::{ListPool, PrimaryMap};

use crate::mir::{
    block::{self, Block},
    inst::{self, Inst},
    value::{self, Value},
};

pub struct Function {
    pub signature: ir::Signature,
    pub values: PrimaryMap<Value, value::Ent>,
    pub value_slices: ListPool<Value>,
    pub blocks: PrimaryMap<Block, block::Ent>,
    pub insts: PrimaryMap<Inst, inst::Ent>,

    pub start: PackedOption<Block>,
    pub end: PackedOption<Block>,
}

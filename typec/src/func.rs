use crate::{
    tir::{
        block::{self, Block},
        inst::{self, Inst},
        value::{self, Value},
        LinkedList,
    },
    ty::{Ty, self}, Types,
};
use cranelift_entity::{packed_option::PackedOption, EntityList, ListPool, PrimaryMap};
use lexer::{Sources, Span, ID, ListPoolExt, SourcesExt};
use parser::ast::{self, Ast};

pub struct Funcs {
    pub ents: PrimaryMap<Func, Ent>,
    pub blocks: PrimaryMap<Block, block::Ent>,
    pub values: PrimaryMap<Value, value::Ent>,
    pub insts: PrimaryMap<Inst, inst::Ent>,
    pub value_slices: ListPool<Value>,
}

impl Funcs {
    pub fn new() -> Self {
        Funcs {
            ents: PrimaryMap::new(),
            blocks: PrimaryMap::new(),
            values: PrimaryMap::new(),
            insts: PrimaryMap::new(),
            value_slices: ListPool::new(),
        }
    }

    pub fn block_params(&self, block: Block) -> impl Iterator<Item = (Value, &value::Ent)> + '_ {
        let args = self.blocks[block].args;
        let view = self.value_slices.view(args);
        view
            .iter()
            .map(|&value| (value, &self.values[value]))
    }

    pub fn display<'a>(
        &'a self,
        func: Func,
        types: &'a Types,
        sources: &'a Sources,
        ast_data: &'a ast::Data,
    ) -> Display<'a> {
        Display {
            funcs: self,
            func,
            types,
            sources,
            _ast_data: ast_data,
        }
    }

    pub fn insts_of(&self, id: Block) -> impl Iterator<Item = (Inst, &inst::Ent)> {
        self.insts.linked_iter(self.blocks[id].first.expand())
    }

    pub fn blocks_of(&self, func: Func) -> impl Iterator<Item = (Block, &block::Ent)> {
        self.blocks.linked_iter(self.ents[func].start.expand())
    }
}

pub struct Display<'a> {
    func: Func,
    funcs: &'a Funcs,
    types: &'a Types,
    sources: &'a Sources,
    _ast_data: &'a ast::Data,
}

impl std::fmt::Display for Display<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let func = &self.funcs.ents[self.func];
        writeln!(f, "fn {}{} {{", self.sources.display(func.name), SignatureDisplay::new(&func.sig, self.types))?;
        for (id, _) in self.funcs.blocks_of(self.func) {
            let args = self.funcs.block_params(id)
                .map(|(value, ent)| {
                    format!(
                        "{}: {}",
                        value,
                        ty::Display::new(self.types, ent.ty),
                    )
                })
                .collect::<Vec<_>>()
                .join(", ");
            writeln!(f, "  {}({}):", id, args)?;
            for (_, inst) in self.funcs.insts_of(id) {
                write!(f, "    ")?;
                match inst.kind {
                    inst::Kind::Variable => {
                        writeln!(f, "var {}", inst.value.unwrap())?;
                    },
                    inst::Kind::Assign(value) => {
                        writeln!(f, "{} = {}", value, inst.value.unwrap())?;
                    },
                    inst::Kind::JumpIfFalse(block) => {
                        writeln!(f, "if {} goto {}", inst.value.unwrap(), block)?;
                    },
                    inst::Kind::Jump(block) => {
                        if let Some(value) = inst.value.expand() {
                            writeln!(f, "goto {}({})", block, value)?;
                        } else {
                            writeln!(f, "goto {}", block)?;
                        }
                    },
                    inst::Kind::Call(func, args) => {
                        let name = self.funcs.ents[func].name;
                        let args = self.funcs.value_slices
                            .view(args)
                            .iter()
                            .map(|&value| format!("{value}"))
                            .collect::<Vec<_>>()
                            .join(", ");
                        let ty = self.funcs.values[inst.value.unwrap()].ty;
                        let ty = ty::Display::new(self.types, ty);
                        writeln!(f, "{}: {} = {}({})", inst.value.unwrap(), ty, self.sources.display(name), args)?;
                    },
                    inst::Kind::BoolLit(value) => {
                        let ty = self.funcs.values[inst.value.unwrap()].ty;
                        let ty = ty::Display::new(self.types, ty);
                        writeln!(f, "{}: {} = {}", inst.value.unwrap(), ty, value)?;
                    },
                    inst::Kind::IntLit => {
                        let ty = self.funcs.values[inst.value.unwrap()].ty;
                        let ty = ty::Display::new(self.types, ty);
                        writeln!(f, "{}: {} = {}", inst.value.unwrap(), ty, self.sources.display(inst.span))?;
                    },
                    inst::Kind::Return => {
                        if let Some(value) = inst.value.expand() {
                            writeln!(f, "return {}", value)?;
                        } else {
                            writeln!(f, "return")?;
                        }
                    },
                }
            }
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

#[derive(Default)]
pub struct Ent {
    pub sig: Signature,
    pub name: Span,
    pub kind: Kind,
    pub ast: Ast,
    pub id: ID,
    pub return_type: PackedOption<Ty>,
    pub start: PackedOption<Block>,
    pub end: PackedOption<Block>,
}

#[derive(PartialEq, Eq)]
pub enum Kind {
    Local,
    External,
    Builtin,
}

impl Default for Kind {
    fn default() -> Self {
        Kind::Local
    }
}

#[derive(Clone, Copy, Default)]
pub struct Signature {
    //pub params: EntityList<Ty>,
    pub call_conv: Span,
    pub args: EntityList<Ty>,
    pub ret: PackedOption<Ty>,
}

pub struct SignatureDisplay<'a> {
    pub sig: &'a Signature,
    pub types: &'a Types,
}

impl<'a> SignatureDisplay<'a> {
    pub fn new(sig: &'a Signature, types: &'a Types) -> Self {
        SignatureDisplay { sig, types }
    }
}

impl std::fmt::Display for SignatureDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        for (i, &ty) in self.types.cons.view(self.sig.args).iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", ty::Display::new(self.types, ty))?;
        }
        write!(f, ")")?;
        if let Some(ty) = self.sig.ret.expand() {
            write!(f, " -> {}", ty::Display::new(self.types, ty))?;
        }
        Ok(())
    }
}

lexer::gen_entity!(Func);
use std::str::FromStr;

use crate::types as ity;
use cranelift_codegen::{
    ir::{self, Type},
    isa::CallConv,
};
use cranelift_entity::{EntityList, SecondaryMap};
use lexer::{ListPoolExt, Sources, SourcesExt, ID};
use modules::tree::{GenericGraph, TreeStorage};
use typec::{self, tir, ty, Signature, Ty};

use crate::{
    error::{self, Error},
    func,
    mir::{
        self,
        value::{self, Flags},
    },
    size::Size,
    types::Types,
};

type Result<T = ()> = std::result::Result<T, Error>;

pub struct TypeTranslator<'a> {
    pub t_types: &'a typec::Types,
    pub t_graph: &'a GenericGraph,
    pub types: &'a mut Types,
    pub ptr_ty: Type,
}

impl<'a> TypeTranslator<'a> {
    pub fn translate(&mut self) -> Result {
        let order = {
            let mut vec: Vec<Ty> = Vec::with_capacity(self.t_types.ents.len());
            self.t_graph.total_ordering(&mut vec);
            vec
        };

        self.types.ents.resize(self.t_types.ents.len());
        for id in order {
            let ty = &self.t_types.ents[id];
            let repr = match ty.kind {
                ty::Kind::Int(base) => match base {
                    64 => ir::types::I64,
                    32 => ir::types::I32,
                    16 => ir::types::I16,
                    8 => ir::types::I8,
                    _ => self.ptr_ty,
                },
                ty::Kind::Bool => ir::types::B1,
                ty::Kind::Struct(fields) => {
                    self.translate_struct(id, fields)?;
                    continue;
                }
                ty::Kind::Unresolved => unreachable!(),
            };

            let bytes = repr.bytes() as i32;
            let size = Size::new(bytes, bytes);
            self.types.ents[id] = ity::Ent {
                repr,
                size,
                small: true,
                align: size.min(Size::PTR),
            };
        }
        Ok(())
    }

    pub fn translate_struct(&mut self, ty: Ty, fields: EntityList<Ty>) -> Result {
        let fields = self.t_types.cons.view(fields);
        let ty_id = self.t_types.ents[ty].id;

        let align = fields
            .iter()
            .map(|&field| self.types.ents[field].align)
            .fold(Size::ZERO, |acc, align| acc.max(align).min(Size::PTR));

        let mut size = Size::ZERO;
        for (i, &field) in fields.iter().enumerate() {
            let ent = &self.types.ents[field];

            let field = ity::Field {
                repr: ent.repr,
                offset: size,
            };
            let id = ty_id + ID(i as u64);
            assert!(self.types.fields.insert(id, field).is_none(), "{id:?}");

            size = size + ent.size;
            let padding = align - size % align;
            if padding != align {
                size = size + padding;
            }
        }

        let (repr, small) = Self::smallest_repr_for(size, self.ptr_ty);
        self.types.ents[ty] = ity::Ent {
            repr,
            small,
            size,
            align,
        };

        Ok(())
    }

    pub fn smallest_repr_for(size: Size, ptr_ty: Type) -> (Type, bool) {
        let size = size.arch(ptr_ty.bytes() == 4);
        let repr = match size {
            0 => ir::types::INVALID,
            1 => ir::types::I8,
            2 => ir::types::I16,
            4 => ir::types::I32,
            _ => ptr_ty,
        };
        (repr, size == repr.bytes() as i32)
    }
}

pub struct FunctionTranslator<'a> {
    pub system_call_convention: CallConv,
    pub value_lookup: &'a mut SecondaryMap<tir::Value, mir::Value>,
    pub block_lookup: &'a mut SecondaryMap<tir::Block, mir::Block>,
    pub function: &'a mut func::Function,
    pub t_functions: &'a typec::Funcs,
    pub t_types: &'a typec::Types,
    pub types: &'a Types,
    pub sources: &'a Sources,
}

impl<'a> FunctionTranslator<'a> {
    pub fn translate_func(&mut self, func: typec::Func) -> Result<()> {
        self.function.clear();

        Self::translate_signature(
            &self.t_functions.ents[func].sig,
            &mut self.function.signature,
            self.sources,
            self.types,
            self.t_types,
            self.system_call_convention,
        )?;

        self.function.name = self.t_functions.ents[func].name;

        for (id, _) in self.t_functions.blocks_of(func) {
            let block = self.function.create_block();
            self.block_lookup[id] = block;
        }

        for (id, _) in self.t_functions.blocks_of(func) {
            self.translate_block(id)?;
        }

        Ok(())
    }

    pub fn translate_signature(
        &Signature {
            call_conv,
            args,
            ret,
        }: &Signature,
        target: &mut ir::Signature,
        sources: &Sources,
        types: &Types,
        t_types: &typec::Types,
        system_call_convention: CallConv,
    ) -> Result<()> {
        target.call_conv = {
            if call_conv.len() < 2 {
                CallConv::Fast
            } else {
                let str = sources.display(call_conv.strip_sides());
                if str == "default" {
                    system_call_convention
                } else {
                    CallConv::from_str(str)
                        .map_err(|_| Error::new(error::Kind::InvalidCallConv, call_conv))?
                }
            }
        };

        let params = t_types.cons.view(args).iter().map(|&ty| {
            let repr = types.ents[ty].repr;
            ir::AbiParam::new(repr)
        });
        target.params.extend(params);

        if let Some(ty) = ret.expand() {
            let repr = types.ents[ty].repr;
            target.returns.push(ir::AbiParam::new(repr));
        }

        Ok(())
    }

    pub fn translate_block(&mut self, id: tir::Block) -> Result {
        let block = self.block_lookup[id];
        self.function.select_block(block);

        for (id, value) in self.t_functions.block_params(id) {
            let value = {
                let repr = self.types.ents[value.ty].repr;
                let ent = value::Ent::repr(repr);
                self.function.values.push(ent)
            };

            self.function.push_block_param(block, value);
            self.value_lookup[id] = value;
        }

        for (_, inst) in self.t_functions.insts_of(id) {
            self.translate_inst(inst)?;
        }

        Ok(())
    }

    pub fn translate_inst(&mut self, inst: &tir::inst::Ent) -> Result {
        match inst.kind {
            tir::Kind::Call(func, args) => {
                let inst = {
                    let arg_iter = self
                        .t_functions
                        .value_slices
                        .view(args)
                        .iter()
                        .map(|&arg| self.value_lookup[arg]);
                    let args = self.function.make_values(arg_iter);
                    let kind = mir::inst::Kind::Call(func, args);

                    let return_value = {
                        if let Some(value) = inst.value.expand() {
                            let ty = self.t_functions.values[value].ty;
                            let repr = self.types.ents[ty].repr;
                            let ent = mir::value::Ent::repr(repr);
                            let mir_value = self.function.values.push(ent);
                            self.value_lookup[value] = mir_value;
                            Some(mir_value)
                        } else {
                            None
                        }
                    };

                    mir::inst::Ent::new(kind, return_value)
                };
                self.function.add_inst(inst);
            }
            tir::Kind::IntLit => {
                let inst_value = inst.value.unwrap();

                let value = {
                    let ty = self.t_functions.values[inst_value].ty;
                    let repr = self.types.ents[ty].repr;
                    let ent = mir::value::Ent::repr(repr);
                    self.function.values.push(ent)
                };

                let inst = {
                    let literal = lexer::int_value(self.sources, inst.span);
                    let kind = mir::inst::Kind::IntLit(literal);
                    mir::inst::Ent::with_value(kind, value)
                };

                println!("{} => {}", inst_value, value);
                self.value_lookup[inst_value] = value;
                self.function.add_inst(inst);
            }
            tir::Kind::Return => {
                let inst = {
                    let value = inst.value.unwrap();
                    let value = self.value_lookup[value];
                    let kind = mir::inst::Kind::Return;
                    mir::inst::Ent::with_value(kind, value)
                };
                self.function.add_inst(inst);
            }
            tir::Kind::JumpIfFalse(block) => {
                let inst = {
                    let block = self.block_lookup[block];
                    let value = self.value_lookup[inst.value.unwrap()];
                    let kind = mir::inst::Kind::JumpIfFalse(block);
                    mir::inst::Ent::with_value(kind, value)
                };

                self.function.add_inst(inst);
            }
            tir::Kind::Jump(block) => {
                let inst = {
                    let block = self.block_lookup[block];
                    println!("{:?} {}", inst.value, self.sources.display(inst.span));
                    let value = inst.value.map(|value| self.value_lookup[value]);
                    let kind = mir::inst::Kind::Jump(block);
                    mir::inst::Ent::new(kind, value)
                };

                self.function.add_inst(inst);
            }
            tir::Kind::BoolLit(literal) => {
                let value = {
                    let ty = self.t_functions.values[inst.value.unwrap()].ty;
                    let repr = self.types.ents[ty];
                    let ent = mir::value::Ent::repr(repr.repr);
                    self.function.values.push(ent)
                };

                self.value_lookup[inst.value.unwrap()] = value;

                let inst = {
                    let kind = mir::inst::Kind::BoolLit(literal);
                    mir::inst::Ent::with_value(kind, value)
                };

                self.function.add_inst(inst);
            }
            tir::Kind::Assign(left) => {
                let left = self.value_lookup[left];
                self.function.values[left].flags |= Flags::MUTABLE;
                let right = self.value_lookup[inst.value.unwrap()];
                let kind = mir::inst::Kind::Assign(left);
                let inst = mir::inst::Ent::with_value(kind, right);
                self.function.add_inst(inst);
            }
            tir::Kind::Variable => {
                let mut value = self.value_lookup[inst.value.unwrap()];

                {
                    let mut ent = self.function.values[value];
                    let owned = ent.flags.is_owned();
                    ent.flags |= Flags::OWNED;
                    if owned {
                        let copy = {
                            ent.offset = Size::ZERO;
                            self.function.values.push(ent)
                        };

                        let inst = {
                            let kind = mir::inst::Kind::Copy(value);
                            mir::inst::Ent::with_value(kind, copy)
                        };
                        self.function.add_inst(inst);

                        value = copy;
                    }
                    // flags could change to owned which we want, either way
                    // the value will end up owned
                    self.function.values[value].flags = ent.flags;
                }

                let kind = mir::inst::Kind::Variable;
                let inst = mir::inst::Ent::with_value(kind, value);
                self.function.add_inst(inst);
            }
        }

        Ok(())
    }
}

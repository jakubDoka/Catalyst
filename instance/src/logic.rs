use std::str::FromStr;

use cranelift_codegen::{ir::{self, Type}, isa::CallConv};
use cranelift_entity::SecondaryMap;
use lexer::{Sources, SourcesExt};
use typec::{self, tir, Ty, Signature, ty};

use crate::{error::{Error, self}, func, mir::{self, value}};

type Result<T = ()> = std::result::Result<T, Error>;

pub struct TypeTranslator<'a> {
    pub repr_lookup: &'a mut SecondaryMap<Ty, Type>,
    pub t_types: &'a typec::Types,
    pub ptr_ty: Type,
}

impl<'a> TypeTranslator<'a> {
    pub fn translate(&mut self) -> Result {
        self.repr_lookup.resize(self.t_types.ents.len());
        for (id, ty) in self.t_types.ents.iter() {
            let repr = match ty.kind {
                ty::Kind::Int(base) => match base {
                    64 => ir::types::I64,
                    32 => ir::types::I32,
                    16 => ir::types::I16,
                    8 => ir::types::I8,
                    _ => self.ptr_ty,
                },
                ty::Kind::Bool => ir::types::B1,
            };
            self.repr_lookup[id] = repr;
        }
        Ok(())
    }    
}

pub struct FunctionTranslator<'a> {
    pub system_call_convention: CallConv,
    pub repr_lookup: &'a SecondaryMap<Ty, Type>,
    pub value_lookup: &'a mut SecondaryMap<tir::Value, mir::Value>,
    pub block_lookup: &'a mut SecondaryMap<tir::Block, mir::Block>,
    pub function: &'a mut func::Function,
    pub t_functions: &'a typec::Functions,
    pub t_types: &'a typec::Types,
    pub sources: &'a Sources,
}

impl<'a> FunctionTranslator<'a> {
    pub fn translate_func(&mut self, func: typec::Func) -> Result<()> {
        self.function.clear();

        Self::translate_signature(
            &self.t_functions.ents[func].sig, 
            &mut self.function.signature, 
            self.sources,
            self.repr_lookup,
            self.t_types, 
            self.system_call_convention
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
        &Signature { call_conv, args, ret }: &Signature, 
        target: &mut ir::Signature, 
        sources: &Sources,
        repr_lookup: &SecondaryMap<Ty, Type>,
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
        
        let params = t_types.slice(args).iter().map(|&ty| {
            let repr = repr_lookup[ty];
            ir::AbiParam::new(repr)
        });
        target.params.extend(params);

        if let Some(ty) = ret.expand() {
            let repr = repr_lookup[ty];
            target
                .returns
                .push(ir::AbiParam::new(repr));
        }

        Ok(())
    }

    pub fn translate_block(&mut self, id: tir::Block) -> Result {
        let block = self.block_lookup[id];
        self.function.select_block(block);

        for (id, value) in self.t_functions.block_params(id) {
            let value = {
                let repr = self.repr_lookup[value.ty];
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
                    let arg_iter = self.t_functions.values(args).iter().map(|&arg| self.value_lookup[arg]);
                    let args = self.function.make_values(arg_iter);
                    let kind = mir::inst::Kind::Call(func, args);
                    
                    let return_value = {
                        if let Some(value) = inst.result.expand() {
                            let ty = self.t_functions.values[value].ty;
                            let repr = self.repr_lookup[ty];
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
                let inst_value = inst.result.unwrap();

                let value = {
                    let ty = self.t_functions.values[inst_value].ty;
                    let repr = self.repr_lookup[ty];
                    let ent = mir::value::Ent::repr(repr);
                    self.function.values.push(ent)
                };

                let inst = {
                    let literal = lexer::int_value(self.sources, inst.span);
                    let kind = mir::inst::Kind::IntLit(literal);
                    mir::inst::Ent::with_value(kind, value)
                };

                self.value_lookup[inst_value] = value;
                self.function.add_inst(inst);
            }
            tir::Kind::Return => {
                let inst = {
                    let value = inst.result.unwrap();
                    let value = self.value_lookup[value];
                    let kind = mir::inst::Kind::Return;
                    mir::inst::Ent::with_value(kind, value)
                };
                self.function.add_inst(inst);
            }
            tir::Kind::JumpIfFalse(block) => {
                let inst = {
                    let block = self.block_lookup[block];
                    let value = self.value_lookup[inst.result.unwrap()];
                    let kind = mir::inst::Kind::JumpIfFalse(block);
                    mir::inst::Ent::with_value(kind, value)
                };

                self.function.add_inst(inst);
            },
            tir::Kind::Jump(block) => {
                let inst = {
                    let block = self.block_lookup[block];
                    let value = inst.result.map(|value| self.value_lookup[value]);
                    let kind = mir::inst::Kind::Jump(block);
                    mir::inst::Ent::new(kind, value)
                };

                self.function.add_inst(inst);
            },
            tir::Kind::BoolLit(literal) => {
                let value = {
                    let ty = self.t_functions.values[inst.result.unwrap()].ty;
                    let repr = self.repr_lookup[ty];
                    let ent = mir::value::Ent::repr(repr);
                    self.function.values.push(ent)
                };

                self.value_lookup[inst.result.unwrap()] = value;

                let inst = {
                    let kind = mir::inst::Kind::BoolLit(literal);
                    mir::inst::Ent::with_value(kind, value)
                };

                self.function.add_inst(inst);
            },
        }

        Ok(())
    }
}
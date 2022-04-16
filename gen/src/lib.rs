#![feature(explicit_generic_args_with_impl_trait)]
#![feature(toowned_clone_into)]
#![feature(let_else)]

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{InstBuilder, Signature, StackSlotData, MemFlags};
use cranelift_codegen::isa::TargetFrontendConfig;
use cranelift_codegen::{ir, packed_option::PackedOption};
use cranelift_entity::{SecondaryMap, EntityList, EntityRef};
use cranelift_frontend::{FunctionBuilder, Variable};
use cranelift_module::{FuncId, Module};
use instance::*;
use lexer::*;
use typec::Func;

pub struct Generator<'a> {
    pub module: &'a mut dyn Module,
    pub builder: &'a mut FunctionBuilder<'a>,
    pub value_lookup: &'a mut SecondaryMap<mir::Value, PackedOption<ir::Value>>,
    pub function_lookup: &'a SecondaryMap<Func, PackedOption<FuncId>>,
    pub block_lookup: &'a mut SecondaryMap<mir::Block, PackedOption<ir::Block>>,
    pub stack_slot_lookup: &'a mut SecondaryMap<mir::Stack, PackedOption<ir::StackSlot>>,
    pub t_functions: &'a typec::Funcs,
    pub types: &'a instance::Types,
    pub source: &'a instance::func::Func,
    pub sources: &'a Sources,
}

impl<'a> Generator<'a> {
    pub fn generate(&mut self) {
        Self::transfer_signature(
            &self.source.sig,
            &mut self.builder.func.signature
        );

        for (id, stack) in self.source.stacks.iter() {
            let slot = StackSlotData::new(ir::StackSlotKind::ExplicitSlot, stack.size);
            let slot = self.builder.create_stack_slot(slot);
            self.stack_slot_lookup[id] = slot.into();
        }

        for (id, _) in self.source.blocks() {
            let block = self.builder.create_block();
            self.block_lookup[id] = block.into();
        }

        for (id, _) in self.source.blocks() {
            self.generate_block(id);
        }

        self.builder.seal_all_blocks();
        self.builder.finalize();
    }

    fn transfer_signature(from: &Signature, to: &mut Signature) {
        to.clear(from.call_conv);
        to.params.extend(from.params.iter().cloned());
        to.returns.extend(from.returns.iter().cloned());
    }

    fn generate_block(&mut self, id: mir::Block) {
        let block = self.block_lookup[id].unwrap();
        self.builder.switch_to_block(block);

        for (value, ent) in self.source.block_params(id) {
            let repr = self.types.ents[ent.ty].repr;
            let ir_value = self.builder.append_block_param(block, repr);
            self.value_lookup[value] = ir_value.into();
        }

        for (_, inst) in self
            .source
            .insts
            .linked_iter(self.source.blocks[id].start.expand())
        {
            self.generate_inst(inst);
        }
    }

    fn generate_inst(&mut self, inst: &mir::InstEnt) {
        match inst.kind {
            mir::InstKind::Call(func, args) => {
                if self.t_functions[func].kind == typec::func::Kind::Builtin {
                    self.generate_native_call(func, args, inst.value);
                    return;
                }

                let ir_inst = {
                    let func_ref = {
                        let ir_func = self.function_lookup[func].unwrap();
                        self.module.declare_func_in_func(ir_func, self.builder.func)
                    };
                    
                    let args: Vec<_> = self.source.value_slices
                        .view(args)
                        .iter()
                        .map(|&value| self.use_value(value))
                        .collect();
                    
                    self.builder.ins().call(func_ref, &args)
                };
                
                self.builder.inst_results(ir_inst).get(0).map(|&value| {
                    self.value_lookup[inst.value.unwrap()] = value.into();
                });
            }
            mir::InstKind::IntLit(literal) => {
                let value = inst.value.unwrap();
                let repr = self.repr_of(value);
                let ir_value = self.builder.ins().iconst(repr, literal as i64);
                self.value_lookup[value] = ir_value.into();
            }
            mir::InstKind::Return => {
                if let Some(value) = inst.value.expand() {
                    let ir_value = self.use_value(value);
                    self.builder.ins().return_(&[ir_value]);
                } else {
                    self.builder.ins().return_(&[]);
                }
            }
            mir::InstKind::JumpIfFalse(block) => {
                let block = self.block_lookup[block].unwrap();
                let value = self.use_value(inst.value.unwrap());
                self.builder.ins().brz(value, block, &[]);
            },
            mir::InstKind::Jump(block) => {
                let block = self.block_lookup[block].unwrap();
                if let Some(value) = inst.value.expand() {
                    let value = self.use_value(value);
                    self.builder.ins().jump(block, &[value]);
                } else {
                    self.builder.ins().jump(block, &[]);
                }
            },
            mir::InstKind::BoolLit(literal) => {
                let value = inst.value.unwrap();
                let repr = self.repr_of(value);
                let ir_value = self.builder.ins().bconst(repr, literal);
                self.value_lookup[value] = ir_value.into();
            },
            mir::InstKind::Variable => {
                let value = inst.value.unwrap();
                let repr = self.repr_of(value);
                let flags = self.source.values[value].flags;
                if flags.contains(mir::Flags::MUTABLE) {
                    let variable = Variable::new(value.index());
                    self.builder.declare_var(variable, repr);
                    let value = self.value_lookup[value].unwrap();
                    self.builder.def_var(variable, value);
                }
            },
            InstKind::Offset(value) => {
                self.value_lookup[inst.value.unwrap()] = 
                    self.value_lookup[value].unwrap().into(); // check
            },
            InstKind::Assign(value) => {
                let target = inst.value.unwrap();

                let (size, dest_align) = {
                    let target_ty = self.source.values[target].ty;
                    let size = self.types.ents[target_ty].size;
                    let align = self.types.ents[target_ty].align;
                    (self.unwrap_size(size), self.unwrap_size(align))
                };

                let src_align = {
                    let value_ty = self.source.values[value].ty;
                    let align = self.types.ents[value_ty].align;
                    self.unwrap_size(align)
                };

                match (
                    self.source.values[value].flags.contains(mir::Flags::POINTER), 
                    self.source.values[target].flags.contains(mir::Flags::POINTER),
                ) {
                    (true, true) => {
                        let target_ir = self.use_value(target);
                        let value_ir = self.use_value(value);
                        self.builder.emit_small_memory_copy(
                            self.module.isa().frontend_config(), 
                            target_ir, 
                            value_ir, 
                            size as u64, 
                            dest_align as u8, 
                            src_align as u8, 
                            false, 
                            MemFlags::new(),
                        )
                    },
                    (true, false) => {
                        let offset = {
                            let offset = self.source.values[value].offset;
                            self.unwrap_size(offset)
                        };
                        let repr = self.repr_of(target);
                        let value = self.use_value(value);
                        let value = self.builder.ins().load(repr, MemFlags::new(), value, offset);
                        
                        let variable = Variable::new(target.index());
                        let right = self.use_value(inst.value.unwrap());
                        self.builder.def_var(variable, right);
                    },
                    (false, true) => {
                        let offset = {
                            let offset = self.source.values[target].offset;
                            self.unwrap_size(offset)
                        };
                        let target = self.use_value(target);
                        let value = self.use_value(value);
                        self.builder.ins().store(MemFlags::new(), value, target, offset as i32);
                    },
                    (false, false) => {
                        let variable = Variable::new(target.index());
                        let right = self.use_value(inst.value.unwrap());
                        self.builder.def_var(variable, right);
                    },
                }
            },
            InstKind::StackAddr(_) => todo!(),
        }
    }

    fn unwrap_size(&self, size: Size) -> i32 {
        size.arch(self.module.isa().pointer_bytes() == 4)
    }

    fn repr_of(&self, value: mir::Value) -> ir::types::Type {
        let ty = self.source.values[value].ty;
        self.types.ents[ty].repr
    }

    fn generate_native_call(&mut self, func: Func, args: EntityList<mir::Value>, result: PackedOption<mir::Value>) {
        let name = self.t_functions[func].name;
        let str = self.sources.display(name);

        match str {
            "+" => self.generate_native_add(args, result),
            "-" => self.generate_native_sub(args, result),
            "*" => self.generate_native_mul(args, result),
            "/" => self.generate_native_div(args, result),
            "<" | 
            ">" | 
            "<=" | 
            ">=" | 
            "==" | 
            "!="=> self.generate_native_cmp(str, args, result),
            _ => todo!("Unhandled native function: {:?}", str),
        }
    }

    fn generate_native_add(&mut self, args: EntityList<mir::Value>, result: PackedOption<mir::Value>) {
        match self.source.value_slices.view(args) {
            &[value] => {
                self.value_lookup[result.unwrap()] = self.use_value(value).into();
            }
            &[left, right] => {
                let left = self.use_value(left);
                let right = self.use_value(right);
                
                let ty = self.builder.func.dfg.value_type(left); 

                assert!(ty == self.builder.func.dfg.value_type(right));
                
                let add = if ty.is_int() {
                    self.builder.ins().iadd(left, right)
                } else {
                    todo!("Unimplemented addition for {}", ty);
                };

                self.value_lookup[result.unwrap()] = add.into();
            }
            _ => unreachable!(),
        }
    }

    fn generate_native_sub(&mut self, args: EntityList<mir::Value>, result: PackedOption<mir::Value>) {
        match self.source.value_slices.view(args) {
            &[value] => {
                let value = self.use_value(value);
                let ty = self.builder.func.dfg.value_type(value);

                let neg = if ty.is_int() {
                    self.builder.ins().ineg(value)
                } else {
                    todo!("Unimplemented subtraction for {}", ty);
                };

                self.value_lookup[result.unwrap()] = neg.into();
            }
            &[left, right] => {
                let left = self.use_value(left);
                let right = self.use_value(right);
                
                let ty = self.builder.func.dfg.value_type(left); 

                assert!(ty == self.builder.func.dfg.value_type(right));
                
                let sub = if ty.is_int() {
                    self.builder.ins().isub(left, right)
                } else {
                    todo!("Unimplemented subtraction for {}", ty);
                };

                self.value_lookup[result.unwrap()] = sub.into();
            }
            _ => unreachable!(),
        }
    }

    fn generate_native_mul(&mut self, args: EntityList<mir::Value>, result: PackedOption<mir::Value>) {
        match self.source.value_slices.view(args) {
            &[left, right] => {
                let left = self.use_value(left);
                let right = self.use_value(right);
                
                let ty = self.builder.func.dfg.value_type(left); 

                assert!(ty == self.builder.func.dfg.value_type(right));
                
                let mul = if ty.is_int() {
                    self.builder.ins().imul(left, right)
                } else {
                    todo!("Unimplemented multiplication for {}", ty);
                };

                self.value_lookup[result.unwrap()] = mul.into();
            }
            _ => unreachable!(),
        }
    }

    fn generate_native_div(&mut self, args: EntityList<mir::Value>, result: PackedOption<mir::Value>) {
        match self.source.value_slices.view(args) {
            &[left, right] => {
                let signed = !self.source.values[left].flags.contains(mir::Flags::UNSIGNED);
                let left = self.use_value(left);
                let right = self.use_value(right);
                
                let ty = self.builder.func.dfg.value_type(left); 

                assert!(ty == self.builder.func.dfg.value_type(right));
                
                let div = if ty.is_int() {
                    if signed {
                        self.builder.ins().sdiv(left, right)
                    } else {
                        self.builder.ins().udiv(left, right)
                    }
                } else {
                    todo!("Unimplemented division for {}", ty);
                };

                self.value_lookup[result.unwrap()] = div.into();
            }
            _ => unreachable!(),
        }
    }

    fn generate_native_cmp(&mut self, op: &str, args: EntityList<mir::Value>, result: PackedOption<mir::Value>) {
        let &[left, right] = self.source.value_slices.view(args) else {
            unreachable!();
        };

        let signed = !self.source.values[left].flags.contains(mir::Flags::UNSIGNED);

        let left = self.use_value(left);
        let right = self.use_value(right);

        let ty = self.builder.func.dfg.value_type(left);
        assert!(ty == self.builder.func.dfg.value_type(right));

        let result = result.unwrap();

        if ty.is_int() {
            let inst = if signed {
                match op {
                    "<" => IntCC::SignedLessThan,
                    ">" => IntCC::SignedGreaterThan,
                    "<=" => IntCC::SignedLessThanOrEqual,
                    ">=" => IntCC::SignedGreaterThanOrEqual,
                    "==" => IntCC::Equal,
                    "!=" => IntCC::NotEqual,
                    _ => unreachable!(),
                }
            } else {
                match op {
                    "<" => IntCC::UnsignedLessThan,
                    ">" => IntCC::UnsignedGreaterThan,
                    "<=" => IntCC::UnsignedLessThanOrEqual,
                    ">=" => IntCC::UnsignedGreaterThanOrEqual,
                    "==" => IntCC::Equal,
                    "!=" => IntCC::NotEqual,
                    _ => unreachable!(),
                }
            };

            let value = self.builder.ins().icmp(inst, left, right);
            self.value_lookup[result] = value.into();
        } else {
            todo!("Unimplemented comparison for {}", ty);
        }
    }

    fn use_value(&mut self, value: mir::Value) -> ir::Value {
        if self.source.values[value].flags.contains(mir::Flags::MUTABLE) {
            let variable = Variable::new(value.index());
            self.builder.use_var(variable)
        } else {
            self.value_lookup[value].unwrap()
        }
    }
}
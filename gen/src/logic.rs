use cranelift_codegen::ir::{InstBuilder, Signature};
use cranelift_codegen::{ir, packed_option::PackedOption};
use cranelift_entity::{SecondaryMap, EntityList};
use cranelift_frontend::FunctionBuilder;
use cranelift_module::{FuncId, Module};
use instance::mir;
use lexer::{Sources, SourcesExt};
use typec::Func;
use typec::tir::LinkedList;

pub struct Generator<'a> {
    pub module: &'a mut dyn Module,
    pub builder: &'a mut FunctionBuilder<'a>,
    pub value_lookup: &'a mut SecondaryMap<mir::Value, PackedOption<ir::Value>>,
    pub function_lookup: &'a SecondaryMap<Func, PackedOption<FuncId>>,
    pub block_lookup: &'a mut SecondaryMap<mir::Block, PackedOption<ir::Block>>,
    pub t_functions: &'a typec::Functions,
    pub source: &'a instance::Function,
    pub sources: &'a Sources,
}

impl<'a> Generator<'a> {
    pub fn generate(&mut self) {
        Self::transfer_signature(
            &self.source.signature,
            &mut self.builder.func.signature
        );

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
            println!("{value:?}");
            let ir_value = self.builder.append_block_param(block, ent.repr);
            self.value_lookup[value] = ir_value.into();
        }

        for (_, inst) in self
            .source
            .insts
            .linked_iter(self.source.blocks[id].first.expand())
        {
            self.generate_inst(inst);
        }
    }

    fn generate_inst(&mut self, inst: &mir::inst::Ent) {
        match inst.kind {
            mir::Kind::Call(func, args) => {
                if self.t_functions.ents[func].kind == typec::func::Kind::Builtin {
                    self.generate_native_call(func, args, inst.value);
                    return;
                }

                let ir_inst = {
                    let func_ref = {
                        let ir_func = self.function_lookup[func].unwrap();
                        self.module.declare_func_in_func(ir_func, self.builder.func)
                    };
                    
                    let args: Vec<_> = self.source
                        .values(args)
                        .iter()
                        .map(|&value| self.value_lookup[value].unwrap())
                        .collect();
                    
                    self.builder.ins().call(func_ref, &args)
                };
                
                self.builder.inst_results(ir_inst).get(0).map(|&value| {
                    self.value_lookup[inst.value.unwrap()] = value.into();
                });
            }
            mir::Kind::IntLit(literal) => {
                let value = inst.value.unwrap();
                let repr = self.source.values[value].repr;
                let ir_value = self.builder.ins().iconst(repr, literal as i64);
                self.value_lookup[value] = ir_value.into();
            }
            mir::Kind::Return => {
                if let Some(value) = inst.value.expand() {
                    let ir_value = self.value_lookup[value];
                    self.builder.ins().return_(&[ir_value.unwrap()]);
                } else {
                    self.builder.ins().return_(&[]);
                }
            }
            mir::Kind::JumpIfFalse(block) => {
                let block = self.block_lookup[block].unwrap();
                let value = self.value_lookup[inst.value.unwrap()];
                self.builder.ins().brz(value.unwrap(), block, &[]);
            },
            mir::Kind::Jump(block) => {
                let block = self.block_lookup[block].unwrap();
                if let Some(value) = inst.value.expand() {
                    let value = self.value_lookup[value];
                    self.builder.ins().jump(block, &[value.unwrap()]);
                } else {
                    self.builder.ins().jump(block, &[]);
                }
            },
            mir::Kind::BoolLit(literal) => {
                let value = inst.value.unwrap();
                let repr = self.source.values[value].repr;
                let ir_value = self.builder.ins().bconst(repr, literal);
                self.value_lookup[value] = ir_value.into();
            },
        }
    }

    fn generate_native_call(&mut self, func: Func, args: EntityList<mir::Value>, result: PackedOption<mir::Value>) {
        let name = self.t_functions.ents[func].name;
        let str = self.sources.display(name);

        match str {
            "+" => self.generate_native_add(args, result),
            "-" => self.generate_native_sub(args, result),
            "*" => self.generate_native_mul(args, result),
            "/" => self.generate_native_div(args, result),
            _ => todo!("Unhandled native function: {:?}", str),
        }
    }

    fn generate_native_add(&mut self, args: EntityList<mir::Value>, result: PackedOption<mir::Value>) {
        match self.source.values(args) {
            &[value] => {
                self.value_lookup[result.unwrap()] = self.value_lookup[value];
            }
            &[left, right] => {
                let left = self.value_lookup[left].unwrap();
                let right = self.value_lookup[right].unwrap();
                
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
        match self.source.values(args) {
            &[value] => {
                let value = self.value_lookup[value].unwrap();
                let ty = self.builder.func.dfg.value_type(value);

                let neg = if ty.is_int() {
                    self.builder.ins().ineg(value)
                } else {
                    todo!("Unimplemented subtraction for {}", ty);
                };

                self.value_lookup[result.unwrap()] = neg.into();
            }
            &[left, right] => {
                let left = self.value_lookup[left].unwrap();
                let right = self.value_lookup[right].unwrap();
                
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
        match self.source.values(args) {
            &[left, right] => {
                let left = self.value_lookup[left].unwrap();
                let right = self.value_lookup[right].unwrap();
                
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
        match self.source.values(args) {
            &[left, right] => {
                let signed = self.source.values[left].flags.is_signed();
                let left = self.value_lookup[left].unwrap();
                let right = self.value_lookup[right].unwrap();
                
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
}
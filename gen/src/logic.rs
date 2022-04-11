use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{InstBuilder, Signature};
use cranelift_codegen::{ir, packed_option::PackedOption};
use cranelift_entity::{EntityList, EntityRef, SecondaryMap};
use cranelift_frontend::{FunctionBuilder, Variable};
use cranelift_module::{FuncId, Module};
use instance::mir;
use lexer::{Sources, SourcesExt};
use typec::tir::LinkedList;
use typec::Func;

pub struct Generator<'a> {
    pub module: &'a mut dyn Module,
    pub builder: &'a mut FunctionBuilder<'a>,
    pub value_lookup: &'a mut SecondaryMap<mir::Value, PackedOption<ir::Value>>,
    pub function_lookup: &'a SecondaryMap<Func, PackedOption<FuncId>>,
    pub block_lookup: &'a mut SecondaryMap<mir::Block, PackedOption<ir::Block>>,
    pub t_functions: &'a typec::Funcs,
    pub source: &'a instance::Function,
    pub sources: &'a Sources,
}

impl<'a> Generator<'a> {
    pub fn generate(&mut self) {
        Self::transfer_signature(&self.source.signature, &mut self.builder.func.signature);

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

                    let args: Vec<_> = self
                        .source
                        .values(args)
                        .iter()
                        .map(|&value| self.use_value(value))
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
                    let ir_value = self.use_value(value);
                    self.builder.ins().return_(&[ir_value]);
                } else {
                    self.builder.ins().return_(&[]);
                }
            }
            mir::Kind::JumpIfFalse(block) => {
                let block = self.block_lookup[block].unwrap();
                let value = self.use_value(inst.value.unwrap());
                self.builder.ins().brz(value, block, &[]);
            }
            mir::Kind::Jump(block) => {
                let block = self.block_lookup[block].unwrap();
                if let Some(value) = inst.value.expand() {
                    let value = self.use_value(value);
                    self.builder.ins().jump(block, &[value]);
                } else {
                    self.builder.ins().jump(block, &[]);
                }
            }
            mir::Kind::BoolLit(literal) => {
                let value = inst.value.unwrap();
                let repr = self.source.values[value].repr;
                let ir_value = self.builder.ins().bconst(repr, literal);
                self.value_lookup[value] = ir_value.into();
            }
            mir::Kind::Assign(left) => {
                let left_value = self.source.values[left];
                if left_value.flags.is_pointer() {
                    todo!();
                } else {
                    let variable = Variable::new(left.index());
                    let right = self.use_value(inst.value.unwrap());
                    self.builder.def_var(variable, right);
                }
            }
            mir::Kind::Variable => {
                let value = inst.value.unwrap();
                let repr = self.source.values[value].repr;
                let flags = self.source.values[value].flags;
                if flags.is_mutable() {
                    let variable = Variable::new(value.index());
                    self.builder.declare_var(variable, repr);
                    let value = self.value_lookup[value].unwrap();
                    self.builder.def_var(variable, value);
                }
            }
            mir::Kind::Copy(target) => {
                let target_value = self.source.values[target];
                if target_value.flags.is_pointer() {
                    todo!();
                } else {
                    let target = self.use_value(target);
                    let copy = self.builder.ins().copy(target);
                    self.value_lookup[inst.value.unwrap()] = copy.into();
                }
            }
        }
    }

    fn generate_native_call(
        &mut self,
        func: Func,
        args: EntityList<mir::Value>,
        result: PackedOption<mir::Value>,
    ) {
        let name = self.t_functions.ents[func].name;
        let str = self.sources.display(name);

        match str {
            "+" => self.generate_native_add(args, result),
            "-" => self.generate_native_sub(args, result),
            "*" => self.generate_native_mul(args, result),
            "/" => self.generate_native_div(args, result),
            "<" | ">" | "<=" | ">=" | "==" | "!=" => self.generate_native_cmp(str, args, result),
            _ => todo!("Unhandled native function: {:?}", str),
        }
    }

    fn generate_native_add(
        &mut self,
        args: EntityList<mir::Value>,
        result: PackedOption<mir::Value>,
    ) {
        match self.source.values(args) {
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

    fn generate_native_sub(
        &mut self,
        args: EntityList<mir::Value>,
        result: PackedOption<mir::Value>,
    ) {
        match self.source.values(args) {
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

    fn generate_native_mul(
        &mut self,
        args: EntityList<mir::Value>,
        result: PackedOption<mir::Value>,
    ) {
        match self.source.values(args) {
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

    fn generate_native_div(
        &mut self,
        args: EntityList<mir::Value>,
        result: PackedOption<mir::Value>,
    ) {
        match self.source.values(args) {
            &[left, right] => {
                let signed = self.source.values[left].flags.is_signed();
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

    fn generate_native_cmp(
        &mut self,
        op: &str,
        args: EntityList<mir::Value>,
        result: PackedOption<mir::Value>,
    ) {
        let &[left, right] = self.source.values(args) else {
            unreachable!();
        };

        let signed = self.source.values[left].flags.is_signed();

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
        if self.source.values[value].flags.is_mutable() {
            let variable = Variable::new(value.index());
            self.builder.use_var(variable)
        } else {
            self.value_lookup[value].unwrap()
        }
    }
}

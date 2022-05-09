#![feature(explicit_generic_args_with_impl_trait)]
#![feature(toowned_clone_into)]
#![feature(let_else)]

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{InstBuilder, MemFlags, Signature, StackSlotData};
use cranelift_codegen::{ir, packed_option::PackedOption};
use cranelift_frontend::{FunctionBuilder, Variable};
use cranelift_module::{FuncId, Linkage, Module};
use instance_types::*;
use storage::*;
use typec_types::*;
use lexer_types::*;

pub struct CirBuilder<'a> {
    pub module: &'a mut dyn Module,
    pub builder: &'a mut FunctionBuilder<'a>,
    pub value_lookup: &'a mut SecondaryMap<mir::Value, PackedOption<ir::Value>>,
    pub function_lookup: &'a SecondaryMap<Func, PackedOption<FuncId>>,
    pub block_lookup: &'a mut SecondaryMap<mir::Block, PackedOption<ir::Block>>,
    pub stack_slot_lookup: &'a mut SecondaryMap<mir::StackSlot, PackedOption<ir::StackSlot>>,
    pub variable_set: &'a mut EntitySet<Variable>,
    pub t_funcs: &'a Funcs,
    pub reprs: &'a Reprs,
    pub types: &'a Types,
    pub ty_lists: &'a TyLists,
    pub source: &'a FuncCtx,
    pub sources: &'a Sources,
}

impl<'a> CirBuilder<'a> {
    pub fn generate(&mut self) {
        Self::transfer_signature(&self.source.sig, &mut self.builder.func.signature);

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
            let repr = self.reprs[ent.ty].repr;
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
            InstKind::BitCast(value) => {
                let ir_value = self.use_value(value);
                let target_value = inst.value.unwrap();
                let repr = self.repr_of(target_value);
                let ir_value = self.builder.ins().bitcast(repr, ir_value);
                self.value_lookup[target_value] = ir_value.into();
            }
            InstKind::Call(func, args) => {
                if self.t_funcs[func].kind == TFuncKind::Builtin {
                    self.generate_native_call(func, args, inst.value);
                    return;
                }

                let ir_inst = {
                    let func_ref = {
                        // println!("{:?}", self.sources.display(self.t_funcs[func].name));
                        let ir_func = self.function_lookup[func].unwrap();
                        self.module.declare_func_in_func(ir_func, self.builder.func)
                    };

                    let args: Vec<_> = self
                        .source
                        .value_slices
                        .get(args)
                        .iter()
                        .map(|&value| {
                            // dbg!(self.source.values[value].ty, self.reprs[self.source.values[value].ty], self.reprs[self.source.values[value].ty].repr);
                            self.use_value(value)
                        })
                        .collect();

                    self.builder.ins().call(func_ref, &args)
                };

                if let Some(value) = inst.value.expand() {
                    if !self.source.values[value]
                        .flags
                        .contains(mir::MirFlags::POINTER)
                    {
                        let ret = self.builder.func.dfg.inst_results(ir_inst)[0];
                        self.value_lookup[inst.value.unwrap()] = ret.into();
                    }
                }
            }
            InstKind::IntLit(literal) => {
                let value = inst.value.unwrap();
                let repr = self.repr_of(value);
                // println!("{:?} {}", repr, literal);
                let ir_value = self.builder.ins().iconst(repr, literal as i64);
                self.value_lookup[value] = ir_value.into();
            }
            InstKind::Return => {
                if let Some(value) = inst.value.expand() {
                    let ir_value = self.use_value(value);
                    self.builder.ins().return_(&[ir_value]);
                } else {
                    self.builder.ins().return_(&[]);
                }
            }
            InstKind::JumpIfFalse(block) => {
                let block = self.block_lookup[block].unwrap();
                let value = self.use_value(inst.value.unwrap());
                self.builder.ins().brz(value, block, &[]);
            }
            InstKind::Jump(block) => {
                let block = self.block_lookup[block].unwrap();
                if let Some(value) = inst.value.expand() {
                    let value = self.use_value(value);
                    self.builder.ins().jump(block, &[value]);
                } else {
                    self.builder.ins().jump(block, &[]);
                }
            }
            InstKind::BoolLit(literal) => {
                let value = inst.value.unwrap();
                let repr = self.repr_of(value);
                let ir_value = self.builder.ins().bconst(repr, literal);
                self.value_lookup[value] = ir_value.into();
            }
            InstKind::Variable => {
                let value = inst.value.unwrap();
                let repr = self.repr_of(value);
                let flags = self.source.values[value].flags;
                if flags.contains(mir::MirFlags::ASSIGNABLE) {
                    let variable = Variable::new(value.index());
                    self.variable_set.insert(variable);
                    self.builder.declare_var(variable, repr);
                    let value = self.value_lookup[value].unwrap();
                    self.builder.def_var(variable, value);
                }
            }
            InstKind::DerefPointer(value)
                if self.source.values[value]
                    .flags
                    .contains(mir::MirFlags::POINTER) =>
            {
                let value = self.use_value(value);
                self.value_lookup[inst.value.unwrap()] = value.into();
            }
            InstKind::Offset(value)
            | InstKind::TakePointer(value)
            | InstKind::DerefPointer(value) => {
                self.value_lookup[inst.value.unwrap()] = self.value_lookup[value].unwrap().into();
                // check
            }
            InstKind::Assign(value) => {
                let target = inst.value.unwrap();

                let (size, dest_align) = {
                    let target_ty = self.source.values[target].ty;
                    let size = self.reprs[target_ty].size;
                    let align = self.reprs[target_ty].align;
                    (self.unwrap_size(size), self.unwrap_size(align))
                };

                let src_align = {
                    let value_ty = self.source.values[value].ty;
                    let align = self.reprs[value_ty].align;
                    self.unwrap_size(align)
                };

                match (
                    self.source.values[value]
                        .flags
                        .contains(mir::MirFlags::POINTER),
                    self.source.values[target]
                        .flags
                        .contains(mir::MirFlags::POINTER),
                ) {
                    (true, true) => {
                        let target_ir = self.use_value_as_pointer(target);
                        let value_ir = self.use_value_as_pointer(value);
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
                    }
                    (true, false) => {
                        let value = self.use_value(value);
                        self.assign_value(target, value);
                    }
                    (false, true) => {
                        let offset = {
                            let offset = self.source.values[target].offset;
                            self.unwrap_size(offset)
                        };
                        let repr = self.repr_of(value);
                        let target = self.use_value_as_pointer(target);
                        let value = {
                            let value = self.use_value(value);
                            if repr == ir::types::B1 {
                                self.builder.ins().bint(ir::types::I8, value)
                            } else {
                                value
                            }
                        };
                        self.builder
                            .ins()
                            .store(MemFlags::new(), value, target, offset as i32);
                    }
                    (false, false) => {
                        let value = self.use_value(value);
                        self.assign_value(target, value);
                    }
                }
            }
            InstKind::StackAddr(stack) => {
                let value = inst.value.unwrap();
                let stack = self.stack_slot_lookup[stack].unwrap();
                let _offset = {
                    let offset = self.source.values[value].offset;
                    self.unwrap_size(offset)
                };
                let addr =
                    self.builder
                        .ins()
                        .stack_addr(self.module.isa().pointer_type(), stack, 0);
                self.value_lookup[value] = addr.into();
            }
        }
    }

    fn unwrap_size(&self, size: Offset) -> i32 {
        size.arch(self.module.isa().pointer_bytes() == 4)
    }

    fn repr_of(&self, value: mir::Value) -> ir::types::Type {
        let ty = self.source.values[value].ty;
        self.reprs[ty].repr
    }

    fn generate_native_call(
        &mut self,
        func: Func,
        args: ValueList,
        result: PackedOption<mir::Value>,
    ) {
        let name = self.t_funcs[func].name;
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

    fn generate_native_add(&mut self, args: ValueList, result: PackedOption<mir::Value>) {
        match self.source.value_slices.get(args) {
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

    fn generate_native_sub(&mut self, args: ValueList, result: PackedOption<mir::Value>) {
        match self.source.value_slices.get(args) {
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

    fn generate_native_mul(&mut self, args: ValueList, result: PackedOption<mir::Value>) {
        match self.source.value_slices.get(args) {
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

    fn generate_native_div(&mut self, args: ValueList, result: PackedOption<mir::Value>) {
        match self.source.value_slices.get(args) {
            &[left, right] => {
                let signed = !self.source.values[left]
                    .flags
                    .contains(mir::MirFlags::UNSIGNED);
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

    fn generate_native_cmp(&mut self, op: &str, args: ValueList, result: PackedOption<mir::Value>) {
        let &[left, right] = self.source.value_slices.get(args) else {
            unreachable!();
        };

        let signed = !self.source.values[left]
            .flags
            .contains(mir::MirFlags::UNSIGNED);

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

    fn assign_value(&mut self, target: mir::Value, value: ir::Value) {
        let mir::ValueEnt { flags, offset, .. } = self.source.values[target];
        let offset = self.unwrap_size(offset);
        if flags.contains(mir::MirFlags::ASSIGNABLE) {
            let variable = Variable::new(target.index());
            let target_ir = self.builder.use_var(variable);
            let value = self.set_bit_field(target_ir, value, offset);
            self.builder.def_var(variable, value);
        } else {
            let target_ir = self.use_value(target);
            let value = self.set_bit_field(target_ir, value, offset);
            self.value_lookup[target] = value.into();
        }
    }

    fn set_bit_field(
        &mut self,
        mut target: ir::Value,
        mut value: ir::Value,
        offset: i32,
    ) -> ir::Value {
        let target_ty = self.builder.func.dfg.value_type(target);
        let value_ty = self.builder.func.dfg.value_type(value);

        if target_ty == value_ty {
            return value;
        }

        if value_ty == ir::types::B1 {
            value = self.builder.ins().bint(ir::types::I8, value);
        }

        if self.builder.func.dfg.value_type(value) != target_ty {
            value = self.builder.ins().uextend(target_ty, value);
        }

        let mask = {
            let value_bits = value_ty.as_int().bits();
            ((1 << value_bits) - 1) << offset * 8
        };

        target = self.builder.ins().band_imm(target, !mask);
        value = self.builder.ins().ishl_imm(value, offset as i64 * 8);

        self.builder.ins().bor(target, value)
    }

    fn use_value(&mut self, value: mir::Value) -> ir::Value {
        self.use_value_low(value, false)
    }

    fn use_value_as_pointer(&mut self, value: mir::Value) -> ir::Value {
        self.use_value_low(value, true)
    }

    fn use_value_low(&mut self, value: mir::Value, as_pointer: bool) -> ir::Value {
        let mir::ValueEnt {
            ty, flags, offset, ..
        } = self.source.values[value];
        let repr = self.reprs[ty].repr;
        let variable = Variable::new(value.index());
        let value =
            if flags.contains(mir::MirFlags::ASSIGNABLE) && self.variable_set.contains(variable) {
                self.builder.use_var(variable)
            } else {
                self.value_lookup[value].unwrap()
            };

        let offset = self.unwrap_size(offset);
        if flags.contains(mir::MirFlags::POINTER) {
            let loader_repr = repr.as_int();

            if self.reprs[ty].flags.contains(ReprFlags::ON_STACK) || as_pointer {
                value
            } else {
                let value = self
                    .builder
                    .ins()
                    .load(loader_repr, MemFlags::new(), value, offset);
                if repr == ir::types::B1 {
                    self.builder.ins().icmp_imm(IntCC::NotEqual, value, 0)
                } else {
                    value
                }
            }
        } else {
            assert!(!as_pointer);
            self.read_bit_field(value, offset, repr)
        }
    }

    fn read_bit_field(&mut self, mut value: ir::Value, offset: i32, repr: ir::Type) -> ir::Value {
        if self.builder.func.dfg.value_type(value) == repr {
            return value;
        }
        // dbg!(repr, self.builder.func.dfg.value_type(value));
        let load_repr = repr.as_int();

        let mask = ((1 << load_repr.bits()) - 1) << offset * 8;
        value = self.builder.ins().band_imm(value, mask);
        value = self.builder.ins().ushr_imm(value, offset as i64 * 8);
        if repr == ir::types::B1 {
            value = self.builder.ins().icmp_imm(IntCC::NotEqual, value, 0);
        }
        value
    }
}

/// returns none if function should not even be linked
pub fn func_linkage(kind: TFuncKind) -> Option<Linkage> {
    use TFuncKind::*;
    Some(match kind {
        Local | Owned(..) | Instance(..) => Linkage::Export,
        Builtin | Bound(..) => return None,
        External => Linkage::Import,
    })
}

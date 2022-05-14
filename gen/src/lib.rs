#![feature(explicit_generic_args_with_impl_trait)]
#![feature(toowned_clone_into)]
#![feature(let_else)]

use std::sync::Arc;

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{InstBuilder, MemFlags, Signature, StackSlotData, FuncRef, ExtFuncData, ExternalName};
use cranelift_codegen::isa::{TargetIsa, CallConv};
use cranelift_codegen::{ir, packed_option::PackedOption};
use cranelift_frontend::{FunctionBuilder, Variable};
use cranelift_module::{FuncId, Linkage, Module};
use instance_types::*;
use lexer_types::*;
use storage::*;
use typec_types::*;

pub struct CirBuilderContext {
    tir_mapping: SecondaryMap<Tir, PackedOption<Value>>,
    used_func_lookup: SecondaryMap<Func, PackedOption<FuncRef>>,
    used_funcs: Vec<Func>,
    value_lookup: SecondaryMap<mir::Value, PackedOption<ir::Value>>,
    block_lookup: SecondaryMap<mir::Block, PackedOption<ir::Block>>,
    stack_slot_lookup: SecondaryMap<mir::StackSlot, PackedOption<ir::StackSlot>>,
    variable_set: EntitySet<Variable>,
}

impl CirBuilderContext {
    pub fn new() -> Self {
        Self {
            tir_mapping: SecondaryMap::new(),
            used_func_lookup: SecondaryMap::new(),
            used_funcs: Vec::new(),
            value_lookup: SecondaryMap::new(),
            block_lookup: SecondaryMap::new(),
            stack_slot_lookup: SecondaryMap::new(),
            variable_set: EntitySet::new(),
        }
    }

    pub fn clear(&mut self) {
        self.tir_mapping.clear();
        
        for func in self.used_funcs.drain(..) {
            self.used_func_lookup[func] = PackedOption::default();
        }

        self.value_lookup.clear();
        self.block_lookup.clear();
        self.stack_slot_lookup.clear();
        self.variable_set.clear();
    }
}

pub struct CirBuilder<'a> {
    pub isa: &'a dyn TargetIsa,
    pub builder: &'a mut FunctionBuilder<'a>,
    pub ctx: &'a mut CirBuilderContext,
    pub t_funcs: &'a Funcs,
    pub reprs: &'a Reprs,
    pub types: &'a Types,
    pub ty_lists: &'a TyLists,
    pub source: &'a FuncCtx,
    pub sources: &'a Sources,
}

impl<'a> CirBuilder<'a> {
    pub fn generate(&mut self) {
        for (id, stack) in self.source.stacks.iter() {
            let slot = StackSlotData::new(ir::StackSlotKind::ExplicitSlot, stack.size);
            let slot = self.builder.create_stack_slot(slot);
            self.ctx.stack_slot_lookup[id] = slot.into();
        }

        for (id, _) in self.source.blocks() {
            let block = self.builder.create_block();
            self.ctx.block_lookup[id] = block.into();
        }

        for (id, _) in self.source.blocks() {
            self.generate_block(id);
        }

        self.builder.seal_all_blocks();
        self.builder.finalize();
    }

    fn generate_block(&mut self, id: mir::Block) {
        let block = self.ctx.block_lookup[id].unwrap();
        self.builder.switch_to_block(block);

        for (value, ent) in self.source.block_params(id) {
            let repr = self.reprs[ent.ty].repr;
            let ir_value = self.builder.append_block_param(block, repr);
            self.ctx.value_lookup[value] = ir_value.into();
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
                self.ctx.value_lookup[target_value] = ir_value.into();
            }
            InstKind::Call(func, args) => {
                if self.t_funcs[func].kind == TFuncKind::Builtin {
                    self.generate_native_call(func, args, inst.value);
                    return;
                }

                let ir_inst = {
                    let value_args: Vec<_> = self
                        .source
                        .value_slices
                        .get(args)
                        .iter()
                        .map(|&value| {
                            // dbg!(self.source.values[value].ty, self.reprs[self.source.values[value].ty], self.reprs[self.source.values[value].ty].repr);
                            self.use_value(value)
                        })
                        .collect();
                    
                    let func_ref = if let Some(func_ref) = self.ctx.used_func_lookup[func].expand() {
                        func_ref
                    } else {
                        let signature = {
                            let ret = self.source.values[inst.value.unwrap()].ty;
                            let call_conv = self.t_funcs[func].sig.call_conv;
                            let signature = translate_signature(
                                call_conv,
                                self.source.value_slices
                                    .get(args)
                                    .iter()
                                    .map(|&value| self.source.values[value].ty), 
                                ret, 
                                self.reprs, 
                                self.types, 
                                self.isa.default_call_conv(), 
                            );
                            self.builder.func.import_signature(signature)
                        };

                        let ext_func_data = ExtFuncData {
                            name: ExternalName::user(0, func.as_u32()),
                            signature,
                            colocated: !self.t_funcs[func].flags.contains(TFuncFlags::EXTERNAL),
                        };
                        let func_ref = self.builder.func.import_function(ext_func_data);
                        self.ctx.used_func_lookup[func] = func_ref.into();
                        self.ctx.used_funcs.push(func);

                        func_ref
                    };

                    self.builder.ins().call(func_ref, &value_args)
                };

                if let Some(value) = inst.value.expand() {
                    if !self.source.values[value]
                        .flags
                        .contains(mir::MirFlags::POINTER)
                    {
                        let ret = self.builder.func.dfg.inst_results(ir_inst)[0];
                        self.ctx.value_lookup[inst.value.unwrap()] = ret.into();
                    }
                }
            }
            InstKind::IntLit(literal) => {
                let value = inst.value.unwrap();
                let repr = self.repr_of(value);
                // println!("{:?} {}", repr, literal);
                let ir_value = self.builder.ins().iconst(repr, literal as i64);
                self.ctx.value_lookup[value] = ir_value.into();
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
                let block = self.ctx.block_lookup[block].unwrap();
                let value = self.use_value(inst.value.unwrap());
                self.builder.ins().brz(value, block, &[]);
            }
            InstKind::Jump(block) => {
                let block = self.ctx.block_lookup[block].unwrap();
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
                self.ctx.value_lookup[value] = ir_value.into();
            }
            InstKind::Variable => {
                let value = inst.value.unwrap();
                let repr = self.repr_of(value);
                let flags = self.source.values[value].flags;
                if flags.contains(mir::MirFlags::ASSIGNABLE) {
                    let variable = Variable::new(value.index());
                    self.ctx.variable_set.insert(variable);
                    self.builder.declare_var(variable, repr);
                    let value = self.ctx.value_lookup[value].unwrap();
                    self.builder.def_var(variable, value);
                }
            }
            InstKind::DerefPointer(value)
                if self.source.values[value]
                    .flags
                    .contains(mir::MirFlags::POINTER) =>
            {
                let value = self.use_value(value);
                self.ctx.value_lookup[inst.value.unwrap()] = value.into();
            }
            InstKind::Offset(value)
            | InstKind::TakePointer(value)
            | InstKind::DerefPointer(value) => {
                self.ctx.value_lookup[inst.value.unwrap()] = self.ctx.value_lookup[value].unwrap().into();
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
                            self.isa.frontend_config(),
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
                let stack = self.ctx.stack_slot_lookup[stack].unwrap();
                let _offset = {
                    let offset = self.source.values[value].offset;
                    self.unwrap_size(offset)
                };
                let addr =
                    self.builder
                        .ins()
                        .stack_addr(self.isa.pointer_type(), stack, 0);
                self.ctx.value_lookup[value] = addr.into();
            }
        }
    }

    fn unwrap_size(&self, size: Offset) -> i32 {
        size.arch(self.isa.pointer_bytes() == 4)
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
                self.ctx.value_lookup[result.unwrap()] = self.use_value(value).into();
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

                self.ctx.value_lookup[result.unwrap()] = add.into();
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

                self.ctx.value_lookup[result.unwrap()] = neg.into();
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

                self.ctx.value_lookup[result.unwrap()] = sub.into();
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

                self.ctx.value_lookup[result.unwrap()] = mul.into();
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

                self.ctx.value_lookup[result.unwrap()] = div.into();
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
            self.ctx.value_lookup[result] = value.into();
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
            self.ctx.value_lookup[target] = value.into();
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
            if flags.contains(mir::MirFlags::ASSIGNABLE) && self.ctx.variable_set.contains(variable) {
                self.builder.use_var(variable)
            } else {
                self.ctx.value_lookup[value].unwrap()
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

pub fn translate_signature(
    call_conv: Option<CallConv>,
    args: impl Iterator<Item = Ty>,
    ret: Ty,
    reprs: &Reprs,
    types: &Types,
    system_call_convention: CallConv,
) -> Signature {
    let mut sig = Signature::new(call_conv.unwrap_or(system_call_convention));

    if TyKind::Nothing != types[ret].kind {
        let repr::ReprEnt { repr, flags, .. } = reprs[ret];
        if flags.contains(repr::ReprFlags::ON_STACK) {
            let ret = ir::AbiParam::special(repr, ir::ArgumentPurpose::StructReturn);
            sig.params.push(ret);
            sig.returns.push(ret);
        } else {
            sig.returns.push(ir::AbiParam::new(repr));
        }
    }

    sig.params.extend(
        args
            .map(|ty| reprs[ty].repr)
            .map(|repr| ir::AbiParam::new(repr)),
    );

    sig
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

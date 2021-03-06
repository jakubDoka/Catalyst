#![feature(explicit_generic_args_with_impl_trait)]
#![feature(let_else)]
#![feature(let_chains)]

pub mod state;

use std::cmp::Ordering;

use cranelift_codegen::ir::immediates::Imm64;
pub use state::CirBuilder;

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{
    ExtFuncData, ExternalName, FuncRef, GlobalValueData, InstBuilder, MemFlags, Signature,
    StackSlotData, StackSlotKind,
};
use cranelift_codegen::isa::{CallConv, TargetIsa};
use cranelift_codegen::{ir, packed_option::PackedOption};
use cranelift_frontend::{FunctionBuilder, Variable};
use cranelift_module::Linkage;

use incr::*;
use instance_types::*;
use lexer::*;
use matching::*;
use storage::*;
use typec_types::*;

pub type Signatures = SparseMap<Func, Signature>;

pub struct CirBuilderContext {
    tir_mapping: SecondaryMap<Tir, PackedOption<Value>>,
    used_func_lookup: SecondaryMap<Func, PackedOption<FuncRef>>,
    used_funcs: Vec<Func>,
    value_lookup: SecondaryMap<mir::Value, PackedOption<ir::Value>>,
    block_lookup: SecondaryMap<mir::Block, PackedOption<ir::Block>>,
    stack_slot_lookup: SecondaryMap<mir::StackSlot, PackedOption<ir::StackSlot>>,
    variable_set: EntitySet<Variable>,
    used_data_lookup: SecondaryMap<Global, PackedOption<ir::GlobalValue>>,
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
            used_data_lookup: SecondaryMap::new(),
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
        self.used_data_lookup.clear();
    }
}

impl CirBuilder<'_> {
    pub fn generate(&mut self, entry: bool, span: Span) {
        self.cir_builder_context.clear();

        for (id, stack) in self.func_ctx.stacks.iter() {
            let slot = StackSlotData::new(ir::StackSlotKind::ExplicitSlot, stack.size);
            let slot = self.builder.create_stack_slot(slot);
            self.cir_builder_context.stack_slot_lookup[id] = slot.into();
        }

        for (id, _) in self.func_ctx.blocks() {
            let block = self.builder.create_block();
            self.cir_builder_context.block_lookup[id] = block.into();
        }

        for (id, _) in self.func_ctx.blocks() {
            self.generate_block(id, entry, span);
        }

        self.builder.seal_all_blocks();
        self.builder.finalize();

        // for (k, &v) in self.cir_builder_context.value_lookup.iter() {
        //     if let Some(v) = v.expand() {
        //         println!("{} -> {}", k, v);
        //     }
        // }
    }

    fn generate_block(&mut self, id: mir::Block, entry: bool, span: Span) {
        let block = self.cir_builder_context.block_lookup[id].unwrap();
        self.builder.switch_to_block(block);
        
        for (value, ent) in self.func_ctx.block_params(id) {
            let repr = self.reprs[ent.ty].repr;
            let ir_value = self.builder.append_block_param(block, repr);
            self.cir_builder_context.value_lookup[value] = ir_value.into();
        }

        if self.stack_frame.is_none() {
            self.create_stack_trace(entry, span);
        }

        for (_, inst) in self
            .func_ctx
            .insts
            .linked_iter(self.func_ctx.blocks[id].start.expand())
        {
            self.generate_inst(inst);
        }
    }

    fn generate_inst(&mut self, inst: &mir::InstEnt) {
        match inst.kind {
            InstKind::Uninit => {
                todo!();
            }
            InstKind::IndirectCall(func, mir_args) => {
                let TyKind::FuncPtr(Sig { cc, .. }) = self.types[self.func_ctx.values[func].ty].kind else {
                    unreachable!();
                };

                let ir_inst = {
                    let value_args = self.vec_pool.alloc_iter(
                        self.func_ctx
                            .value_slices
                            .get(mir_args)
                            .iter()
                            .map(|&value| self.use_value(value)),
                    );

                    let ret = inst
                        .value
                        .map(|val| self.func_ctx.values[val].ty)
                        .unwrap_or(self.builtin_types.nothing);

                    let args_iter = self
                        .func_ctx
                        .value_slices
                        .get(mir_args)
                        .iter()
                        .skip(self.reprs[ret].flags.contains(ReprFlags::ON_STACK) as usize)
                        .map(|&value| self.func_ctx.values[value].ty);

                    let func_sig = translate_signature(
                        cc,
                        args_iter,
                        ret,
                        self.reprs,
                        self.types,
                        self.isa.default_call_conv(),
                    );
                    let sig_ref = self.builder.func.import_signature(func_sig);
                    let func = self.use_value(func);
                    self.builder.ins().call_indirect(sig_ref, func, &value_args)
                };

                if let Some(value) = inst.value.expand() {
                    if !self.func_ctx.values[value]
                        .flags
                        .contains(MirFlags::POINTER)
                    {
                        let ret = self.builder.func.dfg.inst_results(ir_inst)[0];
                        self.cir_builder_context.value_lookup[inst.value.unwrap()] = ret.into();
                    }
                }
            }
            InstKind::FuncPtr(func) => {
                let value = inst.value.unwrap();

                let TyKind::FuncPtr(Sig { args, ret, .. }) = self.types[self.func_ctx.values[value].ty].kind else {
                    unreachable!();
                };

                let args_iter = self.ty_comps.get(args).iter().map(|arg| arg.ty);
                let func_ref = self.func_ref_of(func, args_iter, ret);
                let ir_value = self.builder.ins().func_addr(self.reprs[ret].repr, func_ref);
                self.cir_builder_context.value_lookup[value] = ir_value.into();
            }
            InstKind::GlobalAccess(global) => {
                let global_value =
                    if let Some(gv) = self.cir_builder_context.used_data_lookup[global].expand() {
                        gv
                    } else {
                        // apparently this is how cranelift does it in JITModule and module
                        let ext_func_data = GlobalValueData::Symbol {
                            name: ExternalName::User {
                                namespace: DATA_NAMESPACE,
                                index: global.as_u32(),
                            },
                            offset: Imm64::new(0),
                            colocated: false,
                            tls: false,
                        };
                        let func_ref = self.builder.func.create_global_value(ext_func_data);
                        self.cir_builder_context.used_data_lookup[global] = func_ref.into();
                        func_ref
                    };

                let target_value = inst.value.unwrap();
                let repr = self.repr_of(target_value);
                assert!(self.func_ctx.values[target_value]
                    .flags
                    .contains(MirFlags::POINTER));
                let value = self.builder.ins().global_value(repr, global_value);
                self.cir_builder_context.value_lookup[target_value] = value.into();
            }
            InstKind::BitCast(value) => {
                let target_value = inst.value.unwrap();
                let repr = self.repr_of(target_value);

                let ir_value = if self.func_ctx.values[target_value]
                    .flags
                    .contains(MirFlags::POINTER)
                {
                    self.use_value_as_pointer(value)
                } else {
                    let mut ir_value = self.use_value(value);
                    if repr != self.builder.func.dfg.value_type(ir_value) {
                        ir_value = self.builder.ins().bitcast(repr, ir_value);
                    }
                    ir_value
                };
                self.cir_builder_context.value_lookup[target_value] = ir_value.into();
            }
            InstKind::Call(func, params, args, span) => {
                if self.funcs[func.meta()].kind == FuncKind::Builtin {
                    self.generate_native_call(func, params, args, inst.value, span);
                    return;
                }

                self.save_stack_trace(span);

                let ir_inst =
                    {
                        let value_args = self.vec_pool.alloc_iter(
                            self.func_ctx.value_slices.get(args).iter().map(|&value| {
                                // println!("{}", ty_display!(self, self.func_ctx.values[value].ty));
                                self.use_value(value)
                            }),
                        );

                        let ret = inst
                            .value
                            .map(|val| self.func_ctx.values[val].ty)
                            .unwrap_or(self.builtin_types.nothing);

                        let args_iter = self
                            .func_ctx
                            .value_slices
                            .get(args)
                            .iter()
                            .skip(self.reprs[ret].flags.contains(ReprFlags::ON_STACK) as usize)
                            .map(|&value| self.func_ctx.values[value].ty);

                        let func_ref = self.func_ref_of(func, args_iter, ret);

                        self.builder.ins().call(func_ref, &value_args)
                    };

                if let Some(value) = inst.value.expand() {
                    if !self.func_ctx.values[value]
                        .flags
                        .contains(MirFlags::POINTER)
                    {
                        let ret = self.builder.func.dfg.inst_results(ir_inst)[0];
                        self.cir_builder_context.value_lookup[inst.value.unwrap()] = ret.into();
                    }
                }
            }
            InstKind::IntLit(literal) => {
                let value = inst.value.unwrap();
                let repr = self.repr_of(value);
                let literal = if self.types[self.func_ctx.values[value].ty]
                    .flags
                    .contains(TyFlags::SIGNED)
                {
                    i64::decode(literal)
                } else {
                    u64::decode(literal) as i64
                };
                let ir_value = self.builder.ins().iconst(repr, literal);
                self.cir_builder_context.value_lookup[value] = ir_value.into();
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
                let block = self.cir_builder_context.block_lookup[block].unwrap();
                let value = self.use_value(inst.value.unwrap());
                self.builder.ins().brz(value, block, &[]);
            }
            InstKind::Jump(block) => {
                let block = self.cir_builder_context.block_lookup[block].unwrap();
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
                self.cir_builder_context.value_lookup[value] = ir_value.into();
            }
            InstKind::Variable => {
                let value = inst.value.unwrap();
                let repr = self.repr_of(value);
                let flags = self.func_ctx.values[value].flags;
                if flags.contains(MirFlags::ASSIGNABLE) {
                    let variable = Variable::new(value.index());
                    let value = self.use_value(value);
                    self.cir_builder_context.variable_set.insert(variable);
                    self.builder.declare_var(variable, repr);
                    self.builder.def_var(variable, value);
                }
            }
            InstKind::DerefPtr(value) => {
                let value = self.use_value(value);
                self.cir_builder_context.value_lookup[inst.value.unwrap()] = value.into();
            }
            InstKind::Offset(value) | InstKind::TakePtr(value) => {
                self.cir_builder_context.value_lookup[inst.value.unwrap()] =
                    self.cir_builder_context.value_lookup[value].unwrap().into();
            }
            InstKind::Assign(value) => {
                let target = inst.value.unwrap();

                let (size, dest_align) = {
                    let target_ty = self.func_ctx.values[target].ty;
                    let size = self.reprs[target_ty].layout.size();
                    let align = self.reprs[target_ty].layout.align();
                    (self.unwrap_size(size), self.unwrap_size(align))
                };

                let src_align = {
                    let value_ty = self.func_ctx.values[value].ty;
                    let align = self.reprs[value_ty].layout.align();
                    self.unwrap_size(align)
                };

                match (
                    self.func_ctx.values[value]
                        .flags
                        .contains(MirFlags::POINTER),
                    self.func_ctx.values[target]
                        .flags
                        .contains(MirFlags::POINTER),
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
                            let offset = self.func_ctx.values[target].offset;
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
                        // print value and target types
                        // println!("{} {}", ty_display!(self, self.func_ctx.values[value].ty), ty_display!(self, self.func_ctx.values[target].ty));
                        let value = self.use_value(value);
                        self.assign_value(target, value);
                    }
                }
            }
            InstKind::StackAddr(stack) => {
                let value = inst.value.unwrap();
                let stack = self.cir_builder_context.stack_slot_lookup[stack].unwrap();
                let _offset = {
                    let offset = self.func_ctx.values[value].offset;
                    self.unwrap_size(offset)
                };
                let addr = self
                    .builder
                    .ins()
                    .stack_addr(self.isa.pointer_type(), stack, 0);
                self.cir_builder_context.value_lookup[value] = addr.into();
            }
        }
    }

    fn func_ref_of(&mut self, func: Func, args: impl Iterator<Item = Ty>, ret: Ty) -> FuncRef {
        if let Some(func_ref) = self.cir_builder_context.used_func_lookup[func].expand() {
            func_ref
        } else {
            let signature = {
                let call_conv = self.funcs[func.meta()].sig.cc;
                let signature = if let Some(sig) = self.signatures.get(func) {
                    sig.clone()
                } else {
                    let sig = translate_signature(
                        call_conv,
                        args,
                        ret,
                        self.reprs,
                        self.types,
                        self.isa.default_call_conv(),
                    );
                    self.signatures.insert(func, sig.clone());
                    sig
                };
                self.builder.func.import_signature(signature)
            };

            let ext_func_data = ExtFuncData {
                name: ExternalName::user(FUNC_NAMESPACE, func.as_u32()),
                signature,
                colocated: !self.funcs[func].flags.contains(FuncFlags::EXTERNAL),
            };
            let func_ref = self.builder.func.import_function(ext_func_data);
            self.cir_builder_context.used_func_lookup[func] = func_ref.into();
            self.cir_builder_context.used_funcs.push(func);

            func_ref
        }
    }

    fn unwrap_size(&self, size: Offset) -> i32 {
        size.arch(self.isa.pointer_bytes() == 4)
    }

    fn repr_of(&self, value: mir::Value) -> ir::types::Type {
        let ty = self.func_ctx.values[value].ty;
        self.reprs[ty].repr
    }

    fn generate_native_call(
        &mut self,
        func: Func,
        params: TyList,
        args: ValueList,
        result: PackedOption<mir::Value>,
        span: Span, 
    ) {
        let name = self.funcs[func.meta()].name;
        let str = self.sources.display(name);

        let value = match str {
            "+" => self.generate_native_add(args),
            "-" => self.generate_native_sub(args),
            "*" => self.generate_native_mul(args),
            "/" => self.generate_native_div(args),
            "%" => self.generate_native_rem(args),
            "<" | ">" | "<=" | ">=" | "==" | "!=" => self.generate_native_cmp(str, args),
            "u8" | "u16" | "u32" | "u64" | "uint" | "i8" | "i16" | "i32" | "i64" | "int" => {
                self.generate_native_int_conv(args, result)
            }
            "size_of" => self.generate_size_of(params),
            "get_stack_trace" => self.generate_get_stack_trace(span),
            _ => unimplemented!("Unhandled native function: {:?}", str),
        };

        if let Some(result) = result.expand() {
            self.cir_builder_context.value_lookup[result] = value.unwrap().into();
        }
    }

    fn generate_get_stack_trace(&mut self, span: Span) -> Option<ir::Value> {
        if self.stack_frame.is_none() {
            self.create_stack_trace(false, span);
        }
        let stack = self.stack_frame.unwrap();
        self.save_stack_trace(span);
        Some(self.builder.ins().stack_addr(self.isa.pointer_type(), stack, 0))
    }

    fn generate_size_of(&mut self, params: TyList) -> Option<ir::Value> {
        let ty = self.ty_lists.get(params)[0];
        let size = self.unwrap_size(self.reprs[ty].layout.size());
        let size = self
            .builder
            .ins()
            .iconst(self.isa.pointer_type(), size as i64);
        Some(size)
    }

    fn generate_native_int_conv(
        &mut self,
        args: ValueList,
        result: PackedOption<mir::Value>,
    ) -> Option<ir::Value> {
        let &[value] = self.func_ctx.value_slices.get(args) else {
            unreachable!();
        };

        let value_ir = self.use_value(value);
        let result = result.unwrap();

        let value_repr = self.repr_of(value);
        let result_repr = self.repr_of(result);

        let result_ir = if result_repr.is_int() {
            let value_bytes = value_repr.bytes();
            let result_bytes = result_repr.bytes();
            let signed = {
                let ty = self.func_ctx.values[value].ty;
                self.types[ty].flags.contains(TyFlags::SIGNED)
            };
            match value_bytes.cmp(&result_bytes) {
                Ordering::Less if signed => self.builder.ins().sextend(result_repr, value_ir),
                Ordering::Less => self.builder.ins().uextend(result_repr, value_ir),
                Ordering::Greater => self.builder.ins().ireduce(result_repr, value_ir),
                Ordering::Equal => value_ir,
            }
        } else {
            todo!()
        };

        Some(result_ir)
    }

    fn generate_native_add(&mut self, args: ValueList) -> Option<ir::Value> {
        match self.func_ctx.value_slices.get(args) {
            &[value] => Some(self.use_value(value)),
            &[left, right] => {
                let left = self.use_value(left);
                let right = self.use_value(right);

                let ty = self.builder.func.dfg.value_type(left);

                assert_eq!(ty, self.builder.func.dfg.value_type(right));

                let add = if ty.is_int() {
                    self.builder.ins().iadd(left, right)
                } else {
                    unimplemented!("Unimplemented addition for {}", ty);
                };

                Some(add)
            }
            _ => unreachable!(),
        }
    }

    fn generate_native_sub(&mut self, args: ValueList) -> Option<ir::Value> {
        match self.func_ctx.value_slices.get(args) {
            &[value] => {
                let value = self.use_value(value);
                let ty = self.builder.func.dfg.value_type(value);

                let neg = if ty.is_int() {
                    self.builder.ins().ineg(value)
                } else {
                    unimplemented!("Unimplemented subtraction for {}", ty);
                };

                Some(neg)
            }
            &[left, right] => {
                let left = self.use_value(left);
                let right = self.use_value(right);

                let ty = self.builder.func.dfg.value_type(left);

                assert!(ty == self.builder.func.dfg.value_type(right));

                let sub = if ty.is_int() {
                    self.builder.ins().isub(left, right)
                } else {
                    unimplemented!("Unimplemented subtraction for {}", ty);
                };

                Some(sub)
            }
            _ => unreachable!(),
        }
    }

    fn generate_native_mul(&mut self, args: ValueList) -> Option<ir::Value> {
        match self.func_ctx.value_slices.get(args) {
            &[left, right] => {
                let left = self.use_value(left);
                let right = self.use_value(right);

                let ty = self.builder.func.dfg.value_type(left);

                assert!(ty == self.builder.func.dfg.value_type(right));

                let mul = if ty.is_int() {
                    self.builder.ins().imul(left, right)
                } else {
                    unimplemented!("Unimplemented multiplication for {}", ty);
                };

                Some(mul)
            }
            _ => unreachable!(),
        }
    }

    fn generate_native_div(&mut self, args: ValueList) -> Option<ir::Value> {
        match self.func_ctx.value_slices.get(args) {
            &[left, right] => {
                let signed = self.types[self.func_ctx.values[left].ty]
                    .flags
                    .contains(TyFlags::SIGNED);
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
                    unimplemented!("Unimplemented division for {}", ty);
                };

                Some(div)
            }
            _ => unreachable!(),
        }
    }

    fn generate_native_rem(&mut self, args: ValueList) -> Option<ir::Value> {
        match self.func_ctx.value_slices.get(args) {
            &[left, right] => {
                let signed = self.types[self.func_ctx.values[left].ty]
                    .flags
                    .contains(TyFlags::SIGNED);
                let left = self.use_value(left);
                let right = self.use_value(right);

                let ty = self.builder.func.dfg.value_type(left);

                assert!(ty == self.builder.func.dfg.value_type(right));

                let rem = if ty.is_int() {
                    if signed {
                        self.builder.ins().srem(left, right)
                    } else {
                        self.builder.ins().urem(left, right)
                    }
                } else {
                    unimplemented!("Unimplemented division for {}", ty);
                };

                Some(rem)
            }
            _ => unreachable!(),
        }
    }

    fn generate_native_cmp(&mut self, op: &str, args: ValueList) -> Option<ir::Value> {
        let &[left, right] = self.func_ctx.value_slices.get(args) else {
            unreachable!();
        };

        let signed = self.types[self.func_ctx.values[left].ty]
            .flags
            .contains(TyFlags::SIGNED);

        let left = self.use_value(left);
        let right = self.use_value(right);

        let ty = self.builder.func.dfg.value_type(left);

        assert!(
            ty == self.builder.func.dfg.value_type(right),
            "{:?} != {:?}",
            ty,
            self.builder.func.dfg.value_type(right)
        );

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
            Some(value)
        } else {
            unimplemented!("Unimplemented comparison for {}", ty);
        }
    }

    fn assign_value(&mut self, target: mir::Value, value: ir::Value) {
        let mir::ValueEnt { flags, offset, .. } = self.func_ctx.values[target];
        let offset = self.unwrap_size(offset);
        let variable = Variable::new(target.index());
        if flags.contains(MirFlags::ASSIGNABLE)
            && self.cir_builder_context.variable_set.contains(variable)
        {
            let target_ir = self.builder.use_var(variable);
            let value = self.set_bit_field_low(target_ir, value, offset);
            self.builder.def_var(variable, value);
        } else {
            let value = self.set_bit_field(target, value, offset);
            self.cir_builder_context.value_lookup[target] = value.into();
        }
    }

    fn set_bit_field(&mut self, target: Value, value: ir::Value, offset: i32) -> ir::Value {
        let target_ty = self.repr_of(target);
        let value_ty = self.builder.func.dfg.value_type(value);

        if target_ty == value_ty {
            return value;
        }

        // dbg!(target);

        let target = self.use_value(target);

        self.set_bit_field_low(target, value, offset)
    }

    fn set_bit_field_low(
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

        // dbg!(value_ty, target_ty, offset);

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
        } = self.func_ctx.values[value];
        let repr = self.reprs[ty].repr;
        let variable = Variable::new(value.index());
        let v = if flags.contains(MirFlags::ASSIGNABLE)
            && self.cir_builder_context.variable_set.contains(variable)
        {
            self.builder.use_var(variable)
        } else {
            self.cir_builder_context.value_lookup[value].unwrap()
        };

        let offset = self.unwrap_size(offset);
        if flags.contains(MirFlags::POINTER) {
            // println!("{:?} {} {}", repr, ty_display!(self, ty), ty);
            let loader_repr = repr.as_int();

            if self.reprs[ty].flags.contains(ReprFlags::ON_STACK) || as_pointer {
                v
            } else {
                let value = self
                    .builder
                    .ins()
                    .load(loader_repr, MemFlags::new(), v, offset);
                if repr == ir::types::B1 {
                    self.builder.ins().icmp_imm(IntCC::NotEqual, value, 0)
                } else {
                    value
                }
            }
        } else {
            assert!(!as_pointer);
            self.read_bit_field(v, offset, repr)
        }
    }

    fn read_bit_field(&mut self, mut value: ir::Value, offset: i32, repr: ir::Type) -> ir::Value {
        if self.builder.func.dfg.value_type(value) == repr {
            return value;
        }

        // println!("{}", self.builder.func.display());
        // dbg!(repr, self.builder.func.dfg.value_type(value));
        let load_repr = repr.as_int();

        // dbg!(load_repr.bits());

        let mask = ((1 << load_repr.bits()) - 1) << offset * 8;
        value = self.builder.ins().band_imm(value, mask);
        value = self.builder.ins().ushr_imm(value, offset as i64 * 8);
        if repr == ir::types::B1 {
            value = self.builder.ins().icmp_imm(IntCC::NotEqual, value, 0);
        } else if self.builder.func.dfg.value_type(value).bits() > repr.bits() {
            value = self.builder.ins().ireduce(repr, value);
        }
        value
    }

    fn save_stack_trace(&mut self, span: Span) {
        if let Some(stack) = self.stack_frame && !span.is_reserved_value() {
            let ptr_ty = self.isa.pointer_type();
    
            let (line, col) = self.sources.line_data_of(span);

            let offset = ptr_ty.bytes() as i32;
            
            let line = self.builder.ins().iconst(ir::types::I32, line as i64);
            self.builder.ins().stack_store(line, stack, offset);

            let col = self.builder.ins().iconst(ir::types::I32, col as i64);
            self.builder.ins().stack_store(col, stack, offset * 2);

            let stack_addr = self.builder.ins().stack_addr(ptr_ty, stack, 0);
            self.builder.ins().set_pinned_reg(stack_addr);
        }
    }

    fn create_stack_trace(&mut self, entry: bool, span: Span) {
        if span.is_reserved_value() {
            return;
        }

        let ptr_ty = self.isa.pointer_type();

        let size = self.reprs[self.builtin_types.stack_trace].layout.size(); 
        let stack = self.builder.create_stack_slot(StackSlotData {
            kind: StackSlotKind::ExplicitSlot,
            size: self.unwrap_size(size) as u32,
        });
        

        let prev = if entry {
            self.builder.ins().iconst(ptr_ty, 0)
        } else {
            self.builder.ins().get_pinned_reg(ptr_ty)
        };
        self.builder.ins().stack_store(prev, stack, 0);

        let (f_start, f_end) = self.sources.file_of(span);

        let offset = ptr_ty.bytes() as i32;

        let f_start = self.builder.ins().iconst(ir::types::I32, f_start as i64);
        self.builder.ins().stack_store(f_start, stack, offset * 3);

        let f_end = self.builder.ins().iconst(ir::types::I32, f_end as i64);
        self.builder.ins().stack_store(f_end, stack, offset * 4);

            
        self.stack_frame = Some(stack);
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

    if TyKind::Struct(TyCompList::default()) != types[ret].kind {
        let repr::ReprEnt { repr, flags, .. } = reprs[ret];
        if flags.contains(repr::ReprFlags::ON_STACK) {
            let ret = ir::AbiParam::special(repr, ir::ArgumentPurpose::StructReturn);
            sig.params.push(ret);
            sig.returns.push(ret);
        } else {
            sig.returns.push(ir::AbiParam::new(repr));
        }
    }

    sig.params.extend(args.map(|ty| {
        let ReprEnt { repr, .. } = reprs[ty];
        // if flags.contains(repr::ReprFlags::ON_STACK) {
        //     ir::AbiParam::special(repr, ir::ArgumentPurpose::StructArgument(size.arch(false) as u32))
        // } else {
        // }
        ir::AbiParam::new(repr)
    }));

    sig
}

/// returns none if function should not even be linked
pub fn func_linkage(kind: FuncKind) -> Option<Linkage> {
    use FuncKind::*;
    Some(match kind {
        Local | Owned(..) => Linkage::Export,
        Builtin | Bound(..) => return None,
        External => Linkage::Import,
    })
}

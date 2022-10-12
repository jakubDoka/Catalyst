use std::{cmp::Ordering, slice};

use cranelift_codegen::ir::{
    self, condcodes::IntCC, types, InstBuilder, MemFlags, StackSlotData, StackSlotKind, Type,
};
use mir_t::*;
use storage::*;

use typec_t::*;

use crate::{context::ComputedValue, *};

mod function_loading;
mod size_calc;

impl Generator<'_> {
    pub fn generate(
        &mut self,
        signature: Signature,
        params: &[Ty],
        root: VRef<BlockMir>,
        builder: &mut GenBuilder,
    ) {
        builder.func.clear();
        self.gen_resources.clear();

        let system_cc = builder.isa.default_call_conv();
        let ptr_ty = builder.isa.pointer_ty;
        self.populate_signature(
            signature,
            params,
            &mut builder.func.signature,
            system_cc,
            ptr_ty,
        );

        self.gen_resources
            .block_stack
            .push((root, builder.create_block()));
        while let Some((block, ir_block)) = self.gen_resources.block_stack.pop() {
            self.block(block, ir_block, builder);
        }

        builder.finalize();
    }

    fn block(
        &mut self,
        block: VRef<BlockMir>,
        ir_block: ir::Block,
        builder: &mut GenBuilder,
    ) -> ir::Block {
        let BlockMir {
            args,
            insts,
            control_flow,
            ..
        } = builder.body.blocks[block];

        builder.switch_to_block(ir_block);

        for &arg in &builder.body.value_args[args] {
            let layout = self.ty_layout(builder.body.value_ty(arg), builder.ptr_ty());
            self.gen_resources.values[arg] =
                ComputedValue::Value(builder.append_block_param(ir_block, layout.repr)).into();
            self.gen_resources.must_load[arg] = layout.on_stack;
        }

        for &inst in &builder.body.insts[insts] {
            self.inst(inst, builder);
        }

        self.control_flow(control_flow, builder);

        builder.seal_block(ir_block);

        ir_block
    }

    fn inst(&mut self, inst: InstMir, builder: &mut GenBuilder) {
        match inst {
            InstMir::Int(value, ret) => {
                let ty = self.ty_repr(builder.body.value_ty(ret), builder.ptr_ty());
                let value = builder.ins().iconst(ty, value);
                self.save_value(ret, value, 0, false, builder);
            }
            InstMir::Access(target, ret) => {
                self.gen_resources.must_load[ret] = self.gen_resources.must_load[target];
                self.gen_resources.values[ret] = self.gen_resources.values[target];
                self.gen_resources.offsets[ret] = self.gen_resources.offsets[target];
            }
            InstMir::Call(call, ret) => self.call(call, ret, builder),
            InstMir::Const(id, ret) => self.r#const(id, ret, builder),
            InstMir::Ctor(fields, ret, needs_instance) => {
                self.constructor(fields, ret, needs_instance, builder)
            }
            InstMir::Deref(target, ret) => self.deref(target, ret, builder),
            InstMir::Ref(target, ret) => self.r#ref(target, ret, builder),
            InstMir::Field(header, field, ret) => self.field(header, field, ret, builder),
        };
    }

    fn deref(&mut self, target: VRef<ValueMir>, ret: VRef<ValueMir>, builder: &mut GenBuilder) {
        self.assign_value(ret, target, builder);
        self.gen_resources.must_load[ret] = true;
    }

    fn r#ref(&mut self, target: VRef<ValueMir>, ret: VRef<ValueMir>, builder: &mut GenBuilder) {
        assert!(self.gen_resources.must_load[target]);
        let ptr_ty = builder.ptr_ty();
        let addr = match self.gen_resources.values[target].unwrap() {
            ComputedValue::Value(value) => value,
            ComputedValue::Variable(var) => builder.use_var(var),
            ComputedValue::StackSlot(ss) => {
                builder
                    .ins()
                    .stack_addr(ptr_ty, ss, self.gen_resources.offsets[target])
            }
        };
        self.save_value(ret, addr, 0, false, builder);
    }

    fn field(
        &mut self,
        header: VRef<ValueMir>,
        field: u32,
        ret: VRef<ValueMir>,
        builder: &GenBuilder,
    ) {
        // field just changes offset
        let base = self.gen_resources.offsets[header];
        let header_ty = builder.body.value_ty(header);
        let offsets = self.ty_layout(header_ty, builder.ptr_ty()).offsets;
        let field_offset = self.gen_layouts.offsets[offsets][field as usize];
        self.gen_resources.offsets[ret] = base + field_offset as i32;
        self.gen_resources.values[ret] = self.gen_resources.values[header];
    }

    fn constructor(
        &mut self,
        fields: VRefSlice<ValueMir>,
        ret: VRef<ValueMir>,
        _needs_instance: bool,
        builder: &mut GenBuilder,
    ) {
        let layout = self.ty_layout(builder.body.value_ty(ret), builder.ptr_ty());

        self.ensure_target(ret, None, builder);

        let base_offset = self.gen_resources.offsets[ret];

        self.gen_layouts.offsets[layout.offsets]
            .iter()
            .zip(&builder.body.value_args[fields])
            .for_each(|(&offset, &field)| {
                self.gen_resources.offsets[field] = offset as i32 + base_offset;
                self.gen_resources.values[field] = self.gen_resources.values[ret];
                self.gen_resources.must_load[field] = self.gen_resources.must_load[ret];
            });
    }

    fn ensure_target(
        &mut self,
        target: VRef<ValueMir>,
        source_value: Option<ir::Value>,
        builder: &mut GenBuilder,
    ) -> ComputedValue {
        if let Some(value) = self.gen_resources.values[target] {
            return value;
        }

        let referenced = builder.body.is_referenced(target);

        let layout = self.ty_layout(builder.body.value_ty(target), builder.ptr_ty());
        let value = if layout.on_stack || referenced {
            self.gen_resources.must_load[target] = true;
            let ss = builder.create_sized_stack_slot(StackSlotData {
                kind: StackSlotKind::ExplicitSlot,
                size: layout.size,
            });
            ComputedValue::StackSlot(ss)
        } else {
            ComputedValue::Value(
                source_value.unwrap_or_else(|| builder.ins().iconst(layout.repr, 0)),
            )
        };
        self.gen_resources.values[target] = Some(value);

        value
    }

    fn r#const(&mut self, id: VRef<FuncConstMir>, ret: VRef<ValueMir>, builder: &mut GenBuilder) {
        if builder.isa.jit {
            // since this si already compile time, we inline the constant
            // expression

            let block_id = builder.body.constants[id].block;
            let BlockMir {
                insts,
                control_flow,
                ..
            } = builder.body.blocks[block_id];

            for &inst in &builder.body.insts[insts] {
                self.inst(inst, builder);
            }

            #[allow(irrefutable_let_patterns)]
            let ControlFlowMir::Return(final_value) = control_flow else {
                unreachable!()
            };

            if let Some(final_value) = final_value {
                self.assign_value(ret, final_value, builder)
            }
        } else {
            let value = self.gen_resources.func_constants[id]
                .expect("Constant should be computed before function compilation.");
            let ty = self.ty_repr(builder.body.value_ty(ret), builder.ptr_ty());

            let value = match value {
                GenFuncConstant::Int(val) => builder.ins().iconst(ty, val as i64),
            };

            self.save_value(ret, value, 0, false, builder);
        }
    }

    fn call(
        &mut self,
        CallMir {
            callable,
            params,
            args,
        }: CallMir,
        ret: OptVRef<ValueMir>,
        builder: &mut GenBuilder,
    ) {
        let params = builder.body.ty_params[params]
            .iter()
            .map(|&param| builder.body.dependant_types[param].ty)
            .collect::<BumpVec<_>>();

        let (func_ref, struct_ret) = match callable {
            CallableMir::Func(func_id) => {
                if self.typec.funcs[func_id].flags.contains(FuncFlags::BUILTIN) {
                    let args = builder.body.value_args[args]
                        .iter()
                        .map(|&arg| self.load_value(arg, builder))
                        .collect::<BumpVec<_>>();
                    self.builtin_call(func_id, args, ret, builder);
                    return;
                }
                self.instantiate(func_id, params.iter().copied(), builder)
            }
            CallableMir::SpecFunc(func) => {
                // TODO: I dislike how much can panic here, maybe improve this in the future
                let SpecFunc {
                    parent, generics, ..
                } = self.typec.spec_funcs[func];
                let SpecBase { methods, .. } = self.typec[parent];
                let index = self.typec.spec_funcs.local_index(methods, func);
                let generic_count = params.len() - self.typec[generics].len();
                let (upper, caller, lower) = (
                    &params[..generic_count - 1],
                    params[generic_count - 1],
                    &params[generic_count..],
                );
                let used_spec = if upper.is_empty() {
                    Spec::Base(parent)
                } else {
                    Spec::Instance(self.typec.spec_instance(parent, upper, self.interner))
                };
                let r#impl = self
                    .typec
                    .find_implementation(caller, used_spec, &[], &mut None, self.interner)
                    .unwrap()
                    .unwrap();
                let Impl {
                    generics,
                    methods,
                    key: ImplKey { ty, spec },
                    ..
                } = self.typec.impls[r#impl];
                let func_id = self.typec.func_slices[methods][index];
                let mut infer_slots = bumpvec![None; self.typec[generics].len()];
                let _ = self.typec.compatible(&mut infer_slots, caller, ty);
                let _ = self
                    .typec
                    .compatible_spec(&mut infer_slots, used_spec, spec);
                let params = infer_slots
                    .iter()
                    .map(|&slot| slot.unwrap())
                    .chain(lower.iter().copied());
                self.instantiate(func_id, params, builder)
            }
            CallableMir::Pointer(_) => todo!(),
        };

        let struct_ptr = struct_ret.then(|| {
            let ret = ret.unwrap();
            if self.gen_resources.values[ret].is_some() {
                self.load_value(ret, builder)
            } else {
                let ptr_ty = builder.ptr_ty();
                let layout = self.ty_layout(builder.body.value_ty(ret), ptr_ty);
                let stack_slot = builder.create_sized_stack_slot(StackSlotData {
                    kind: StackSlotKind::ExplicitSlot,
                    size: layout.size as u32,
                });
                builder.ins().stack_addr(ptr_ty, stack_slot, 0)
            }
        });

        let args = struct_ptr
            .into_iter()
            .chain(
                builder.body.value_args[args]
                    .iter()
                    .map(|&arg| self.load_value(arg, builder)),
            )
            .collect::<BumpVec<_>>();

        let inst = builder.ins().call(func_ref, &args);
        if let Some(&first) = builder.inst_results(inst).first() {
            let ret = ret.unwrap();
            self.save_value(ret, first, 0, false, builder)
        }
    }

    fn builtin_call(
        &mut self,
        func_id: VRef<Func>,
        args: BumpVec<ir::Value>,
        target: OptVRef<ValueMir>,
        builder: &mut GenBuilder,
    ) {
        let Func {
            signature, name, ..
        } = self.typec.funcs[func_id];
        let op_str = self.interner[name]
            .split_whitespace()
            .nth(1)
            .unwrap_or(&self.interner[name]);
        let signed = signature.ret.is_signed();

        macro_rules! helper {
            (ints) => {
                ir::types::I8 | ir::types::I16 | ir::types::I32 | ir::types::I64
            };
            (binary) => {
                helper!(ints) | ir::types::B1
            };
            (scalars) => {
                helper!(binary)
            };
        }

        let value = match *args.as_slice() {
            [a, b] => match (builder.func.dfg.value_type(a), op_str) {
                (helper!(ints), "+") => builder.ins().iadd(a, b),
                (helper!(ints), "-") => builder.ins().isub(a, b),
                (helper!(ints), "*") => builder.ins().imul(a, b),
                (helper!(ints), "/") if signed => builder.ins().sdiv(a, b),
                (helper!(ints), "/") => builder.ins().udiv(a, b),
                (helper!(scalars), "==") => builder.ins().icmp(IntCC::Equal, a, b),
                (helper!(binary), "&") => builder.ins().band(a, b),
                val => unimplemented!("{:?}", val),
            },
            _ => unimplemented!(),
        };

        self.save_value(target.unwrap(), value, 0, false, builder);
    }

    fn control_flow(&mut self, control_flow: ControlFlowMir, builder: &mut GenBuilder) {
        match control_flow {
            ControlFlowMir::Return(ret) => {
                if let Some(ret) = ret {
                    let ret = self.load_value(ret, builder);
                    builder.ins().return_(&[ret]);
                } else {
                    builder.ins().return_(&[]);
                }
            }
            ControlFlowMir::Terminal => {
                builder.ins().trap(ir::TrapCode::UnreachableCodeReached);
            }
            ControlFlowMir::Split(cond, a, b) => {
                let a = self.instantiate_block(a, builder);
                let b = self.instantiate_block(b, builder);
                let cond = self.load_value(cond, builder);
                builder.ins().brnz(cond, a, &[]);
                builder.ins().jump(b, &[]);
            }
            ControlFlowMir::Goto(b, val) => {
                let b = self.instantiate_block(b, builder);
                let val = val.map(|val| {
                    let value = self.load_value(val, builder);
                    self.gen_resources.values[val].take();
                    self.gen_resources.offsets[val] = 0;
                    self.gen_resources.must_load[val] = false;
                    value
                });
                builder
                    .ins()
                    .jump(b, val.as_ref().map(slice::from_ref).unwrap_or_default());
            }
        }
    }

    fn instantiate_block(&mut self, block: VRef<BlockMir>, builder: &mut GenBuilder) -> ir::Block {
        let gen_block = self.gen_resources.blocks[block].get_or_insert_with(|| GenBlock {
            id: builder.create_block(),
            visit_count: builder.body.blocks[block].ref_count,
        });

        if gen_block.visit_count != 1 {
            gen_block.visit_count -= 1;
        } else {
            self.gen_resources.block_stack.push((block, gen_block.id));
        }

        gen_block.id
    }

    fn load_value(&mut self, target: VRef<ValueMir>, builder: &mut GenBuilder) -> ir::Value {
        let must_load = self.gen_resources.must_load[target];
        let offset = self.gen_resources.offsets[target];
        let ptr_ty = builder.ptr_ty();
        let layout = self.ty_layout(builder.body.value_ty(target), ptr_ty);
        let value = match self.gen_resources.values[target].expect("value must be computed by now")
        {
            ComputedValue::Value(value) => value,
            ComputedValue::StackSlot(ss) => {
                return match layout.on_stack {
                    true => builder.ins().stack_addr(ptr_ty, ss, offset),
                    false => builder.ins().stack_load(layout.repr, ss, offset),
                };
            }
            ComputedValue::Variable(var) => builder.use_var(var),
        };

        match must_load && !layout.on_stack {
            true => builder.ins().load(layout.repr, MemFlags::new(), value, 0),
            false => value,
        }
    }

    fn assign_value(
        &mut self,
        target: VRef<ValueMir>,
        source: VRef<ValueMir>,
        builder: &mut GenBuilder,
    ) {
        let source_value =
            self.gen_resources.values[source].expect("value must be computed by now");
        let source_offset = self.gen_resources.offsets[source];
        let must_load_source = self.gen_resources.must_load[source];
        self.save_value(
            target,
            source_value,
            source_offset,
            must_load_source,
            builder,
        );
    }

    fn save_value(
        &mut self,
        target: VRef<ValueMir>,
        source_value: impl Into<ComputedValue>,
        source_offset: i32,
        must_load_source: bool,
        builder: &mut GenBuilder,
    ) {
        let source_value = source_value.into();

        let ptr_ty = builder.ptr_ty();
        let layout = self.ty_layout(builder.body.value_ty(target), builder.ptr_ty());

        let target_offset = self.gen_resources.offsets[target];

        if must_load_source && self.gen_resources.must_load[target] {
            let target_value = self.ensure_target(target, None, builder);
            let mut get_addr = |value| match value {
                ComputedValue::StackSlot(slot) => builder.ins().stack_addr(ptr_ty, slot, 0),
                ComputedValue::Value(value) => value,
                ComputedValue::Variable(var) => builder.use_var(var),
            };
            let target_addr = get_addr(target_value);
            let source_addr = get_addr(source_value);

            let non_overlapping = matches!((target_value, source_value), (
                ComputedValue::StackSlot(a),
                ComputedValue::StackSlot(b),
            ) if a != b);

            let config = builder.isa.frontend_config();
            builder.emit_small_memory_copy(
                config,
                target_addr,
                source_addr,
                layout.size as u64,
                layout.align.get(),
                layout.align.get(),
                non_overlapping,
                MemFlags::new(),
            );

            return;
        }

        let source = if must_load_source {
            let repr = match layout.repr == types::B1 {
                true => types::I8,
                false => layout.repr,
            };
            match source_value {
                ComputedValue::Value(val) => {
                    builder
                        .ins()
                        .load(repr, MemFlags::new(), val, source_offset)
                }
                ComputedValue::Variable(var) => {
                    let val = builder.use_var(var);
                    builder
                        .ins()
                        .load(repr, MemFlags::new(), val, source_offset)
                }
                ComputedValue::StackSlot(ss) => builder.ins().stack_load(repr, ss, source_offset),
            }
        } else {
            let value = match source_value {
                ComputedValue::Value(val) => val,
                ComputedValue::Variable(var) => builder.use_var(var),
                ComputedValue::StackSlot(..) => unreachable!(),
            };

            let shifted = match source_offset.cmp(&0) {
                Ordering::Less => builder.ins().ishl_imm(value, -source_offset as i64 * 8),
                Ordering::Equal => value,
                Ordering::Greater => builder.ins().ushr_imm(value, source_offset as i64 * 8),
            };

            match builder.func.dfg.value_type(shifted).bytes() > layout.size {
                true => builder.ins().ireduce(layout.repr, shifted),
                false => shifted,
            }
        };

        let must_load_target = match self.gen_resources.values[target].is_none() {
            true => builder.body.is_referenced(target) || layout.on_stack,
            false => self.gen_resources.must_load[target],
        };

        let source = match layout.repr == types::B1 && !must_load_target && must_load_source {
            true => builder.ins().icmp_imm(IntCC::NotEqual, source, 0),
            false => source,
        };

        let target_value = self.ensure_target(target, Some(source), builder);
        if must_load_target {
            match target_value {
                ComputedValue::Value(val) => {
                    builder
                        .ins()
                        .store(MemFlags::new(), source, val, target_offset);
                }
                ComputedValue::Variable(var) => {
                    let val = builder.use_var(var);
                    builder
                        .ins()
                        .store(MemFlags::new(), source, val, target_offset);
                }
                ComputedValue::StackSlot(ss) => {
                    builder.ins().stack_store(source, ss, target_offset);
                }
            }
        } else {
            match target_value {
                ComputedValue::Value(value) => {
                    let new_value = self.set_bit_field(source, value, target_offset, builder);
                    self.gen_resources.values[target] = Some(ComputedValue::Value(new_value));
                }
                ComputedValue::StackSlot(..) => unreachable!(),
                ComputedValue::Variable(var) => {
                    let value = builder.use_var(var);
                    let new_value = self.set_bit_field(source, value, target_offset, builder);
                    builder.def_var(var, new_value);
                }
            }
        }
    }

    fn set_bit_field(
        &mut self,
        source: ir::Value,
        target: ir::Value,
        target_offset: i32,
        builder: &mut GenBuilder,
    ) -> ir::Value {
        let source_size = builder.func.dfg.value_type(source).bytes();
        let target_repr = builder.func.dfg.value_type(target);
        let balanced = match target_repr.bytes() > source_size {
            true => builder.ins().uextend(target_repr, source),
            false => source,
        };

        let shifted = match target_offset.cmp(&0) {
            Ordering::Less => builder.ins().ishl_imm(balanced, -target_offset as i64 * 8),
            Ordering::Equal => balanced,
            Ordering::Greater => builder.ins().ushr_imm(balanced, target_offset as i64 * 8),
        };

        if target_repr.bytes() != source_size {
            let insert_mask = !((
                // results into source size full of ones
                (1 << (source_size as i64 * 8)) - 1
            ) << (target_offset as i64 * 8));
            let target = builder.ins().band_imm(target, insert_mask);
            builder.ins().bor(target, shifted)
        } else {
            shifted
        }
    }
}

use std::{cmp::Ordering, default::default, slice};

use cranelift_codegen::ir::{
    self, condcodes::IntCC, types, InstBuilder, MemFlags, StackSlotData, StackSlotKind, Type,
};
use cranelift_frontend::Variable;
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
        let has_s_ret =
            self.populate_signature(signature, params, &mut builder.func.signature, system_cc);

        let entry_block = builder.create_block();
        if has_s_ret {
            let ret = builder.body.ret;
            let addr = builder.append_block_param(entry_block, ptr_ty);
            self.gen_resources.values[ret] = GenValue {
                computed: Some(ComputedValue::Value(addr)),
                offset: 0,
                must_load: true,
            };
        }
        self.gen_resources
            .block_stack
            .push((root, true, entry_block));
        while let Some((block, seal, ir_block)) = self.gen_resources.block_stack.pop() {
            self.block(block, ir_block, seal, builder);
        }

        builder.finalize();
    }

    fn block(
        &mut self,
        block: VRef<BlockMir>,
        ir_block: ir::Block,
        seal: bool,
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
            let layout = self.ty_layout(builder.value_ty(arg));
            if layout.size == 0 {
                continue;
            }
            if self.gen_resources.values[arg].computed.is_none() {
                let source = builder.append_block_param(ir_block, layout.repr);
                self.prepare_block_arg(arg, source, layout, builder);
            }
        }

        for &inst in &builder.body.insts[insts] {
            self.inst(inst, builder);
        }

        self.control_flow(control_flow, builder);

        if seal {
            builder.seal_block(ir_block);
        }

        ir_block
    }

    fn prepare_block_arg(
        &mut self,
        arg: VRef<ValueMir>,
        source: ir::Value,
        layout: Layout,
        builder: &mut GenBuilder,
    ) {
        let (computed, must_load) = if !layout.on_stack && builder.body.is_referenced(arg) {
            let ss = builder.create_sized_stack_slot(StackSlotData {
                kind: StackSlotKind::ExplicitSlot,
                size: layout.size,
            });
            builder.ins().stack_store(source, ss, 0);
            (ComputedValue::StackSlot(ss), true)
        } else if builder.body.is_mutable(arg) {
            let var = Variable::from_u32(arg.as_u32());
            builder.declare_var(var, layout.repr);
            builder.def_var(var, source);
            (ComputedValue::Variable(var), layout.on_stack)
        } else {
            (ComputedValue::Value(source), layout.on_stack)
        };

        self.gen_resources.values[arg] = GenValue {
            computed: Some(computed),
            offset: 0,
            must_load,
        };
    }

    fn inst(&mut self, inst: InstMir, builder: &mut GenBuilder) {
        match inst {
            InstMir::Int(value, ret) => {
                let ty = self.ty_repr(builder.value_ty(ret));
                let value = builder.ins().iconst(ty, value);
                self.save_value(ret, value, 0, false, builder);
            }
            InstMir::Bool(value, ret) => {
                let ty = self.ty_repr(builder.value_ty(ret));
                let value = builder.ins().bconst(ty, value);
                self.save_value(ret, value, 0, false, builder);
            }
            InstMir::Access(target, ret) => {
                if let Some(ret) = ret {
                    self.assign_value(ret, target, builder);
                }
            }
            InstMir::Call(call, ret) => self.call(call, ret, builder),
            InstMir::Ctor(fields, ret, needs_instance) => {
                self.constructor(fields, ret, needs_instance, builder)
            }
            InstMir::Deref(target, ret) => self.deref(target, ret, builder),
            InstMir::Ref(target, ret) => self.r#ref(target, ret, builder),
            InstMir::Field(header, field, ret) => self.field(header, field, ret, builder),
            InstMir::Var(value, ret) => {
                self.assign_value(ret, value, builder);
            }
            InstMir::Drop(drop) => self.drop(drop, builder),
        };
    }

    fn drop(&mut self, drop: VRef<DropMir>, builder: &mut GenBuilder) {
        let value = builder.body.drops[drop].value;
        let ty = builder.value_ty(value);
        let mut funcs = self.gen_resources.drops[drop.index()].clone();
        let GenValue {
            computed, offset, ..
        } = self.gen_resources.values[value];
        let value = self.ref_low(computed, ty, offset, builder);
        let mut frontier = bumpvec![(0, ty)];
        while let Some((offset, ty)) = frontier.pop() {
            if !self.typec.may_need_drop(ty) {
                continue;
            }

            // we can pass empty generics since all types are concrete
            if let Some(Some(..)) = ty.is_drop(&[], self.typec, self.interner) {
                let CompileRequestChild { id, params, .. } =
                    self.gen_resources.calls[funcs.next().unwrap()];
                let params = self.compile_requests.ty_slices[params].iter().copied();
                let (func, ..) = self.import_compiled_func(id, params, builder);
                let value = match offset == 0 {
                    true => value,
                    false => builder.ins().iadd_imm(value, offset as i64),
                };
                builder.ins().call(func, &[value]);
            }

            match ty {
                Ty::Struct(s) => {
                    let layout = self.ty_layout(ty);
                    self.gen_layouts.offsets[layout.offsets]
                        .iter()
                        .copied()
                        .rev()
                        .zip(self.typec[self.typec[s].fields].iter().map(|f| f.ty).rev())
                        .collect_into(&mut *frontier);
                }
                Ty::Enum(..) => todo!(),
                Ty::Instance(i) => {
                    let Instance { base, args } = self.typec[i];
                    let params = self.typec[args].to_bumpvec();
                    match base {
                        GenericTy::Struct(s) => {
                            let layout = self.ty_layout(ty);
                            self.gen_layouts.offsets[layout.offsets]
                                .iter()
                                .copied()
                                .rev()
                                .zip(
                                    self.typec[self.typec[s].fields]
                                        .to_bumpvec()
                                        .into_iter()
                                        .map(|f| {
                                            self.typec.instantiate(f.ty, &params, self.interner)
                                        })
                                        .rev(),
                                )
                                .collect_into(&mut *frontier);
                        }
                        GenericTy::Enum(..) => todo!(),
                    }
                }
                Ty::Pointer(..) | Ty::Param(..) | Ty::Builtin(..) => (),
            }
        }
    }

    fn control_flow(&mut self, control_flow: ControlFlowMir, builder: &mut GenBuilder) {
        match control_flow {
            ControlFlowMir::Return(ret) => {
                if !self.ty_layout(builder.value_ty(ret)).on_stack && let Some(ir_ret) = self.load_value(ret, builder) {
                    builder.ins().return_(&[ir_ret]);
                } else {
                    builder.ins().return_(&[]);
                }
            }
            ControlFlowMir::Terminal => {
                builder.ins().trap(ir::TrapCode::UnreachableCodeReached);
            }
            ControlFlowMir::Split(cond, a, b) => {
                let cond = self.load_value(cond, builder).unwrap();
                self.instantiate_block(a, builder, |b, _, builder| builder.ins().brnz(cond, b, &[]));
                self.instantiate_block(b, builder, |b, _, builder| builder.ins().jump(b, &[]));
            }
            ControlFlowMir::Goto(b, val) => {
                let val = (!self.gen_resources.values[val].must_load
                    || !self.ty_layout(builder.value_ty(val)).on_stack)
                    .then_some(val)
                    .and_then(|val| {
                        let value = self.load_value(val, builder);
                        self.gen_resources.values[val] = default();
                        value
                    });
                self.instantiate_block(b, builder, |b, _, builder|     builder
                    .ins()
                    .jump(b, val.as_ref().map(slice::from_ref).unwrap_or_default())
                );
            }
        }
    }

    fn deref(&mut self, target: VRef<ValueMir>, ret: VRef<ValueMir>, builder: &mut GenBuilder) {
        if let GenValue {
            computed: Some(..),
            must_load: true,
            ..
        } = self.gen_resources.values[ret]
        {
            let value = self.gen_resources.values[target].computed.unwrap();
            self.save_value(ret, value, 0, true, builder);
            return;
        }

        let Some(value) = self.load_value(target, builder) else { return; };
        self.gen_resources.values[ret] = GenValue {
            must_load: true,
            computed: Some(ComputedValue::Value(value)),
            ..self.gen_resources.values[target]
        };
    }

    fn r#ref(&mut self, target: VRef<ValueMir>, ret: VRef<ValueMir>, builder: &mut GenBuilder) {
        let GenValue {
            computed,
            offset,
            must_load,
        } = self.gen_resources.values[target];
        assert!(must_load);
        let ty = builder.value_ty(target);
        let addr = self.ref_low(computed, ty, offset, builder);
        self.save_value(ret, addr, 0, false, builder);
    }

    fn ref_low(
        &mut self,
        computed: Option<ComputedValue>,
        ty: Ty,
        offset: i32,
        builder: &mut GenBuilder,
    ) -> ir::Value {
        let ptr_ty = builder.ptr_ty();
        let value = match computed {
            Some(ComputedValue::Value(value)) => value,
            Some(ComputedValue::Variable(var)) => builder.use_var(var),
            Some(ComputedValue::StackSlot(ss)) => {
                return builder.ins().stack_addr(ptr_ty, ss, offset)
            }
            None => {
                let align = self.ty_layout(ty).align;
                return builder.ins().iconst(ptr_ty, align.get() as i64);
            }
        };

        match offset == 0 {
            true => value,
            false => builder.ins().iadd_imm(value, offset as i64),
        }
    }

    fn field(
        &mut self,
        header: VRef<ValueMir>,
        field: u32,
        ret: VRef<ValueMir>,
        builder: &mut GenBuilder,
    ) {
        // field just changes offset
        let GenValue {
            computed,
            offset,
            must_load,
        } = self.gen_resources.values[header];
        let header_ty = builder.value_ty(header);
        let offsets = self.ty_layout(header_ty).offsets;
        let field_offset = self.gen_layouts.offsets[offsets][field as usize];
        if self.gen_resources.values[ret].computed.is_some() {
            self.save_value(
                ret,
                computed.unwrap(),
                offset + field_offset as i32,
                must_load,
                builder,
            );
        } else {
            self.gen_resources.values[ret] = GenValue {
                computed,
                offset: offset + field_offset as i32,
                must_load,
            };
        }
    }

    fn constructor(
        &mut self,
        fields: VRefSlice<ValueMir>,
        ret: VRef<ValueMir>,
        _needs_instance: bool,
        builder: &mut GenBuilder,
    ) {
        let layout = self.ty_layout(builder.value_ty(ret));

        self.ensure_target_low(ret, None, true, builder);

        let base_value = self.gen_resources.values[ret];

        self.gen_layouts.offsets[layout.offsets]
            .iter()
            .zip(&builder.body.value_args[fields])
            .for_each(|(&offset, &field)| {
                self.gen_resources.values[field] = GenValue {
                    offset: offset as i32 + base_value.offset,
                    ..base_value
                };
            });
    }

    fn call(&mut self, call: VRef<CallMir>, ret: VRef<ValueMir>, builder: &mut GenBuilder) {
        let CallMir { args, .. } = builder.body.calls[call];
        let CompileRequestChild { id, func, params } = self.gen_resources.calls[call.index()];

        if self.typec.funcs[func].flags.contains(FuncFlags::BUILTIN) {
            if func == Func::CAST {
                self.cast(args, ret, builder)
            } else if func == Func::SIZEOF {
                self.sizeof(self.compile_requests.ty_slices[params][0], ret, builder)
            } else {
                let args = builder.body.value_args[args]
                    .iter()
                    .filter_map(|&arg| self.load_value(arg, builder))
                    .collect::<BumpVec<_>>();
                self.builtin_call(func, args, ret, builder);
            }

            return;
        }

        let params = self.compile_requests.ty_slices[params].iter().copied();
        let (func, struct_ret) = self.import_compiled_func(id, params, builder);

        let struct_ptr = struct_ret.then(|| {
            if self.gen_resources.values[ret].computed.is_some() {
                self.load_value(ret, builder).unwrap()
            } else {
                let ptr_ty = builder.ptr_ty();
                let layout = self.ty_layout(builder.value_ty(ret));
                let stack_slot = builder.create_sized_stack_slot(StackSlotData {
                    kind: StackSlotKind::ExplicitSlot,
                    size: layout.size,
                });
                self.gen_resources.values[ret] = GenValue {
                    computed: Some(ComputedValue::StackSlot(stack_slot)),
                    offset: 0,
                    must_load: true,
                };
                builder.ins().stack_addr(ptr_ty, stack_slot, 0)
            }
        });

        let args = struct_ptr
            .into_iter()
            .chain(
                builder.body.value_args[args]
                    .iter()
                    .filter_map(|&arg| self.load_value(arg, builder)),
            )
            .collect::<BumpVec<_>>();

        let inst = builder.ins().call(func, &args);
        if let Some(&first) = builder.inst_results(inst).first() && !struct_ret {
            self.save_value(ret, first, 0, false, builder)
        }
    }

    fn cast(&mut self, args: VRefSlice<ValueMir>, ret: VRef<ValueMir>, builder: &mut GenBuilder) {
        let [value] = builder.body.value_args[args] else {
            unreachable!()
        };
        self.assign_value(ret, value, builder)
    }

    fn sizeof(&mut self, ty: Ty, ret: VRef<ValueMir>, builder: &mut GenBuilder) {
        let ptr_ty = builder.ptr_ty();
        let ty = self.ty_layout(ty);
        let value = builder.ins().iconst(ptr_ty, ty.size as i64);
        self.save_value(ret, value, 0, false, builder);
    }

    fn builtin_call(
        &mut self,
        func_id: FragRef<Func>,
        args: BumpVec<ir::Value>,
        target: VRef<ValueMir>,
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
            ref slice => unimplemented!("{slice:?}"),
        };

        self.save_value(target, value, 0, false, builder);
    }

    fn instantiate_block<W>(
        &mut self,
        block: VRef<BlockMir>,
        builder: &mut GenBuilder,
        access: impl FnOnce(ir::Block, &mut Self, &mut GenBuilder) -> W,
    ) {
        let gen_block = self.gen_resources.blocks[block].get_or_insert_with(|| GenBlock {
            id: builder.create_block(),
            forward_visit_count: builder.body.blocks[block].ref_count
                - builder.body.blocks[block].cycles,
            backward_visit_count: builder.body.blocks[block].cycles,
        });

        if gen_block.forward_visit_count == 0 {
            if gen_block.backward_visit_count != 1 {
                gen_block.backward_visit_count -= 1;
            } else {
                let id = gen_block.id;
                access(id, self, builder);
                builder.seal_block(id);
                return;
            }
        } else {
            if gen_block.forward_visit_count == 1 {
                self.gen_resources.block_stack.push((
                    block,
                    gen_block.backward_visit_count == 0,
                    gen_block.id,
                ));
            }
            gen_block.forward_visit_count -= 1;
        }

        access(gen_block.id, self, builder);
    }

    fn load_value(
        &mut self,
        target: VRef<ValueMir>,
        builder: &mut GenBuilder,
    ) -> Option<ir::Value> {
        let GenValue {
            computed,
            offset,
            must_load,
        } = self.gen_resources.values[target];
        let layout = self.ty_layout(builder.value_ty(target));
        // dbg!(layout, builder.value_ty(target), target);
        if layout.size == 0 {
            return None;
        }
        Some(Self::load_value_low(
            builder,
            must_load,
            false,
            layout,
            offset,
            computed.expect("value should be computed"),
        ))
    }

    fn assign_value(
        &mut self,
        target: VRef<ValueMir>,
        source: VRef<ValueMir>,
        builder: &mut GenBuilder,
    ) {
        let GenValue {
            computed,
            offset,
            must_load,
        } = self.gen_resources.values[source];
        self.save_value(
            target,
            computed.expect("value must be computed by now"),
            offset,
            must_load,
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
        let layout = self.ty_layout(builder.value_ty(target));

        if layout.size == 0 {
            return;
        }

        let GenValue {
            computed,
            offset,
            must_load,
        } = self.gen_resources.values[target];

        let must_load_target = match computed.is_none() {
            true => builder.body.is_referenced(target) || layout.on_stack,
            false => must_load,
        };

        if must_load_source && must_load_target {
            let target_value = self
                .ensure_target(target, None, builder)
                .expect("impossible");

            let mut get_addr = |value, offset: i32| match value {
                ComputedValue::StackSlot(slot) => builder.ins().stack_addr(ptr_ty, slot, offset),
                ComputedValue::Value(val) => match offset != 0 {
                    true => builder.ins().iadd_imm(val, offset as i64),
                    false => val,
                },
                ComputedValue::Variable(var) => builder.use_var(var),
            };
            let target_addr = get_addr(target_value, offset);
            let source_addr = get_addr(source_value, source_offset);

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

        let source = Self::load_value_low(
            builder,
            must_load_source,
            must_load_target,
            layout,
            source_offset,
            source_value,
        );

        let Some(target_value) = self.ensure_target(target, Some(source), builder) else {
            return;
        };

        if must_load_target {
            match target_value {
                ComputedValue::Value(val) => {
                    builder.ins().store(MemFlags::new(), source, val, offset);
                }
                ComputedValue::Variable(var) => {
                    let val = builder.use_var(var);
                    builder.ins().store(MemFlags::new(), source, val, offset);
                }
                ComputedValue::StackSlot(ss) => {
                    builder.ins().stack_store(source, ss, offset);
                }
            }
        } else {
            match target_value {
                ComputedValue::Value(value) => {
                    let new_value = self.set_bit_field(source, value, offset, builder);
                    self.gen_resources.values[target].computed =
                        Some(ComputedValue::Value(new_value));
                }
                ComputedValue::StackSlot(..) => unreachable!(),
                ComputedValue::Variable(var) => {
                    let value = builder.use_var(var);
                    let new_value = self.set_bit_field(source, value, offset, builder);
                    builder.def_var(var, new_value);
                }
            }
        }
    }

    fn load_value_low(
        builder: &mut GenBuilder,
        must_load_source: bool,
        must_load_target: bool,
        layout: Layout,
        source_offset: i32,
        source_value: ComputedValue,
    ) -> ir::Value {
        let source = if must_load_source {
            let repr = match layout.repr == types::B1 {
                true => types::I8,
                false => layout.repr,
            };
            if layout.on_stack {
                'a: {
                    let value = match source_value {
                        ComputedValue::Value(val) => val,
                        ComputedValue::Variable(var) => builder.use_var(var),
                        ComputedValue::StackSlot(ss) => {
                            break 'a builder.ins().stack_addr(repr, ss, source_offset)
                        }
                    };

                    match source_offset == 0 {
                        true => value,
                        false => builder.ins().iadd_imm(value, source_offset as i64),
                    }
                }
            } else {
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
                    ComputedValue::StackSlot(ss) => {
                        builder.ins().stack_load(repr, ss, source_offset)
                    }
                }
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

        match layout.repr == types::B1 && !must_load_target && must_load_source {
            true => builder.ins().icmp_imm(IntCC::NotEqual, source, 0),
            false => source,
        }
    }

    fn ensure_target(
        &mut self,
        target: VRef<ValueMir>,
        source_value: Option<ir::Value>,
        builder: &mut GenBuilder,
    ) -> Option<ComputedValue> {
        self.ensure_target_low(target, source_value, false, builder)
    }

    fn ensure_target_low(
        &mut self,
        target: VRef<ValueMir>,
        source_value: Option<ir::Value>,
        force_mutable: bool,
        builder: &mut GenBuilder,
    ) -> Option<ComputedValue> {
        if let value @ Some(..) = self.gen_resources.values[target].computed {
            return value;
        }

        let referenced = builder.body.is_referenced(target);

        let layout = self.ty_layout(builder.value_ty(target));
        let must_load = layout.on_stack || referenced;
        let computed = if must_load {
            let ss = builder.create_sized_stack_slot(StackSlotData {
                kind: StackSlotKind::ExplicitSlot,
                size: layout.size,
            });
            if let Some(source) = source_value {
                builder.ins().stack_store(source, ss, 0);
            }
            ComputedValue::StackSlot(ss)
        } else {
            let init = source_value.unwrap_or_else(|| builder.ins().iconst(layout.repr, 0));
            if builder.body.is_mutable(target) || force_mutable {
                let var = Variable::from_u32(target.as_u32());
                builder.declare_var(var, layout.repr);
                builder.def_var(var, init);
                ComputedValue::Variable(var)
            } else {
                ComputedValue::Value(init)
            }
        };

        self.gen_resources.values[target] = GenValue {
            computed: Some(computed),
            offset: 0,
            must_load,
        };

        source_value.is_none().then_some(computed)
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
            false => return source,
        };

        let shifted = match target_offset.cmp(&0) {
            Ordering::Less => builder.ins().ushr_imm(balanced, -target_offset as i64 * 8),
            Ordering::Equal => balanced,
            Ordering::Greater => builder.ins().ishl_imm(balanced, target_offset as i64 * 8),
        };

        let insert_mask = !((
            // results into source size full of ones
            (1 << (source_size as i64 * 8)) - 1
        ) << (target_offset as i64 * 8));
        let target = builder.ins().band_imm(target, insert_mask);
        builder.ins().bor(target, shifted)
    }
}

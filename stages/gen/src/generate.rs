use std::{cmp::Ordering, default::default, slice};

use cranelift_codegen::ir::{
    self, condcodes::IntCC, types, InstBuilder, MemFlags, StackSlotData, StackSlotKind, Type,
};
use cranelift_frontend::Variable;
use diags::{snippet, Workspace};
use mir_t::*;
use packaging_t::Source;
use storage::*;

use typec_t::*;

use crate::{context::ComputedValue, *};

mod function_loading;
mod size_calc;

impl Generator<'_> {
    pub fn check_casts(
        &mut self,
        source: VRef<Source>,
        workspace: &mut Workspace,
        checks: &mut Vec<CastCheck>,
    ) {
        for CastCheck { loc, from, to } in checks.drain(..) {
            if self.typec.contains_params(from) || self.typec.contains_params(to) {
                workspace.push(snippet! {
                    err: "cast between generic types is not allowed";
                    info: (
                        "cast from {} to {}, which contain generic parameters that depend on function instance",
                        self.typec.display_ty(from, self.interner),
                        self.typec.display_ty(to, self.interner),
                    );
                    (loc, source) {
                        err[loc]: "happened here";
                    }
                });
                continue;
            }

            let from_layout = self.ty_layout(from);
            let to_layout = self.ty_layout(to);
            if from_layout.size != to_layout.size {
                workspace.push(snippet! {
                    err: "cast size mismatch";
                    info: (
                        "cast from {}({}) to {}({}), size does not match",
                        self.typec.display_ty(from, self.interner),
                        from_layout.size,
                        self.typec.display_ty(to, self.interner),
                        to_layout.size,
                    );
                    (loc, source) {
                        err[loc]: "happened here";
                    }
                });
            }
        }
    }

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
        self.gen_resources.block_stack.push((root, entry_block));
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
            let layout = self.ty_layout(builder.value_ty(arg));
            if let val @ GenValue { computed: None, .. } = &mut self.gen_resources.values[arg] {
                *val = GenValue {
                    computed: Some(ComputedValue::Value(
                        builder.append_block_param(ir_block, layout.repr),
                    )),
                    offset: 0,
                    must_load: layout.on_stack,
                };
            }
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
            InstMir::Const(id, ret) => self.r#const(id, ret, builder),
            InstMir::Ctor(fields, ret, needs_instance) => {
                self.constructor(fields, ret, needs_instance, builder)
            }
            InstMir::Deref(target, ret) => self.deref(target, ret, builder),
            InstMir::Ref(target, ret) => self.r#ref(target, ret, builder),
            InstMir::Field(header, field, ret) => self.field(header, field, ret, builder),
            InstMir::Var(value, ret) => {
                self.assign_value(ret, value, builder);
            }
        };
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
                let val = val
                    .filter(|&val| {
                        !self.gen_resources.values[val].must_load
                            || !self.ty_layout(builder.value_ty(val)).on_stack
                    })
                    .map(|val| {
                        let value = self.load_value(val, builder);
                        self.gen_resources.values[val] = default();
                        value
                    });
                builder
                    .ins()
                    .jump(b, val.as_ref().map(slice::from_ref).unwrap_or_default());
            }
        }
    }

    fn deref(&mut self, target: VRef<ValueMir>, ret: VRef<ValueMir>, builder: &mut GenBuilder) {
        let value = self.load_value(target, builder);
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
        let ptr_ty = builder.ptr_ty();
        let addr = match computed.unwrap() {
            ComputedValue::Value(value) => value,
            ComputedValue::Variable(var) => builder.use_var(var),
            ComputedValue::StackSlot(ss) => builder.ins().stack_addr(ptr_ty, ss, offset),
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
        let GenValue {
            computed,
            offset,
            must_load,
        } = self.gen_resources.values[header];
        let header_ty = builder.value_ty(header);
        let offsets = self.ty_layout(header_ty).offsets;
        let field_offset = self.gen_layouts.offsets[offsets][field as usize];
        self.gen_resources.values[ret] = GenValue {
            computed,
            offset: offset + field_offset as i32,
            must_load,
        };
    }

    fn constructor(
        &mut self,
        fields: VRefSlice<ValueMir>,
        ret: VRef<ValueMir>,
        _needs_instance: bool,
        builder: &mut GenBuilder,
    ) {
        let layout = self.ty_layout(builder.value_ty(ret));

        self.ensure_target(ret, None, builder);

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

    fn r#const(&mut self, id: VRef<FuncConstMir>, ret: VRef<ValueMir>, builder: &mut GenBuilder) {
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
    }

    fn call(&mut self, call: VRef<CallMir>, ret: OptVRef<ValueMir>, builder: &mut GenBuilder) {
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
                    .map(|&arg| self.load_value(arg, builder))
                    .collect::<BumpVec<_>>();
                self.builtin_call(func, args, ret, builder);
            }

            return;
        }

        let params = self.compile_requests.ty_slices[params].iter().copied();
        let (func, struct_ret) = self.import_compiled_func(id, params, builder);

        let struct_ptr = struct_ret.then(|| {
            let ret = ret.unwrap();
            if self.gen_resources.values[ret].computed.is_some() {
                self.load_value(ret, builder)
            } else {
                let ptr_ty = builder.ptr_ty();
                let layout = self.ty_layout(builder.value_ty(ret));
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

        let inst = builder.ins().call(func, &args);
        if let Some(&first) = builder.inst_results(inst).first() && !struct_ret {
            let ret = ret.unwrap();
            self.save_value(ret, first, 0, false, builder)
        }
    }

    fn cast(
        &mut self,
        args: VRefSlice<ValueMir>,
        ret: OptVRef<ValueMir>,
        builder: &mut GenBuilder,
    ) {
        let [value] = builder.body.value_args[args] else {
            unreachable!()
        };
        self.assign_value(ret.unwrap(), value, builder)
    }

    fn sizeof(&mut self, ty: Ty, ret: OptVRef<ValueMir>, builder: &mut GenBuilder) {
        let ptr_ty = builder.ptr_ty();
        let ty = self.ty_layout(ty);
        let value = builder.ins().iconst(ptr_ty, ty.size as i64);
        self.save_value(ret.unwrap(), value, 0, false, builder);
    }

    fn builtin_call(
        &mut self,
        func_id: FragRef<Func>,
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
            ref slice => unimplemented!("{slice:?}"),
        };

        self.save_value(target.unwrap(), value, 0, false, builder);
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
        let GenValue {
            computed,
            offset,
            must_load,
        } = self.gen_resources.values[target];
        let ptr_ty = builder.ptr_ty();
        let layout = self.ty_layout(builder.value_ty(target));
        let value = match computed.expect("value must be computed by now") {
            ComputedValue::Value(value) => value,
            ComputedValue::StackSlot(ss) => {
                return match layout.on_stack {
                    true => builder.ins().stack_addr(ptr_ty, ss, offset),
                    false => builder.ins().stack_load(layout.repr, ss, offset),
                };
            }
            ComputedValue::Variable(var) => builder.use_var(var),
        };

        match (must_load, layout.on_stack) {
            (true, false) => builder
                .ins()
                .load(layout.repr, MemFlags::new(), value, offset),
            (true, true) if offset != 0 => builder.ins().iadd_imm(value, offset as i64),
            _ => value,
        }
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
            let (target_value, ..) = self.ensure_target(target, None, builder);

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

        let source = match layout.repr == types::B1 && !must_load_target && must_load_source {
            true => builder.ins().icmp_imm(IntCC::NotEqual, source, 0),
            false => source,
        };

        let (target_value, used) = self.ensure_target(target, Some(source), builder);
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
        } else if !used {
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

    fn ensure_target(
        &mut self,
        target: VRef<ValueMir>,
        source_value: Option<ir::Value>,
        builder: &mut GenBuilder,
    ) -> (ComputedValue, bool) {
        if let Some(value) = self.gen_resources.values[target].computed {
            return (value, false);
        }

        let referenced = builder.body.is_referenced(target);

        let layout = self.ty_layout(builder.value_ty(target));
        let must_load = layout.on_stack || referenced;
        let (computed, used) = if must_load {
            let ss = builder.create_sized_stack_slot(StackSlotData {
                kind: StackSlotKind::ExplicitSlot,
                size: layout.size,
            });
            (ComputedValue::StackSlot(ss), false)
        } else {
            let init = source_value.unwrap_or_else(|| builder.ins().iconst(layout.repr, 0));
            (
                if builder.body.is_mutable(target) {
                    let var = Variable::from_u32(target.as_u32());
                    builder.declare_var(var, layout.repr);
                    builder.def_var(var, init);
                    ComputedValue::Variable(var)
                } else {
                    ComputedValue::Value(init)
                },
                source_value.is_some(),
            )
        };

        self.gen_resources.values[target] = GenValue {
            computed: Some(computed),
            offset: 0,
            must_load,
        };

        (computed, used)
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

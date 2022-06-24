use cranelift_codegen::packed_option::PackedOption;

use crate::*;
use instance_types::*;
use storage::*;
use typec_types::*;

type ExprValue = Option<Value>;
type Dest<'a> = &'a mut Option<Value>;

pub struct MirBuilderContext {
    tir_mapping: SecondaryMap<Tir, PackedOption<Value>>,
    match_block_mapping: SecondaryMap<Tir, PackedOption<Block>>,
    match_block_stack: Vec<PackedOption<Block>>,
    seen: EntitySet<Tir>,
}

impl MirBuilderContext {
    pub fn new() -> Self {
        Self {
            tir_mapping: SecondaryMap::new(),
            match_block_mapping: SecondaryMap::new(),
            match_block_stack: Vec::new(),
            seen: EntitySet::new(),
        }
    }

    pub fn clear(&mut self) {
        self.tir_mapping.clear();
        self.match_block_mapping.clear();
        self.match_block_stack.clear();
        self.seen.clear();
    }
}

impl<'a> MirBuilder<'a> {
    pub fn func(&mut self) -> errors::Result {
        self.mir_builder_context.clear();
        self.func_ctx.clear();

        let FuncMeta {
            sig, body, args, ..
        } = self.funcs[self.func.meta()];

        let entry_point = self.func_ctx.create_block();
        {
            self.func_ctx.value_slices.mark_frame();

            let has_sret = {
                let ret = sig.ret;
                self.reprs[ret].flags.contains(ReprFlags::ON_STACK)
            };

            if has_sret {
                let value = self.add_value(ValueEnt::new(sig.ret).flags(MirFlags::POINTER));
                self.return_dest = Some(value);
                self.func_ctx.value_slices.push_one(value);
            }

            for &tir in self.tir_data.cons.get(args) {
                let ty = self.tir_data.ents[tir].ty;
                let value = self.add_value(ValueEnt::new(ty));
                self.func_ctx.value_slices.push_one(value);
                self.mir_builder_context.seen.insert(tir);
                self.mir_builder_context.tir_mapping[tir] = value.into();
            }

            self.func_ctx.blocks[entry_point].params = self.func_ctx.value_slices.pop_frame();
        }
        self.func_ctx.select_block(entry_point);

        self.expr(body, &mut None);

        Ok(())
    }

    fn expr(&mut self, tir: Tir, dest: Dest) -> ExprValue {
        if !self.mir_builder_context.seen.insert(tir) {
            let value = self.mir_builder_context.tir_mapping[tir].expand();
            if let Some(value) = value {
                self.gen_assign(value, dest);
            }
            return value;
        }

        let TirEnt {
            kind, flags, ty, ..
        } = self.tir_data.ents[tir];

        let mut result = match kind {
            TirKind::FuncPtr(..)
            | TirKind::IntLit(..)
            | TirKind::BoolLit(..)
            | TirKind::CharLit(..) => self.lit(tir),

            TirKind::GlobalAccess(..) => self.global_access(tir),

            TirKind::IndirectCall(..) => self.indirect_call(tir, dest),
            TirKind::Call(..) => self.call(tir, dest),
            TirKind::Constructor(..) => self.constructor(tir, dest),

            // taking the dest to not generate assign twice
            TirKind::Access(value, ..) => self.expr(value, dest),

            TirKind::FieldAccess(..) => self.field_access(tir),

            TirKind::If(..) => self.r#if(tir, dest),
            TirKind::Loop(..) => self.r#loop(tir, dest),

            TirKind::Continue(..) => self.r#continue(tir),
            TirKind::Break(..) => self.r#break(tir),
            TirKind::Return(..) => self.r#return(tir),

            TirKind::Block(..) => self.block(tir, dest),

            TirKind::Assign(..) => self.assign(tir),

            TirKind::BitCast(..) => self.bit_cast(tir, dest),

            TirKind::Variable(..) => self.variable(tir),

            TirKind::TakePtr(..) => self.take_ptr(tir),
            TirKind::DerefPtr(..) => self.deref_ptr(tir),

            TirKind::Match(_, _) => todo!(),
            TirKind::MatchBlock(_) => todo!(),

            TirKind::Argument(..) | TirKind::LoopInProgress(..) | TirKind::Invalid => {
                unreachable!()
            }
        };

        if let Some(ref mut value) = result {
            let result_flags = self.func_ctx.values[*value].flags;
            if flags.contains(TirFlags::SPILLED) && !result_flags.contains(MirFlags::POINTER) {
                let stack = self.create_stack(ty);
                self.gen_assign(*value, &mut Some(stack));
                *value = stack;
            }

            self.gen_assign(*value, dest);
        }

        self.mir_builder_context.tir_mapping[tir] = result.into();

        result
    }

    fn deref_ptr(&mut self, tir: Tir) -> ExprValue {
        let TirEnt {
            kind: TirKind::DerefPtr(value),
            ty,
            ..
        } = self.tir_data.ents[tir] else {
            unreachable!()
        };

        let value = self.expr(value, &mut None).unwrap();

        let ent = ValueEnt::new(ty).flags(MirFlags::POINTER);
        let result = self.add_value(ent);

        let kind = InstKind::DerefPtr(value);
        let ent = InstEnt::new(kind).value(result);
        self.add_inst(ent);

        Some(result)
    }

    fn take_ptr(&mut self, tir: Tir) -> ExprValue {
        let TirEnt {
            kind: TirKind::TakePtr(value),
            ty,
            ..
        } = self.tir_data.ents[tir] else {
            unreachable!()
        };

        let value = self.expr(value, &mut None).unwrap();

        let ent = ValueEnt::new(ty);
        let result = self.add_value(ent);

        let kind = InstKind::TakePtr(value);
        let ent = InstEnt::new(kind).value(result);
        self.add_inst(ent);

        Some(result)
    }

    fn variable(&mut self, tir: Tir) -> ExprValue {
        let TirEnt {
            kind: TirKind::Variable(var),
            flags,
            ty,
            ..
        } = self.tir_data.ents[tir] else {
            unreachable!()
        };

        let assignable = !flags.contains(TirFlags::IMMUTABLE);
        let on_stack = self.reprs[ty].flags.contains(ReprFlags::ON_STACK);

        let mut dest = on_stack.then(|| self.create_stack(ty));
        let value = self.expr(var, &mut dest).unwrap();

        let pointer = self.func_ctx.values[value]
            .flags
            .contains(MirFlags::POINTER);

        self.func_ctx.values[value]
            .flags
            .insert(MirFlags::ASSIGNABLE & (assignable && !on_stack && !pointer));

        let kind = InstKind::Variable;
        let ent = InstEnt::new(kind).value(value);
        self.func_ctx.add_inst(ent);

        None
    }

    fn bit_cast(&mut self, tir: Tir, dest: Dest) -> ExprValue {
        let TirEnt {
            kind: TirKind::BitCast(value),
            ..
        } = self.tir_data.ents[tir] else {
            unreachable!();
        };

        self.expr(value, dest)
    }

    fn assign(&mut self, tir: Tir) -> ExprValue {
        let TirEnt { kind: TirKind::Assign(lhs, rhs, drops), .. } = self.tir_data.ents[tir] else {
            unreachable!()
        };

        let a = self.expr(lhs, &mut None).unwrap();

        self.gen_drops(drops);

        self.expr(rhs, &mut Some(a)).unwrap();

        None
    }

    fn block(&mut self, tir: Tir, dest: Dest) -> ExprValue {
        let TirKind::Block(stmts, drops) = self.tir_data.ents[tir].kind else {
            unreachable!()
        };

        let value = if let Some((&last, others)) = self.tir_data.cons.get(stmts).split_last() {
            for &stmt in others {
                self.expr(stmt, &mut None);
            }
            self.expr(last, dest)
        } else {
            None
        };

        self.gen_drops(drops);

        value
    }

    fn r#return(&mut self, tir: Tir) -> ExprValue {
        let TirEnt { kind: TirKind::Return(value, drops), .. } = self.tir_data.ents[tir] else {
            unreachable!();
        };

        self.gen_drops(drops);

        let value = if let Some(value) = value.expand() {
            let mut dest = self.return_dest;
            self.expr(value, &mut dest)
        } else {
            None
        };

        let kind = InstKind::Return;
        let ent = InstEnt::new(kind).value(value);
        self.func_ctx.add_inst(ent);

        None
    }

    fn r#break(&mut self, tir: Tir) -> ExprValue {
        let TirEnt { kind: TirKind::Break(loop_header_marker, value, drops), .. } = self.tir_data.ents[tir] else {
            unreachable!()
        };

        self.gen_drops(drops);

        let &Loop {
            exit,
            mut dest,
            block_pass,
            ..
        } = self
            .func_ctx
            .loops
            .iter()
            .rev()
            .find(|loop_header| loop_header.marker == loop_header_marker)
            .unwrap();

        let value = if let Some(value) = value.expand() {
            if block_pass {
                self.expr(value, &mut None)
            } else {
                self.expr(value, &mut dest);
                None
            }
        } else {
            None
        };

        self.gen_jump(exit.unwrap(), value);

        None
    }

    fn r#continue(&mut self, tir: Tir) -> ExprValue {
        let TirEnt { kind: TirKind::Continue(loop_header_marker, drops), .. } = self.tir_data.ents[tir] else {
            unreachable!();
        };

        self.gen_drops(drops);

        let &Loop { entry, .. } = self
            .func_ctx
            .loops
            .iter()
            .rev()
            .find(|loop_header| loop_header.marker == loop_header_marker)
            .unwrap();

        self.gen_jump(entry, None);

        None
    }

    fn r#loop(&mut self, tir: Tir, dest: Dest) -> ExprValue {
        let TirEnt {
            kind: TirKind::Loop(body),
            flags,
            ty,
            ..
        } = self.tir_data.ents[tir] else {
            unreachable!()
        };

        let &mut dest_copy = dest;

        let enter_block = self.func_ctx.create_block();
        let (result, join_block) = self.gen_join_block(flags, ty, dest);

        self.gen_jump(enter_block, None);

        let loop_header = Loop {
            entry: enter_block,
            exit: join_block,
            marker: tir,
            dest: dest_copy,
            block_pass: result.is_some(),
        };

        self.func_ctx.select_block(enter_block);
        self.func_ctx.loops.push(loop_header);
        self.expr(body, &mut None);
        self.func_ctx.loops.pop().unwrap();

        if let Some(join_block) = join_block {
            if !self.func_ctx.is_terminated() {
                self.gen_jump(enter_block, None);
            }
            self.func_ctx.select_block(join_block);
        }

        result
    }

    fn r#if(&mut self, tir: Tir, dest: Dest) -> ExprValue {
        let TirEnt {
            kind: TirKind::If(cond, then, otherwise, pre_computes),
            flags,
            ty,
            ..
        } = self.tir_data.ents[tir] else {
            unreachable!();
        };

        let &mut mut dest_copy = dest;
        let has_ret = ty != self.builtin_types.nothing;

        let then_block = self.func_ctx.create_block();
        let otherwise_block = self.func_ctx.create_block();
        let (result, join_block) = self.gen_join_block(flags, ty, dest);

        let cond = self.expr(cond, &mut None).unwrap();

        for &pre_compute in self.tir_data.cons.get(pre_computes) {
            self.expr(pre_compute, &mut None).unwrap();
        }

        let kind = InstKind::JumpIfFalse(otherwise_block);
        let ent = InstEnt::new(kind).value(cond);
        self.add_inst(ent);

        self.gen_jump(then_block, None);

        self.func_ctx.select_block(then_block);
        let value = has_ret.then_some(self.expr(then, &mut dest_copy)).flatten();
        if let Some(join_block) = join_block && !self.func_ctx.is_terminated() {
            self.gen_jump(join_block, value);
        }

        self.func_ctx.select_block(otherwise_block);
        let value = has_ret
            .then_some(self.expr(otherwise, &mut dest_copy))
            .flatten();
        if let Some(join_block) = join_block && !self.func_ctx.is_terminated(){
            self.gen_jump(join_block, value);
        }

        if let Some(join_block) = join_block {
            self.func_ctx.select_block(join_block);
        }

        result
    }

    fn gen_join_block(
        &mut self,
        flags: TirFlags,
        ty: Ty,
        dest: Dest,
    ) -> (Option<Value>, Option<Block>) {
        if !flags.contains(TirFlags::TERMINATING) {
            let block = self.func_ctx.create_block();

            let on_stack = self.reprs[ty].flags.contains(ReprFlags::ON_STACK);
            let has_ret = ty != self.builtin_types.nothing;
            let result = if has_ret {
                if on_stack {
                    Some(dest.take().unwrap_or_else(|| self.create_stack(ty)))
                } else {
                    let value = self.add_value(ValueEnt::new(ty));
                    self.func_ctx.blocks[block].params = self.func_ctx.value_slices.push(&[value]);
                    Some(value)
                }
            } else {
                None
            };

            (result, Some(block))
        } else {
            (None, None)
        }
    }

    fn gen_jump(&mut self, block: Block, value: Option<Value>) {
        let kind = InstKind::Jump(block);
        let ent = InstEnt::new(kind).value(value);
        self.add_inst(ent);
    }

    fn field_access(&mut self, tir: Tir) -> ExprValue {
        let TirEnt {
            kind: TirKind::FieldAccess(base, field),
            ..
        } = self.tir_data.ents[tir] else {
            unreachable!()
        };

        let base_ty = self.tir_data.ents[base].ty;
        let base = self.expr(base, &mut None).unwrap();

        let TyCompEnt { index, .. } = self.ty_comps[field];
        let fields = self.reprs[base_ty].fields;
        // println!("{}", ty_display!(self, base_ty));
        let ReprField { offset, ty } = self.repr_fields.get(fields)[index as usize];

        let value = self.gen_offset(base, ty, offset);

        Some(value)
    }

    fn constructor(&mut self, tir: Tir, dest: Dest) -> ExprValue {
        let TirEnt {
            ty,
            kind: TirKind::Constructor(values),
            // flags,
            ..
        } = self.tir_data.ents[tir] else {
            unreachable!();
        };

        let on_stack = self.reprs[ty].flags.contains(ReprFlags::ON_STACK);
        // println!("{}", on_stack);
        let mut result = dest.unwrap_or_else(|| {
            if on_stack {
                self.create_stack(ty)
            } else {
                let value = self.add_value(ValueEnt::new(ty));

                let kind = InstKind::Int(0);
                let ent = InstEnt::new(kind).value(value);
                self.add_inst(ent);

                value
            }
        });

        if on_stack {
            dest.take();
        }

        let fields = self.reprs[ty].fields;
        let iter = self
            .tir_data
            .cons
            .get(values)
            .iter()
            .zip(self.repr_fields.get(fields));
        // println!("{}", ty_display!(self, ty));
        if on_stack {
            for (&value, field) in iter {
                let dest = self.gen_offset(result, ty, field.offset);
                self.expr(value, &mut Some(dest));
            }
        } else {
            for (&value, field) in iter {
                // println!("{} {}", ty_display!(self, field.ty), field.offset);
                result = self.gen_offset(result, ty, field.offset);
                self.expr(value, &mut Some(result));
                // this is important for structures that need bit manipulation (they fit into pointer)
                result = self.gen_offset(result, ty, Offset::ZERO - field.offset);
            }
        }

        Some(result)
    }

    fn call(&mut self, tir: Tir, dest: Dest) -> ExprValue {
        let TirEnt {
            ty,
            kind: TirKind::Call(caller, params, mut func, args),
            flags,
            ..
        } = self.tir_data.ents[tir] else {
            unreachable!();
        };

        let mut param_slots = self.vec_pool.get();
        if let FuncKind::Bound(bound, index) = self.funcs[func.meta()].kind {
            let bound_impl = bound_checker!(self)
                .implements(bound, caller.unwrap(), false)
                .unwrap();
            param_slots.resize(
                self.ty_lists.len_of(bound_impl.params),
                Ty::reserved_value(),
            );

            infer_parameters(
                caller.unwrap(),
                bound_impl.ty,
                &mut param_slots,
                Default::default(),
                self.types,
                self.ty_lists,
            )
            .unwrap();

            func = self.func_lists.get(bound_impl.funcs)[index as usize];
        }

        if flags.contains(TirFlags::GENERIC) {
            func = self.instantiate_func(func, params, &param_slots);
        }

        self.call_low(|args| InstKind::Call(func, args), ty, args, dest)
    }

    fn indirect_call(&mut self, tir: Tir, dest: Dest) -> ExprValue {
        let TirEnt {
            ty,
            kind: TirKind::IndirectCall(func, args),
            ..
        } = self.tir_data.ents[tir] else {
            unreachable!();
        };

        let func = self.expr(func, &mut None).unwrap();

        self.call_low(|args| InstKind::IndirectCall(func, args), ty, args, dest)
    }

    fn instantiate_func(&mut self, func: Func, params: TyList, global_params: &[Ty]) -> Func {
        let func_ent = self.funcs[func];
        let id = global_params
            .iter()
            .chain(self.ty_lists.get(params))
            .fold(func_ent.id, |acc, &ty| acc + self.types[ty].id);

        if let Some(&instance) = self.func_instances.get(id) {
            instance
        } else {
            let instance = FuncEnt {
                id,
                flags: func_ent.flags & !FuncFlags::GENERIC,
            };
            let instance = self.funcs.push_instance(instance, func);
            self.func_instances.insert(id, instance);
            let global_params = self.ty_lists.push(global_params);
            let params = self.ty_lists.join(global_params, params);
            self.to_compile.push((instance, params));
            instance
        }
    }

    fn call_low(
        &mut self,
        kind: impl FnOnce(ValueList) -> InstKind,
        ret: Ty,
        args: TirList,
        dest: Dest,
    ) -> ExprValue {
        let has_ret = ret != self.builtin_types.nothing;
        let has_sret = self.reprs[ret].flags.contains(ReprFlags::ON_STACK);

        let result = has_ret.then(|| {
            if has_sret {
                dest.take().unwrap_or_else(|| self.create_stack(ret))
            } else {
                let value = ValueEnt::new(ret);
                self.add_value(value)
            }
        });

        self.func_ctx.value_slices.mark_frame();

        if has_sret {
            self.func_ctx.value_slices.push_one(result.unwrap());
        }

        let args = self.args(args);

        let kind = kind(args);
        let ent = InstEnt::new(kind).value(result);
        self.add_inst(ent);

        result
    }

    fn create_stack(&mut self, ty: Ty) -> Value {
        let size = self.reprs[ty].layout.size();
        let slot = StackEnt::new(size, self.ptr_ty);
        let stack = self.func_ctx.stacks.push(slot);

        let ent = ValueEnt::new(ty).flags(MirFlags::POINTER);
        let value = self.add_value(ent);

        let kind = InstKind::StackAddr(stack);
        let ent = InstEnt::new(kind).value(value);
        self.func_ctx.add_inst(ent);

        value
    }

    fn lit(&mut self, tir: Tir) -> ExprValue {
        let TirEnt { ty, kind, .. } = self.tir_data.ents[tir];

        let inst = match kind {
            TirKind::FuncPtr(v) => InstKind::FuncPtr(v),
            TirKind::IntLit(v) => InstKind::Int(v),
            TirKind::BoolLit(v) => InstKind::Bool(v),
            TirKind::CharLit(v) => InstKind::Int(v as u128),
            TirKind::GlobalAccess(global) => InstKind::GlobalAccess(global),
            _ => unreachable!(),
        };

        let value = self.add_value(ValueEnt::new(ty));
        self.add_inst(InstEnt::new(inst).value(value));

        Some(value)
    }

    fn global_access(&mut self, tir: Tir) -> ExprValue {
        let TirEnt {
            ty,
            kind: TirKind::GlobalAccess(global),
            ..
        } = self.tir_data.ents[tir] else {
            unreachable!();
        };

        let value = self.add_value(ValueEnt::new(ty).flags(MirFlags::POINTER));

        let kind = InstKind::GlobalAccess(global);
        self.add_inst(InstEnt::new(kind).value(value));

        Some(value)
    }

    fn args(&mut self, args: TirList) -> ValueList {
        for &tir in self.tir_data.cons.get(args) {
            let expr = self.expr(tir, &mut None).unwrap();
            self.func_ctx.value_slices.push_one(expr);
        }
        self.func_ctx.value_slices.pop_frame()
    }

    fn gen_offset(&mut self, value: Value, dest_ty: Ty, offset: Offset) -> Value {
        let mut ent = self.func_ctx.values[value];
        ent.flags &= !MirFlags::ASSIGNABLE;
        ent.offset = ent.offset + offset;
        ent.ty = dest_ty;
        let result = self.add_value(ent);

        let kind = InstKind::Offset(value);
        let ent = InstEnt::new(kind).value(result);
        self.add_inst(ent);

        result
    }

    fn gen_drops(&mut self, drops: TirList) {
        for &drop in self.tir_data.cons.get(drops) {
            let drop = self.expr(drop, &mut None).unwrap();
            self.gen_drop(drop);
        }
    }

    fn gen_drop(&mut self, drop: Value) {
        let mut frontier = vec![drop];

        while let Some(drop) = frontier.pop() {
            let ValueEnt { ty, .. } = self.func_ctx.values[drop];
            let base_ty = self.types.base_of(ty);
            let TyEnt { kind, .. } = self.types[base_ty];
            if let TyKind::Enum(..) = kind {
                continue;
            }

            if let Ok(bound_impl) = bound_checker!(self).drop_impl(ty) {
                let prt = {
                    let mut_ptr = pointer_of(ty, true, self.types, self.ty_instances);
                    self.reprs[mut_ptr].repr = self.ptr_ty;

                    let value = self.add_value(ValueEnt::new(mut_ptr));
                    let kind = InstKind::TakePtr(drop);
                    self.func_ctx.add_inst(InstEnt::new(kind).value(value));
                    value
                };

                let drop_fn = {
                    let drop_fn_index = 0;
                    let drop_fn = self.func_lists.get(bound_impl.funcs)[drop_fn_index];
                    let mut param_slots = self.vec_pool.of_size(
                        Ty::reserved_value(),
                        self.ty_lists.len_of(bound_impl.params),
                    );
                    infer_parameters(
                        ty,
                        bound_impl.ty,
                        &mut param_slots,
                        Default::default(),
                        self.types,
                        self.ty_lists,
                    )
                    .unwrap();
                    self.instantiate_func(drop_fn, bound_impl.params, &param_slots)
                };

                let args = self.func_ctx.value_slices.push(&[prt]);
                let kind = InstKind::Call(drop_fn, args);
                self.func_ctx.add_inst(InstEnt::new(kind));
            }

            for field in self.repr_fields.get(self.reprs[ty].fields) {
                let value = self.gen_offset(drop, field.ty, field.offset);
                frontier.push(value);
            }
        }
    }

    fn gen_assign(&mut self, value: Value, dest: Dest) {
        if let Some(dest) = dest.take() {
            let kind = InstKind::Assign(value);
            self.add_inst(InstEnt::new(kind).value(dest));
        }
    }

    fn add_value(&mut self, ent: ValueEnt) -> Value {
        self.func_ctx.values.push(ent)
    }

    fn add_inst(&mut self, ent: InstEnt) -> Inst {
        self.func_ctx.add_inst(ent)
    }
}

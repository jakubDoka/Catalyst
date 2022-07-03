use cranelift_codegen::packed_option::PackedOption;

use crate::*;
use instance_types::*;
use storage::*;
use typec_types::*;
use lexer::*;

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

            // for &used in self.ty_lists.get(self.tir_data.used_types) {
            //     print!("{} ", ty_display!(self, used));
            // }
            // println!();

            for &tir in self.tir_data.cons.get(args) {
                let ty = self.tir_data.ents[tir].ty;
                let on_stack = self.reprs[ty].flags.contains(ReprFlags::ON_STACK);
                // print!("{} ", ty_display!(self, ty));
                let value = self.add_value(ValueEnt::new(ty).flags(MirFlags::POINTER & on_stack));
                self.func_ctx.value_slices.push_one(value);
                self.mir_builder_context.seen.insert(tir);
                self.mir_builder_context.tir_mapping[tir] = value.into();
            }
            // println!();

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

            TirKind::Match(..) => self.r#match(tir, dest),
            TirKind::MultiEntryBlock(..) => self.multi_entry_block(tir, dest),

            TirKind::Uninit => self.uninit(tir),

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

    fn uninit(&mut self, tir: Tir) -> ExprValue {
        let ty = self.tir_data.ents[tir].ty;
        let value = self.add_value(ValueEnt::new(ty));
        self.add_inst(InstEnt::new(InstKind::Uninit).value(value));
        Some(value)
    }

    fn r#match(&mut self, tir: Tir, dest: Dest) -> ExprValue {
        let TirEnt { kind: TirKind::Match(expr, branches), ty, flags, .. } = self.tir_data.ents[tir] else {
            unreachable!();
        };

        self.expr(expr, &mut None);

        let (value, block) = self.gen_join_block(flags, ty, dest);

        self.mir_builder_context
            .match_block_stack
            .push(block.into());

        self.expr(branches, dest);

        self.mir_builder_context.match_block_stack.pop().unwrap();

        if let Some(block) = block {
            self.func_ctx.select_block(block);
        }

        value
    }

    fn multi_entry_block(&mut self, tir: Tir, dest: Dest) -> ExprValue {
        let TirEnt { kind: TirKind::MultiEntryBlock(tir), .. } = self.tir_data.ents[tir] else {
            unreachable!();
        };

        if let Some(block) = self.mir_builder_context.match_block_mapping[tir].expand() {
            self.gen_jump(block, None);
            self.expr(tir, dest);
        } else {
            let block = self.func_ctx.create_block();
            self.gen_jump(block, None);
            self.func_ctx.select_block(block);
            let expr = self.expr(tir, dest);
            if !self.func_ctx.is_terminated() {
                let block = self
                    .mir_builder_context
                    .match_block_stack
                    .last()
                    .unwrap()
                    .unwrap();
                self.gen_jump(block, expr);
            }
            self.mir_builder_context.match_block_mapping[tir] = block.into();
        }

        None
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
        let TirEnt { kind: TirKind::Assign(lhs, rhs, drops), span, .. } = self.tir_data.ents[tir] else {
            unreachable!()
        };

        let a = self.expr(lhs, &mut None).unwrap();

        self.gen_drops(drops, span);

        self.expr(rhs, &mut Some(a)).unwrap();

        None
    }

    fn block(&mut self, tir: Tir, dest: Dest) -> ExprValue {
        let TirEnt { kind: TirKind::Block(stmts, drops), span, .. } = self.tir_data.ents[tir] else {
            unreachable!()
        };

        // println!("{}", _span.log(self.sources));

        let value = if let Some((&last, others)) = self.tir_data.cons.get(stmts).split_last() {
            for &stmt in others {
                self.expr(stmt, &mut None);
            }
            self.expr(last, dest)
        } else {
            None
        };

        self.gen_drops(drops, span);

        value
    }

    fn r#return(&mut self, tir: Tir) -> ExprValue {
        let TirEnt { 
            kind: TirKind::Return(value, drops), 
            span,
            .. 
        } = self.tir_data.ents[tir] else {
            unreachable!();
        };

        self.gen_drops(drops, span);

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
        let TirEnt { 
            kind: TirKind::Break(loop_header_marker, value, drops), 
            span,
            .. 
        } = self.tir_data.ents[tir] else {
            unreachable!()
        };

        self.gen_drops(drops, span);

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
        let TirEnt { 
            kind: TirKind::Continue(loop_header_marker, drops), 
            span, 
            .. 
        } = self.tir_data.ents[tir] else {
            unreachable!();
        };

        self.gen_drops(drops, span);

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
        let (mut result, mut join_block) = (None, None);
        let mut computed = false;

        let mut branch = |s: &mut Self, block: Block, tir: Tir| {
            s.func_ctx.select_block(block);
            let value = has_ret.then_some(s.expr(tir, &mut dest_copy)).flatten();
            if !s.func_ctx.is_terminated() {
                // we need to do this lazily since terminating flag does not
                // cover match branching case
                if !computed {
                    computed = true;
                    (result, join_block) = s.gen_join_block(flags, ty, dest);
                }

                if let Some(join_block) = join_block {
                    s.gen_jump(join_block, value);
                }
            }
        };

        let cond = self.expr(cond, &mut None).unwrap();

        for &pre_compute in self.tir_data.cons.get(pre_computes) {
            self.expr(pre_compute, &mut None).unwrap();
        }

        self.gen_switch(cond, then_block, otherwise_block);

        branch(self, then_block, then);
        branch(self, otherwise_block, otherwise);

        if let Some(join_block) = join_block {
            self.func_ctx.select_block(join_block);
        }

        result
    }

    fn gen_switch(&mut self, cond: Value, then: Block, otherwise: Block) {
        let kind = InstKind::JumpIfFalse(otherwise);
        let ent = InstEnt::new(kind).value(cond);
        self.add_inst(ent);
        self.gen_jump(then, None);
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
            // span,
            ..
        } = self.tir_data.ents[tir] else {
            unreachable!()
        };

        let base_ty = self.tir_data.ents[base].ty;
        let base = self.expr(base, &mut None).unwrap();

        let TyCompEnt { index, .. } = self.ty_comps[field];
        let fields = self.reprs[base_ty].fields;
        // println!("{} {}", ty_display!(self, base_ty), span.log(self.sources));
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

                let kind = InstKind::IntLit(0);
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
            span,
            ..
        } = self.tir_data.ents[tir] else {
            unreachable!();
        };

        let mut param_slots = self.vec_pool.get();
        if let FuncKind::Bound(bound, index) = self.funcs[func.meta()].kind {
            let bound_impl = bound_checker!(self)
                .implements(bound, caller.unwrap(), false)
                .unwrap();
            let param_slice = self.ty_lists.get(bound_impl.params);
            param_slots.resize(param_slice.len(), None);

            bound_checker!(self)
                .infer_parameters(
                    caller.unwrap(),
                    bound_impl.ty,
                    &mut param_slots,
                    param_slice,
                    Default::default(),
                    false,
                )
                .unwrap();

            func = self.func_lists.get(bound_impl.funcs)[index as usize];
        }

        if flags.contains(TirFlags::GENERIC) {
            // println!("{}", span.log(self.sources));
            let param_slots = self
                .vec_pool
                .alloc_iter(param_slots.drain(..).map(Option::unwrap));
            func = self.instantiate_func(func, params, &param_slots);
        }

        self.call_low(|args| InstKind::Call(func, params, args, span), ty, args, dest)
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

        // print!("{:?} {}", id, self.sources.display(self.funcs[func.meta()].name));
        // for &ty in global_params.iter().chain(self.ty_lists.get(params)) {
        //     print!(" {}", ty_display!(self, ty));
        // }
        // println!();

        if let Some(&instance) = self.func_instances.get(id) {
            instance
        } else {
            let instance = FuncEnt {
                id,
                flags: func_ent.flags & !FuncFlags::GENERIC,
            };
            let instance = self.funcs.push_header(instance, func);
            self.func_instances.insert(id, instance);
            let param_vec = self.vec_pool.alloc_iter(
                global_params
                    .iter()
                    .chain(self.ty_lists.get(params))
                    .map(|&ty| *self.ty_instances.get(self.types[ty].id).unwrap_or(&ty)),
            );
            if self.funcs[func.meta()].kind != FuncKind::Builtin {
                self.to_compile
                    .push((instance, self.ty_lists.push(&param_vec)));
            }
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
            TirKind::IntLit(v) => InstKind::IntLit(v),
            TirKind::BoolLit(v) => InstKind::BoolLit(v),
            TirKind::CharLit(v) => InstKind::IntLit(v as u128),
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

    fn gen_drops(&mut self, drops: TirList, span: Span) {
        for &drop in self.tir_data.cons.get(drops) {
            let drop = self.expr(drop, &mut None).unwrap();
            self.gen_drop(drop, span);
        }
    }

    fn gen_drop(&mut self, drop: Value, span: Span) {
        let ValueEnt { ty, .. } = self.func_ctx.values[drop];
        let base_ty = self.types.base_of(ty);
        let TyEnt { kind, .. } = self.types[base_ty];

        let ty = self
            .ty_instances
            .get(self.types[ty].id)
            .cloned()
            .unwrap_or(ty);

        if let Ok(bound_impl) = bound_checker!(self).drop_impl(ty) {
            let prt = {
                let mut_ptr = ty_factory!(self).pointer_of(ty, true);
                self.reprs[mut_ptr].repr = self.ptr_ty;

                let value = self.add_value(ValueEnt::new(mut_ptr));
                let kind = InstKind::TakePtr(drop);
                self.func_ctx.add_inst(InstEnt::new(kind).value(value));
                value
            };

            let drop_fn = {
                let drop_fn_index = 0;
                let drop_fn = self.func_lists.get(bound_impl.funcs)[drop_fn_index];
                let param_slice = self.ty_lists.get(bound_impl.params);
                let mut param_slots = self.vec_pool.of_size(None, param_slice.len());

                // for &param in param_slice {
                //     print!("{}", ty_display!(self, param));
                // }
                // println!();
                // println!("{} {}", ty_display!(self, ty), ty_display!(self, bound_impl.ty));

                bound_checker!(self)
                    .infer_parameters(
                        ty,
                        bound_impl.ty,
                        &mut param_slots,
                        param_slice,
                        Default::default(),
                        false,
                    )
                    .unwrap();

                let param_slots = self
                    .vec_pool
                    .alloc_iter(param_slots.drain(..).map(Option::unwrap));
                self.instantiate_func(drop_fn, bound_impl.params, &param_slots)
            };

            let args = self.func_ctx.value_slices.push(&[prt]);
            let kind = InstKind::Call(drop_fn, TyList::reserved_value(), args, span);
            self.func_ctx.add_inst(InstEnt::new(kind));
        }

        match kind {
            TyKind::Struct(..) => {
                for field in self.repr_fields.get(self.reprs[ty].fields) {
                    let value = self.gen_offset(drop, field.ty, field.offset);
                    self.gen_drop(value, span);
                }
            }
            TyKind::Enum(discriminant_ty, variants) => {
                let [flag, value] = self.repr_fields.get(self.reprs[ty].fields) else {
                    unreachable!();
                };
                let flag = self.gen_offset(drop, flag.ty, flag.offset);

                let mut then;
                let mut otherwise;
                // we construct end block anyway since enum with no variants cant be
                // constructed which implies it cant be dropped
                let end = self.func_ctx.create_block();
                // we skip first component which is the flag
                let Some((last, others)) = self.ty_comps.get(variants)[1..].split_last() else {
                    unreachable!();
                };

                for (i, variant) in others.iter().enumerate() {
                    then = self.func_ctx.create_block();
                    otherwise = self.func_ctx.create_block();

                    let cond = {
                        let cont_flag = {
                            let kind = InstKind::IntLit(i as u128);
                            let value = self.add_value(ValueEnt::new(discriminant_ty));
                            self.add_inst(InstEnt::new(kind).value(value));
                            value
                        };

                        let cmp_fn = {
                            let id = {
                                let left = self.types[discriminant_ty].id;
                                let op = ID::new("==");
                                ID::binary(left, op)
                            };
                            *self.func_instances.get(id).unwrap()
                        };

                        let args = self.func_ctx.value_slices.push(&[flag, cont_flag]);
                        let kind = InstKind::Call(cmp_fn, TyList::reserved_value(), args, span);
                        let value = self.add_value(ValueEnt::new(self.builtin_types.bool));
                        self.func_ctx.add_inst(InstEnt::new(kind).value(value));
                        value
                    };

                    self.gen_switch(cond, then, otherwise);

                    self.func_ctx.select_block(then);
                    let value = self.gen_offset(drop, variant.ty, value.offset);
                    self.gen_drop(value, span);
                    self.gen_jump(end, None);

                    self.func_ctx.select_block(otherwise);
                }

                let value = self.gen_offset(drop, last.ty, value.offset);
                self.gen_drop(value, span);
                self.gen_jump(end, None);

                self.func_ctx.select_block(end);
            }
            _ => (),
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

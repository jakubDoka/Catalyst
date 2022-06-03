use cranelift_codegen::{packed_option::PackedOption};

use instance_types::*;
use storage::*;
use typec_types::*;
use crate::*;

pub type ExprResult = errors::Result<Option<Value>>;

pub struct MirBuilderContext {
    tir_mapping: SecondaryMap<Tir, PackedOption<Value>>,
    match_block_mapping: SecondaryMap<Tir, PackedOption<Block>>,
    match_block_stack: Vec<PackedOption<Block>>,
}

impl MirBuilderContext {
    pub fn new() -> Self {
        Self {
            tir_mapping: SecondaryMap::new(),
            match_block_mapping: SecondaryMap::new(),
            match_block_stack: Vec::new(),
        }
    }

    pub fn clear(&mut self) {
        self.tir_mapping.clear();
        self.match_block_mapping.clear();
        self.match_block_stack.clear();
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
                let value = self.flagged_value_from_ty(sig.ret, mir::MirFlags::POINTER);
                self.return_dest = Some(value);
                self.func_ctx.value_slices.push_one(value);
            }

            for &tir in self.tir_data.cons.get(args) {
                let value = self.value(tir).unwrap();
                self.func_ctx.value_slices.push_one(value);
            }

            self.func_ctx.blocks[entry_point].params = self.func_ctx.value_slices.pop_frame();
        }
        self.func_ctx.select_block(entry_point);

        let value = self.expr(body, self.return_dest)?;

        if !self.func_ctx.is_terminated() {
            if sig.ret == self.builtin_types.nothing {
                self.func_ctx.add_inst(InstEnt::new(InstKind::Return, None));
            } else {
                self.func_ctx.add_inst(InstEnt::new(InstKind::Return, value));
            }
        }

        Ok(())
    }

    fn block(&mut self, tir: Tir, dest: Option<Value>) -> ExprResult {
        let TirKind::Block(stmts) = self.tir_data.ents[tir].kind else {
            unreachable!()
        };

        if stmts.is_reserved_value() {
            return Ok(None);
        }

        let stmts = self.tir_data.cons.get(stmts);
        if stmts.is_empty() {
            return Ok(None);
        }

        for &stmt in stmts[..stmts.len() - 1].iter() {
            self.expr(stmt, None)?;
        }

        let value = self.expr(stmts[stmts.len() - 1], dest)?;

        Ok(value)
    }

    fn expr(&mut self, tir: Tir, dest: Option<Value>) -> ExprResult {
        if let Some(value) = self.mir_builder_context.tir_mapping[tir].expand() {
            return Ok(Some(value));
        }

        let TirEnt { kind, ty, .. } = self.tir_data.ents[tir];
        let value = match kind {
            TirKind::Return(value) => self.r#return(value.expand())?,
            TirKind::Call(..) => self.call(tir, dest)?,
            TirKind::If(..) => self.r#if(tir, dest)?,
            TirKind::Variable(value) => self.variable(value)?,
            TirKind::Constructor(..) => self.constructor(tir, dest)?,
            TirKind::FieldAccess(..) => self.field_access(tir, dest)?,
            TirKind::Block(..) => self.block(tir, dest)?,
            TirKind::Loop(..) => self.r#loop(tir, dest)?,
            TirKind::Break(loop_header, value) => self.r#break(loop_header, value)?,
            TirKind::Assign(a, b) => self.assign(a, b)?,
            TirKind::Access(value) => self.expr(value, dest)?,
            TirKind::IntLit(value) => self.int_lit(ty, value, dest)?,
            TirKind::BoolLit(value) => self.bool_lit(ty, value, dest)?,
            TirKind::CharLit(value) => self.char_lit(ty, value, dest)?,
            TirKind::TakePtr(..) => self.take_pointer(tir, dest)?,
            TirKind::DerefPointer(..) => self.deref_pointer(tir, dest)?,
            TirKind::BitCast(..) => self.bit_cast(tir, dest)?,
            TirKind::Match(..) => self.r#match(tir, dest)?,
            TirKind::MatchBlock(..) => self.match_block(tir, dest)?,
            TirKind::GlobalAccess(..) => self.global_access(tir, dest)?,
            _ => unimplemented!("Unhandled Kind::{:?}", kind),
        };

        self.mir_builder_context.tir_mapping[tir] = value.into();

        Ok(value)
    }

    fn global_access(&mut self, tir: Tir, dest: Option<Value>) -> ExprResult {
        let TirEnt {
            kind: TirKind::GlobalAccess(global),
            ty,
            ..
        } = self.tir_data.ents[tir] else {
            unreachable!();
        };
        
        let on_stack = self.on_stack(tir);
        let dest = self.unwrap_dest_low(ty, on_stack, dest).or(dest);

        let value = self.flagged_value_from_ty(ty, MirFlags::POINTER);

        {
            let kind = InstKind::GlobalAccess(global);
            let ent = InstEnt::new(kind, value.into());
            self.func_ctx.add_inst(ent);
        }

        if on_stack {
            self.gen_assign(value, dest);
            Ok(dest)
        } else {
            Ok(Some(value))
        }
    }

    fn match_block(&mut self, tir: Tir, dest: Option<Value>) -> ExprResult {
        let TirEnt { kind: TirKind::MatchBlock(inner_block), .. } = self.tir_data.ents[tir] else {
            unreachable!()
        };

        if let Some(block) = self.mir_builder_context.match_block_mapping[inner_block].expand() {
            self.func_ctx
                .add_inst(InstEnt::new(InstKind::Jump(block), None));
        } else {
            let ret_block = self
                .mir_builder_context
                .match_block_stack
                .last()
                .map(|block| block.expand())
                .flatten();

            let block = self.func_ctx.create_block();
            self.mir_builder_context.match_block_mapping[inner_block] = block.into();

            self.func_ctx
                .add_inst(InstEnt::new(InstKind::Jump(block), None));

            self.func_ctx.select_block(block);
            let value = self.block(inner_block, dest)?;
            let value = (!self.on_stack(tir)).then_some(value).flatten();
            if let Some(ret_block) = ret_block {
                self.func_ctx
                    .add_inst(InstEnt::new(InstKind::Jump(ret_block), value));
            }
        }

        Ok(None)
    }

    fn r#match(&mut self, tir: Tir, dest: Option<Value>) -> ExprResult {
        let TirEnt { kind: TirKind::Match(expr, branches), .. } = self.tir_data.ents[tir] else {
            unreachable!()
        };

        let on_stack = self.on_stack(tir);
        let value = self.unwrap_dest(tir, dest).or(dest);

        self.expr(expr, None)?;
        let terminating = self.tir_data.ents[branches]
            .flags
            .contains(TirFlags::TERMINATING);
        let block = (!terminating).then(|| {
            let block = self.func_ctx.create_block();
            if let Some(value) = value && !on_stack {
                self.func_ctx.blocks[block].params = self.func_ctx.value_slices.push(&[value]);
            }
            block
        });
        self.mir_builder_context.match_block_stack.push(block.into());

        drop(self.expr(branches, dest));

        self.mir_builder_context.match_block_stack.pop();

        if let Some(block) = block {
            self.func_ctx.select_block(block);
        }

        Ok(value)
    }

    fn bit_cast(&mut self, tir: Tir, dest: Option<Value>) -> ExprResult {
        let TirEnt { kind: TirKind::BitCast(expr), ty, span, .. } = self.tir_data.ents[tir] else {
            unreachable!()
        };

        let expr_ty = self.tir_data.ents[expr].ty;

        if self.reprs[expr_ty].layout != self.reprs[ty].layout {
            self.diagnostics.push(InstError::InvalidBitCast {
                loc: span,
                instantiated_from: None,
                from: format!("{}", ty_display!(self, expr_ty)),
                from_layout: self.reprs[expr_ty].layout,
                to: format!("{}", ty_display!(self, ty)),
                to_layout: self.reprs[ty].layout,
            });
        }

        if let Some(dest) = dest && self.func_ctx.values[dest].flags.contains(MirFlags::POINTER) {
            return self.expr(expr, dest.into());
        }

        let value = self.expr(expr, None)?.unwrap();
        let flags = self.func_ctx.values[value].flags;

        let result = self.flagged_value_from_ty(ty, flags);

        let kind = InstKind::BitCast(value);
        let inst = InstEnt::new(kind, result.into());
        self.func_ctx.add_inst(inst);

        self.gen_assign(result, dest);

        Ok(Some(result))
    }

    fn take_pointer(&mut self, tir: Tir, dest: Option<Value>) -> ExprResult {
        let TirEnt { ty, kind: TirKind::TakePtr(value), .. } = self.tir_data.ents[tir] else {
            unreachable!();
        };

        let mir_value = self.expr(value, dest)?.unwrap();

        // // funny thing, any stack allocated value treated as
        // // value is pointer but pointer treated as pointer is value
        let value = self.value_from_ty(ty);

        {
            let kind = InstKind::TakePointer(mir_value);
            let inst = InstEnt::new(kind, value.into());
            self.func_ctx.add_inst(inst);
        }

        self.gen_assign(value, dest);

        self.mir_builder_context.tir_mapping[tir] = value.into();

        Ok(Some(value))
    }

    fn deref_pointer(&mut self, tir: Tir, dest: Option<Value>) -> ExprResult {
        let TirEnt { ty, kind: TirKind::DerefPointer(value), .. } = self.tir_data.ents[tir] else {
            unreachable!()
        };

        let mir_value = self.expr(value, dest)?.unwrap();

        let value = self.flagged_value_from_ty(ty, mir::MirFlags::POINTER);

        {
            let kind = InstKind::DerefPointer(mir_value);
            let inst = InstEnt::new(kind, value.into());
            self.func_ctx.add_inst(inst);
        }

        self.gen_assign(value, dest);

        self.mir_builder_context.tir_mapping[tir] = value.into();

        Ok(Some(value))
    }

    fn char_lit(
        &mut self,
        ty: Ty,
        literal_value: char,
        dest: Option<Value>,
    ) -> ExprResult {
        let value = self.gen_int_lit(ty, literal_value as u128);

        self.gen_assign(value, dest);

        Ok(Some(value))
    }

    fn assign(&mut self, a: Tir, b: Tir) -> ExprResult {
        let a = self.expr(a, None)?.unwrap();
        let b = self.expr(b, Some(a))?.unwrap();

        if a == b {
            return Ok(None);
        }

        self.gen_assign(b, Some(a));

        Ok(None)
    }

    fn r#break(&mut self, loop_header_marker: Tir, value: PackedOption<Tir>) -> ExprResult {
        let &Loop { exit, dest, .. } = self
            .func_ctx
            .loops
            .iter()
            .rev()
            .find(|loop_header| loop_header.marker == loop_header_marker)
            .unwrap();

        let value = if let Some(value) = value.expand() {
            dest.is_none()
                .then_some(self.expr(value, dest)?)
                .flatten()
        } else {
            None
        };

        {
            let kind = InstKind::Jump(exit);
            let ent = InstEnt::new(kind, value);
            self.func_ctx.add_inst(ent);
        }

        Ok(None)
    }

    fn r#loop(&mut self, tir: Tir, dest: Option<Value>) -> ExprResult {
        let TirEnt { ty, kind: TirKind::Loop(body), .. } = self.tir_data.ents[tir] else {
            unreachable!()
        };

        let enter_block = self.func_ctx.create_block();
        let exit_block = self.func_ctx.create_block();

        let on_stack = self.on_stack(tir);
        let value = self.unwrap_dest_low(ty, on_stack, dest);

        let loop_header = Loop {
            _enter: enter_block,
            exit: exit_block,
            marker: tir,
            dest,
        };

        {
            let kind = InstKind::Jump(enter_block);
            let inst = InstEnt::new(kind, None);
            self.func_ctx.add_inst(inst);
        }

        self.func_ctx.select_block(enter_block);

        self.func_ctx.loops.push(loop_header);

        self.block(body, None)?;

        self.func_ctx.loops.pop().unwrap();

        if !self.func_ctx.is_terminated() {
            let kind = InstKind::Jump(enter_block);
            let inst = InstEnt::new(kind, None);
            self.func_ctx.add_inst(inst);
        }

        if !on_stack && let Some(value) = value {
            let values = self.func_ctx.value_slices.push(&[value]);
            self.func_ctx.blocks[exit_block].params = values;
        }
        self.func_ctx.select_block(exit_block);

        Ok(value)
    }

    fn field_access(&mut self, tir: Tir, dest: Option<Value>) -> ExprResult {
        let TirEnt { ty, kind: TirKind::FieldAccess(base, field), .. } = self.tir_data.ents[tir] else {
            unreachable!()
        };
        let header = self.expr(base, None)?.unwrap();

        let offset = self.field_offset(header, field);

        self.access_offset(ty, header, offset, dest)
    }

    fn access_offset(
        &mut self,
        dest_ty: Ty,
        header: Value,
        offset: Offset,
        dest: Option<Value>,
    ) -> ExprResult {
        let value = self.offset_low(header, offset, Some(dest_ty));

        self.gen_assign(value, dest);

        Ok(Some(value))
    }

    fn bool_lit(
        &mut self,
        ty: Ty,
        literal_value: bool,
        dest: Option<Value>,
    ) -> ExprResult {
        let value = self.value_from_ty(ty);

        {
            let kind = InstKind::BoolLit(literal_value);
            let ent = InstEnt::new(kind, value.into());
            self.func_ctx.add_inst(ent);
        }

        self.gen_assign(value, dest);

        Ok(Some(value))
    }

    fn constructor(&mut self, tir: Tir, dest: Option<Value>) -> ExprResult {
        let TirEnt { ty, kind: TirKind::Constructor(data), .. } = self.tir_data.ents[tir] else {
            unreachable!()
        };

        let on_stack = self.on_stack(tir);
        let Some(mut value) = self.unwrap_dest_low(ty, on_stack, dest).or(dest) else {
            unreachable!();
        };

        if !on_stack && dest.is_none() {
            self.gen_int_lit_low(value, 0);
        }

        let data_view = self.tir_data.cons.get(data);
        if let TyKind::Enum(..) = self.types[ty].kind {
            let &[flag, constructor] = data_view else {
                unreachable!()
            };

            self.expr(flag, Some(value))?;

            let offset = self.repr_fields.get(self.reprs[ty].fields)[1].offset;

            let value = self.offset(value, offset);

            self.expr(constructor, Some(value))?;
        } else {
            for (i, &data) in data_view.iter().enumerate() {
                let offset = self.field_offset_by_index(ty, i);
                let dest = self.offset(value, offset);
                self.expr(data, Some(dest))?;
                if !on_stack {
                    value = self.offset(dest, Offset::ZERO - offset);
                }
            }
        }

        Ok(Some(value))
    }

    fn variable(&mut self, tir: Tir) -> ExprResult {
        let assignable = self.tir_data.ents[tir].flags.contains(TirFlags::ASSIGNABLE);
        let on_stack = self.on_stack(tir);

        let dest = if on_stack {
            self.unwrap_dest(tir, None)
        } else {
            None
        };

        let value = self.expr(tir, dest)?;

        {
            let kind = InstKind::Variable;
            let ent = InstEnt::new(kind, value);
            self.func_ctx.add_inst(ent);
        }

        self.func_ctx.values[value.unwrap()]
            .flags
            .insert(mir::MirFlags::ASSIGNABLE & assignable);

        Ok(None)
    }

    fn int_lit(
        &mut self,
        ty: Ty,
        literal_value: u128,
        dest: Option<Value>,
    ) -> ExprResult {
        let value = self.gen_int_lit(ty, literal_value);

        self.gen_assign(value, dest);

        Ok(Some(value))
    }

    fn r#if(&mut self, tir: Tir, dest: Option<Value>) -> ExprResult {
        let TirEnt { ty, kind: TirKind::If(cond, then, otherwise), .. } = self.tir_data.ents[tir] else {
            unreachable!();
        };

        let cond = self.expr(cond, None)?;

        let on_stack = self.on_stack(tir);
        let value = self.unwrap_dest_low(ty, on_stack, dest);
        let block_dest = on_stack.then_some(value.or(dest)).flatten();
        let no_value = ty == self.builtin_types.nothing;

        let then_block = self.func_ctx.create_block();
        let otherwise_block = otherwise.is_some().then(|| self.func_ctx.create_block());
        let mut skip_block = None;

        let mut get_skip_block = |s: &mut Self| {
            skip_block.unwrap_or_else(|| {
                let b = s.func_ctx.create_block();
                skip_block = Some(b);
                b
            })
        };

        {
            let kind =
                InstKind::JumpIfFalse(otherwise_block.unwrap_or_else(|| get_skip_block(self)));
            let ent = InstEnt::new(kind, cond);
            self.func_ctx.add_inst(ent);
        }

        {
            let kind = InstKind::Jump(then_block);
            let ent = InstEnt::new(kind, None);
            self.func_ctx.add_inst(ent);
        }

        {
            self.func_ctx.select_block(then_block);
            let value = {
                let value = self.expr(then, block_dest)?;
                (!on_stack && !no_value).then_some(value).flatten()
            };

            if !self.func_ctx.is_terminated() {
                let kind = InstKind::Jump(get_skip_block(self));
                let ent = InstEnt::new(kind, value);
                self.func_ctx.add_inst(ent);
            }
        }

        if let Some(block) = otherwise_block {
            self.func_ctx.select_block(block);
            let value = {
                let value = self.expr(otherwise.unwrap(), block_dest)?;
                (!on_stack && !no_value).then_some(value).flatten()
            };

            if !self.func_ctx.is_terminated() {
                let kind = InstKind::Jump(get_skip_block(self));
                let ent = InstEnt::new(kind, value);
                self.func_ctx.add_inst(ent);
            }
        }

        if !on_stack && let Some(value) = value {
            let values = self.func_ctx.value_slices.push(&[value]);
            if let Some(skip_block) = skip_block {
                self.func_ctx.blocks[skip_block].params = values;
            }
        };

        if let Some(skip_block) = skip_block {
            self.func_ctx.select_block(skip_block);
        }

        Ok(value)
    }

    fn call(&mut self, tir: Tir, dest: Option<Value>) -> ExprResult {
        let TirEnt { ty, kind: TirKind::Call(caller, params, mut func, args), flags, .. } = self.tir_data.ents[tir] else {
            unreachable!();
        };

        let param_slice = self.ty_lists.get(params);

        // dispatch bound call
        if let FuncKind::Bound(bound, index) = self.funcs[func.meta()].kind {
            let id = {
                let bound = self.types[bound].id;
                let implementor = self.types[caller.unwrap()].id;
                ID::bound_impl(bound, implementor)
            };

            let funcs = self.bound_impls.get(id).unwrap().funcs;
            func = self.func_lists.get(funcs)[index as usize]
        }

        let func_ent = self.funcs[func];

        // instantiate generic function
        if flags.contains(TirFlags::GENERIC) {
            let id = param_slice
                .iter()
                .fold(func_ent.id, |acc, &ty| acc + self.types[ty].id);

            func = if let Some(&instance) = self.func_instances.get(id) {
                instance
            } else {
                let instance = FuncEnt {
                    id,
                    flags: func_ent.flags & !FuncFlags::GENERIC,
                };
                let instance = self.funcs.push_instance(instance, func);
                self.func_instances.insert(id, instance);
                self.to_compile.push((instance, params));
                instance
            };
        }

        let on_stack = self.on_stack(tir);
        let has_sret = self.reprs[ty].flags.contains(ReprFlags::ON_STACK);

        let filtered_dest = on_stack.then_some(dest).flatten();
        let value = self
            .unwrap_dest_low(ty, on_stack, filtered_dest)
            .or(filtered_dest);

        let args = {
            let args_view = self.tir_data.cons.get(args);

            self.func_ctx.value_slices.mark_frame();

            if has_sret {
                self.func_ctx.value_slices.push_one(value.unwrap());
            }

            for &arg in args_view {
                let value = self.expr(arg, None)?.unwrap();
                self.func_ctx.value_slices.push_one(value);
            }
            self.func_ctx.value_slices.pop_frame()
        };

        if on_stack && !has_sret {
            let temp = {
                let kind = InstKind::Call(func, args);
                let value = self.value_from_ty(ty);
                let ent = InstEnt::new(kind, value.into());
                self.func_ctx.add_inst(ent);
                value
            };

            self.gen_assign(temp, value);
        } else {
            let kind = InstKind::Call(func, args);
            let inst = InstEnt::new(kind, value);
            self.func_ctx.add_inst(inst);

            if let Some(value) = value {
                self.gen_assign(value, dest);
            }
        }

        Ok(value)
    }

    fn unwrap_dest(&mut self, tir: Tir, dest: Option<Value>) -> Option<Value> {
        let on_stack = self.on_stack(tir);
        let ret = self.tir_data.ents[tir].ty;
        self.unwrap_dest_low(ret, on_stack, dest)
    }

    fn unwrap_dest_low(&mut self, ret: Ty, on_stack: bool, dest: Option<Value>) -> Option<Value> {
        let has_ret = ret != self.builtin_types.nothing;
        (dest.is_none() && has_ret).then(|| {
            if on_stack {
                // stack needs to be allocated to pass valid pointer
                let stack = {
                    let size = self.reprs[ret].layout.size();
                    let ent = StackEnt::new(size, self.ptr_ty);
                    self.func_ctx.stacks.push(ent)
                };

                let value = self.flagged_value_from_ty(ret, mir::MirFlags::POINTER);

                {
                    let kind = InstKind::StackAddr(stack);
                    let ent = InstEnt::new(kind, value.into());
                    self.func_ctx.add_inst(ent);
                }

                value
            } else {
                // no need to allocate, register is enough
                self.value_from_ty(ret)
            }
        })
    }

    fn gen_int_lit(&mut self, ty: Ty, literal_value: u128) -> Value {
        let value = self.value_from_ty(ty);
        self.gen_int_lit_low(value, literal_value);
        value
    }

    fn gen_int_lit_low(&mut self, value: Value, literal_value: u128) {
        let kind = InstKind::IntLit(literal_value);
        let ent = InstEnt::new(kind, value.into());
        self.func_ctx.add_inst(ent);
    }

    fn field_offset(&self, header: Value, field: TyComp) -> Offset {
        let header_ty = self.func_ctx.values[header].ty;
        self.field_offset_low(header_ty, field)
    }

    fn field_offset_low(&self, header_ty: Ty, field: TyComp) -> Offset {
        let TyCompEnt { index, .. } = self.ty_comps[field];
        self.field_offset_by_index(header_ty, index as usize)
    }

    fn field_offset_by_index(&self, header_ty: Ty, index: usize) -> Offset {
        let fields = self.reprs[header_ty].fields;
        self.repr_fields.get(fields)[index as usize].offset
    }

    fn offset(&mut self, value: Value, offset: Offset) -> Value {
        self.offset_low(value, offset, None)
    }

    fn offset_low(&mut self, value: Value, offset: Offset, ty: Option<Ty>) -> Value {
        let of_value = {
            let mut value = self.func_ctx.values[value];
            value.offset = value.offset + offset;
            if let Some(ty) = ty {
                value.ty = ty;
            }
            self.func_ctx.values.push(value)
        };

        {
            let kind = InstKind::Offset(value);
            let ent = InstEnt::new(kind, of_value.into());
            self.func_ctx.add_inst(ent);
        }

        of_value
    }

    fn gen_assign(&mut self, value: Value, dest: Option<Value>) {
        if let Some(dest) = dest {
            let kind = InstKind::Assign(value);
            let ent = InstEnt::new(kind, dest.into());
            self.func_ctx.add_inst(ent);
        }
    }

    fn on_stack(&self, tir: Tir) -> bool {
        let TirEnt { ty, flags, .. } = self.tir_data.ents[tir];
        self.reprs[ty].flags.contains(ReprFlags::ON_STACK)
            || flags.contains(TirFlags::SPILLED)
    }

    fn r#return(&mut self, value: Option<Tir>) -> ExprResult {
        let value = if let Some(value) = value {
            let dest = self.return_dest;
            self.expr(value, dest)?
        } else {
            None
        };

        {
            let kind = InstKind::Return;
            let ent = InstEnt::new(kind, value);
            self.func_ctx.add_inst(ent);
        }

        Ok(None)
    }

    fn value(&mut self, tir: Tir) -> Option<Value> {
        let TirEnt { ty, .. } = self.tir_data.ents[tir];
        if ty == self.builtin_types.nothing {
            return None;
        }
        let flags = MirFlags::POINTER & self.reprs[ty].flags.contains(ReprFlags::ON_STACK);
        let value = self.flagged_value_from_ty(ty, flags);
        self.mir_builder_context.tir_mapping[tir] = value.into();
        Some(value)
    }

    fn value_from_ty(&mut self, ty: Ty) -> Value {
        self.flagged_value_from_ty(ty, Default::default())
    }

    fn flagged_value_from_ty(&mut self, ty: Ty, flags: mir::MirFlags) -> Value {
        let value = ValueEnt::flags(ty, flags);
        self.func_ctx.values.push(value)
    }
}

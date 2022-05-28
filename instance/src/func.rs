


use cranelift_codegen::ir::{Type};
use cranelift_codegen::{isa::CallConv, packed_option::PackedOption};

use errors::*;
use instance_types::*;
use lexer::*;
use storage::*;
use typec_types::*;

pub type ExprResult = errors::Result<Option<Value>>;
pub type PatternStacks = Vec<Vec<Tir>>;


pub struct MirBuilderContext {
    tir_mapping: SecondaryMap<Tir, PackedOption<Value>>,
}

impl MirBuilderContext {
    pub fn new() -> Self {
        Self {
            tir_mapping: SecondaryMap::new(),
        }
    }

    pub fn clear(&mut self) {
        self.tir_mapping.clear();
    }
}

pub struct MirBuilder<'a> {
    pub func_id: Func,
    pub ptr_ty: Type,
    pub system_call_convention: CallConv,
    
    pub reprs: &'a Reprs,
    pub types: &'a Types,
    pub ty_lists: &'a TyLists,
    pub func_lists: &'a TFuncLists,
    pub ty_comps: &'a TyComps,
    pub bound_impls: &'a BoundImpls,
    pub repr_fields: &'a ReprFields,
    pub builtin_types: &'a BuiltinTypes,
    pub funcs: &'a mut Funcs,
    pub func: &'a mut FuncCtx,
    pub body: &'a TirData,
    pub return_dest: Option<Value>,
    pub sources: &'a Sources,
    pub diagnostics: &'a mut Diagnostics,
    pub ctx: &'a mut MirBuilderContext,
    pub func_meta: &'a FuncMeta,
    pub pattern_stacks: &'a mut  PatternStacks,
}

impl MirBuilder<'_> {
    pub fn translate_func(&mut self) -> errors::Result {
        self.ctx.clear();
        self.func.clear();
        
        let FuncMetaData {
            sig,
            body,
            args,
            ..
        } = self.func_meta[self.func_id];

        let entry_point = self.func.create_block();
        {
            self.func.value_slices.mark_frame();

            let has_sret = {
                let ret = sig.ret;
                self.reprs[ret].flags.contains(ReprFlags::ON_STACK)
            };

            if has_sret {
                let value = self.flagged_value_from_ty(sig.ret, mir::MirFlags::POINTER);
                self.return_dest = Some(value);
                self.func.value_slices.push_one(value);
            }

            for &tir in self.body.cons.get(args) {
                let value = self.translate_value(tir).unwrap();
                self.func.value_slices.push_one(value);
            }

            self.func.blocks[entry_point].params = self.func.value_slices.pop_frame();
        }
        self.func.select_block(entry_point);

        let value = self.translate_block(body, self.return_dest)?;

        if !self.func.is_terminated() {
            if sig.ret == self.builtin_types.nothing {
                self.func.add_inst(InstEnt::new(InstKind::Return, None));
            } else {
                self.func.add_inst(InstEnt::new(InstKind::Return, value));
            }
        }

        Ok(())
    }

    fn translate_block(&mut self, tir: Tir, dest: Option<Value>) -> ExprResult {
        let TirKind::Block(stmts) = self.body.ents[tir].kind else {
            unreachable!()
        };

        if stmts.is_reserved_value() {
            return Ok(None);
        }

        let stmts = self.body.cons.get(stmts);
        if stmts.is_empty() {
            return Ok(None);
        }

        for &stmt in stmts[..stmts.len() - 1].iter() {
            self.translate_expr(stmt, None)?;
        }

        let value = self.translate_expr(stmts[stmts.len() - 1], dest)?;

        Ok(value)
    }

    fn translate_expr(&mut self, tir: Tir, dest: Option<Value>) -> ExprResult {
        if let Some(value) = self.ctx.tir_mapping[tir].expand() {
            return Ok(Some(value));
        }

        let TirEnt { kind, ty, .. } = self.body.ents[tir];
        let value = match kind {
            TirKind::Return(value) => self.translate_return(value.expand())?,
            TirKind::Call(..) => self.translate_call(tir, dest)?,
            TirKind::If(..) => self.translate_if(tir, dest)?,
            TirKind::Variable(value) => self.translate_variable(value)?,
            TirKind::Constructor(..) => self.translate_constructor(tir, dest)?,
            TirKind::FieldAccess(..) => self.translate_field_access(tir, dest)?,
            TirKind::Block(..) => self.translate_block(tir, dest)?,
            TirKind::Loop(..) => self.translate_loop(tir, dest)?,
            TirKind::Break(loop_header, value) => self.translate_break(loop_header, value)?,
            TirKind::Assign(a, b) => self.translate_assign(a, b)?,
            TirKind::Access(value) => self.translate_expr(value, dest)?,
            TirKind::IntLit(value) => self.translate_int_lit(ty, value, dest)?,
            TirKind::BoolLit(value) => self.translate_bool_lit(ty, value, dest)?,
            TirKind::CharLit(value) => self.translate_char_lit(ty, value, dest)?,
            TirKind::TakePtr(..) => self.translate_take_pointer(tir, dest)?,
            TirKind::DerefPointer(..) => self.translate_deref_pointer(tir, dest)?,
            TirKind::BitCast(..) => self.translate_bit_cast(tir, dest)?,
            _ => todo!("Unhandled Kind::{:?}", kind),
        };

        self.ctx.tir_mapping[tir] = value.into();

        Ok(value)
    }
    
    fn translate_bit_cast(
        &mut self,
        tir: Tir,
        dest: Option<Value>,
    ) -> ExprResult {
        let TirEnt { kind: TirKind::BitCast(expr), ty, span, .. } = self.body.ents[tir] else {
            unreachable!()
        };

        let expr_ty = self.body.ents[expr].ty;

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

        if let Some(dest) = dest && self.func.values[dest].flags.contains(MirFlags::POINTER) {
            return self.translate_expr(expr, dest.into());
        }

        let value = self.translate_expr(expr, None)?.unwrap();
        let flags = self.func.values[value].flags;

        let result = self.flagged_value_from_ty(ty, flags);

        let kind = InstKind::BitCast(value);
        let inst = InstEnt::new(kind, result.into());
        self.func.add_inst(inst);

        self.assign(result, dest);

        Ok(Some(result))
    }

    fn translate_take_pointer(
        &mut self,
        tir: Tir,
        dest: Option<Value>,
    ) -> ExprResult {
        let TirEnt { ty, kind: TirKind::TakePtr(value), .. } = self.body.ents[tir] else {
            unreachable!();
        };

        let mir_value = self.translate_expr(value, dest)?.unwrap();

        // // funny thing, any stack allocated value treated as
        // // value is pointer but pointer treated as pointer is value
        let value = self.value_from_ty(ty);

        {
            let kind = InstKind::TakePointer(mir_value);
            let inst = InstEnt::new(kind, value.into());
            self.func.add_inst(inst);
        }

        self.assign(value, dest);

        self.ctx.tir_mapping[tir] = value.into();

        Ok(Some(value))
    }

    fn translate_deref_pointer(
        &mut self,
        tir: Tir,
        dest: Option<Value>,
    ) -> ExprResult {
        let TirEnt { ty, kind: TirKind::DerefPointer(value), .. } = self.body.ents[tir] else {
            unreachable!()
        };

        let mir_value = self.translate_expr(value, dest)?.unwrap();

        let value = self.flagged_value_from_ty(ty, mir::MirFlags::POINTER);

        {
            let kind = InstKind::DerefPointer(mir_value);
            let inst = InstEnt::new(kind, value.into());
            self.func.add_inst(inst);
        }

        self.assign(value, dest);

        self.ctx.tir_mapping[tir] = value.into();

        Ok(Some(value))
    }

    fn translate_char_lit(
        &mut self,
        ty: Ty,
        literal_value: char,
        dest: Option<Value>,
    ) -> ExprResult {
        let value = self.int_lit(ty, literal_value as i64);

        self.assign(value, dest);

        Ok(Some(value))
    }

    fn translate_assign(&mut self, a: Tir, b: Tir) -> ExprResult {
        let a = self.translate_expr(a, None)?.unwrap();
        let b = self.translate_expr(b, Some(a))?.unwrap();

        if a == b {
            return Ok(None);
        }

        self.assign(b, Some(a));

        Ok(None)
    }

    fn translate_break(
        &mut self,
        loop_header_marker: Tir,
        value: PackedOption<Tir>,
    ) -> ExprResult {
        let &Loop { exit, dest, .. } = self
            .func
            .loops
            .iter()
            .rev()
            .find(|loop_header| loop_header.marker == loop_header_marker)
            .unwrap();

        let value = if let Some(value) = value.expand() {
            dest.is_none()
                .then_some(self.translate_expr(value, dest)?)
                .flatten()
        } else {
            None
        };

        {
            let kind = InstKind::Jump(exit);
            let ent = InstEnt::new(kind, value);
            self.func.add_inst(ent);
        }

        Ok(None)
    }

    fn translate_loop(&mut self, tir: Tir, dest: Option<Value>) -> ExprResult {
        let TirEnt { ty, kind: TirKind::Loop(body), .. } = self.body.ents[tir] else {
            unreachable!()
        };

        let enter_block = self.func.create_block();
        let exit_block = self.func.create_block();

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
            self.func.add_inst(inst);
        }

        self.func.select_block(enter_block);

        self.func.loops.push(loop_header);

        self.translate_block(body, None)?;

        self.func.loops.pop().unwrap();

        if !self.func.is_terminated() {
            let kind = InstKind::Jump(enter_block);
            let inst = InstEnt::new(kind, None);
            self.func.add_inst(inst);
        }

        if !on_stack && let Some(value) = value {
            let values = self.func.value_slices.push(&[value]);
            self.func.blocks[exit_block].params = values;
        }
        self.func.select_block(exit_block);

        Ok(value)
    }

    fn translate_field_access(
        &mut self,
        tir: Tir,
        dest: Option<Value>,
    ) -> ExprResult {
        let TirEnt { ty, kind: TirKind::FieldAccess(base, field), .. } = self.body.ents[tir] else {
            unreachable!()
        };
        let header = self.translate_expr(base, None)?.unwrap();

        let offset = self.field_offset(header, field);
        
        self.access_offset(ty, header, offset, dest)
    }

    fn access_offset(&mut self, dest_ty: Ty, header: Value, offset: Offset, dest: Option<Value>) -> ExprResult {
        let value = self.offset_low(header, offset, Some(dest_ty));

        self.assign(value, dest);

        Ok(Some(value))
    }

    fn translate_bool_lit(
        &mut self,
        ty: Ty,
        literal_value: bool,
        dest: Option<Value>,
    ) -> ExprResult {
        let value = self.value_from_ty(ty);

        {
            let kind = InstKind::BoolLit(literal_value);
            let ent = InstEnt::new(kind, value.into());
            self.func.add_inst(ent);
        }

        self.assign(value, dest);

        Ok(Some(value))
    }

    fn translate_constructor(
        &mut self,
        tir: Tir,
        dest: Option<Value>,
    ) -> ExprResult {
        let TirEnt { ty, kind: TirKind::Constructor(data), .. } = self.body.ents[tir] else {
            unreachable!()
        };

        let on_stack = self.on_stack(tir);
        let Some(mut value) = self.unwrap_dest_low(ty, on_stack, dest).or(dest) else {
            unreachable!();
        };

        if !on_stack && dest.is_none() {
            self.int_lit_low(value, 0);
        }

        let data_view = self.body.cons.get(data);
        if let TyKind::Enum(..) = self.types[ty].kind {
            let &[flag, constructor] = data_view else {
                unreachable!()
            };

            self.translate_expr(flag, Some(value))?;

            let offset = self.repr_fields.get(self.reprs[ty].fields)[0].offset;

            let value = self.offset(value, offset);

            self.translate_expr(constructor, Some(value))?;
        } else {
            for (i, &data) in data_view.iter().enumerate() {
                let offset = self.field_offset_by_index(ty, i);
                let dest = self.offset(value, offset);
                self.translate_expr(data, Some(dest))?;
                if !on_stack {
                    value = self.offset(dest, Offset::ZERO - offset);
                }
            }
        }

        Ok(Some(value))
    }

    fn translate_variable(&mut self, tir: Tir) -> ExprResult {
        let assignable = self.body.ents[tir].flags.contains(TirFlags::ASSIGNABLE);
        let on_stack = self.on_stack(tir);

        let dest = if on_stack {
            self.unwrap_dest(tir, None)
        } else {
            None
        };

        let value = self.translate_expr(tir, dest)?;

        {
            let kind = InstKind::Variable;
            let ent = InstEnt::new(kind, value);
            self.func.add_inst(ent);
        }

        self.func.values[value.unwrap()]
            .flags
            .insert(mir::MirFlags::ASSIGNABLE & assignable);

        Ok(None)
    }

    fn translate_int_lit(
        &mut self,
        ty: Ty,
        literal_value: i64,
        dest: Option<Value>,
    ) -> ExprResult {
        let value = self.int_lit(ty, literal_value);

        self.assign(value, dest);

        Ok(Some(value))
    }

    fn translate_if(&mut self, tir: Tir, dest: Option<Value>) -> ExprResult {
        let TirEnt { ty, kind: TirKind::If(cond, then, otherwise), .. } = self.body.ents[tir] else {
            unreachable!();
        };

        let cond = self.translate_expr(cond, None)?;

        let on_stack = self.on_stack(tir);
        let value = self.unwrap_dest_low(ty, on_stack, dest);
        let block_dest = on_stack.then_some(value.or(dest)).flatten();
        let no_value = ty == self.builtin_types.nothing;

        let then_block = self.func.create_block();
        let otherwise_block = otherwise.is_some().then(|| self.func.create_block());
        let skip_block = self.func.create_block();

        {
            let kind = InstKind::JumpIfFalse(otherwise_block.unwrap_or(skip_block));
            let ent = InstEnt::new(kind, cond);
            self.func.add_inst(ent);
        }

        {
            let kind = InstKind::Jump(then_block);
            let ent = InstEnt::new(kind, None);
            self.func.add_inst(ent);
        }

        {
            self.func.select_block(then_block);
            let value = {
                let value = self.translate_block(then, block_dest)?;
                (!on_stack && !no_value).then_some(value).flatten()
            };

            if !self.func.is_terminated() {
                let kind = InstKind::Jump(skip_block);
                let ent = InstEnt::new(kind, value);
                self.func.add_inst(ent);
            }
        }

        if let Some(block) = otherwise_block {
            self.func.select_block(block);
            let value = {
                let value = self.translate_block(otherwise.unwrap(), block_dest)?;
                (!on_stack && !no_value).then_some(value).flatten()
            };

            if !self.func.is_terminated() {
                let kind = InstKind::Jump(skip_block);
                let ent = InstEnt::new(kind, value);
                self.func.add_inst(ent);
            }
        }

        if !on_stack && let Some(value) = value {
            let values = self.func.value_slices.push(&[value]);
            self.func.blocks[skip_block].params = values;
        };

        self.func.select_block(skip_block);

        Ok(value)
    }

    fn translate_call(&mut self, tir: Tir, dest: Option<Value>) -> ExprResult {
        let TirEnt { ty, kind: TirKind::Call(params, mut func, args), flags, .. } = self.body.ents[tir] else {
            unreachable!();
        };

        let (caller, param_slice) = {
            let params = self.ty_lists.get(params);
            let has_caller = flags.contains(TirFlags::WITH_CALLER);
            let caller_offset = has_caller as usize;
            (has_caller.then(|| params[0]), &params[caller_offset..])
        };

        // dispatch bound call
        if let FuncKind::Bound(bound, index) = self.func_meta[func].kind {
            let id = {
                let bound = self.types[bound].id;
                let implementor = self.types[caller.unwrap()].id;
                ID::bound_impl(bound, implementor)
            };

            let funcs = self.bound_impls.get(id).unwrap().funcs;
            func = self.func_lists.get(funcs)[index as usize]
        }

        let func_ent = self.funcs.ents[func];

        // instantiate generic function
        if flags.contains(TirFlags::GENERIC) {
            let id = param_slice.iter().fold(func_ent.id, |acc, &ty| acc + self.types[ty].id);

            func = if let Some(&instance) = self.funcs.instances.get(id) {
                instance
            } else {
                let instance = FuncEnt {
                    id,
                    parent: func.into(),
                    flags: func_ent.flags & !FuncFlags::GENERIC,
                };
                let instance = self.funcs.ents.push(instance);
                self.funcs.instances.insert(id, instance);
                self.funcs.to_compile.push((instance, params));
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
            let args_view = self.body.cons.get(args);

            self.func.value_slices.mark_frame();

            if has_sret {
                self.func.value_slices.push_one(value.unwrap());
            }

            for &arg in args_view {
                let value = self.translate_expr(arg, None)?.unwrap();
                self.func.value_slices.push_one(value);
            }
            self.func.value_slices.pop_frame()
        };

        if on_stack && !has_sret {
            let temp = {
                let kind = InstKind::Call(func, args);
                let value = self.value_from_ty(ty);
                let ent = InstEnt::new(kind, value.into());
                self.func.add_inst(ent);
                value
            };

            self.assign(temp, value);
        } else {
            let kind = InstKind::Call(func, args);
            let inst = InstEnt::new(kind, value);
            self.func.add_inst(inst);

            if let Some(value) = value {
                self.assign(value, dest);
            }
        }

        Ok(value)
    }

    fn unwrap_dest(&mut self, tir: Tir, dest: Option<Value>) -> Option<Value> {
        let on_stack = self.on_stack(tir);
        let ret = self.body.ents[tir].ty;
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
                    self.func.stacks.push(ent)
                };

                let value = self.flagged_value_from_ty(ret, mir::MirFlags::POINTER);

                {
                    let kind = InstKind::StackAddr(stack);
                    let ent = InstEnt::new(kind, value.into());
                    self.func.add_inst(ent);
                }

                value
            } else {
                // no need to allocate, register is enough
                self.value_from_ty(ret)
            }
        })
    }

    fn int_lit(&mut self, ty: Ty, literal_value: i64) -> Value {
        let value = self.value_from_ty(ty);
        self.int_lit_low(value, literal_value);
        value
    }

    fn int_lit_low(&mut self, value: Value, literal_value: i64) {
        let kind = InstKind::IntLit(literal_value);
        let ent = InstEnt::new(kind, value.into());
        self.func.add_inst(ent);
    }

    fn field_offset(&self, header: Value, field: TyComp) -> Offset {
        let header_ty = self.func.values[header].ty;
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
            let mut value = self.func.values[value];
            value.offset = value.offset + offset;
            if let Some(ty) = ty {
                value.ty = ty;
            }
            self.func.values.push(value)
        };

        {
            let kind = InstKind::Offset(value);
            let ent = InstEnt::new(kind, of_value.into());
            self.func.add_inst(ent);
        }

        of_value
    }

    fn assign(&mut self, value: Value, dest: Option<Value>) {
        if let Some(dest) = dest {
            let kind = InstKind::Assign(value);
            let ent = InstEnt::new(kind, dest.into());
            self.func.add_inst(ent);
        }
    }

    fn on_stack(&self, tir: Tir) -> bool {
        let TirEnt { ty, flags, .. } = self.body.ents[tir];
        self.reprs[ty].flags.contains(repr::ReprFlags::ON_STACK)
            || flags.contains(TirFlags::SPILLED)
    }

    fn translate_return(&mut self, value: Option<Tir>) -> ExprResult {
        let value = if let Some(value) = value {
            let dest = self.return_dest;
            self.translate_expr(value, dest)?
        } else {
            None
        };

        {
            let kind = InstKind::Return;
            let ent = InstEnt::new(kind, value);
            self.func.add_inst(ent);
        }

        Ok(None)
    }

    fn translate_value(&mut self, tir: Tir) -> Option<Value> {
        let TirEnt { ty, .. } = self.body.ents[tir];
        if ty == self.builtin_types.nothing {
            return None;
        }
        let flags = MirFlags::POINTER & self.reprs[ty].flags.contains(ReprFlags::ON_STACK);
        let value = self.flagged_value_from_ty(ty, flags);
        self.ctx.tir_mapping[tir] = value.into();
        Some(value)
    }

    fn value_from_ty(&mut self, ty: Ty) -> Value {
        self.flagged_value_from_ty(ty, Default::default())
    }

    fn flagged_value_from_ty(&mut self, ty: Ty, flags: mir::MirFlags) -> Value {
        let value = ValueEnt::flags(ty, flags);
        self.func.values.push(value)
    }   
}

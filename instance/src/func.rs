use std::str::FromStr;

use cranelift_codegen::ir::Type;
use cranelift_codegen::{ir, isa::CallConv, packed_option::PackedOption};

use lexer_types::*;
use instance_types::*;
use storage::*;
use typec_types::*;

pub struct MirBuilder<'a> {
    pub func_id: Func,
    pub system_call_convention: CallConv,
    pub reprs: &'a Reprs,
    pub ptr_ty: Type,
    pub types: &'a Types,
    pub ty_lists: &'a TyLists,
    pub func_lists: &'a TFuncLists,
    pub sfields: &'a SFields,
    pub bound_impls: &'a BoundImpls,
    pub repr_fields: &'a ReprFields,
    pub builtin_types: &'a BuiltinTable,
    pub t_funcs: &'a Funcs,
    pub func: &'a mut FuncCtx,
    pub body: &'a TirData,
    pub return_dest: Option<Value>,
    pub has_sret: &'a EntitySet<Func>,
    pub tir_mapping: &'a mut SecondaryMap<Tir, PackedOption<Value>>,
    pub sources: &'a Sources,
}

impl MirBuilder<'_> {
    pub fn translate_func(&mut self) -> errors::Result {
        let TFuncEnt {
            sig,
            body,
            args,
            flags,
            ..
        } = self.t_funcs[self.func_id];

        let has_sret = Self::translate_signature(
            &sig,
            &mut self.func.sig,
            self.reprs,
            self.types,
            self.ty_lists,
            self.sources,
            self.system_call_convention,
            flags.contains(TFuncFlags::ENTRY),
        )?;

        let entry_point = self.func.create_block();
        {
            if has_sret {
                let value = self.flagged_value_from_ty(sig.ret, mir::Flags::POINTER);
                self.return_dest = Some(value);
                self.func.value_slices.push_one(value);
            }

            for &tir in self.body.cons.get(args) {
                let value = self.translate_value(tir).unwrap();
                self.func.value_slices.push_one(value);
            }

            self.func.blocks[entry_point].params = self.func.value_slices.close_frame();
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

    fn translate_block(&mut self, tir: Tir, dest: Option<Value>) -> errors::Result<Option<Value>> {
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

    fn translate_expr(&mut self, tir: Tir, dest: Option<Value>) -> errors::Result<Option<Value>> {
        if let Some(value) = self.tir_mapping[tir].expand() {
            return Ok(Some(value));
        }

        let TirEnt { kind, ty, span, .. } = self.body.ents[tir];
        let value = match kind {
            TirKind::Return(value) => self.translate_return(value.expand())?,
            TirKind::Call(..) => self.translate_call(tir, dest)?,
            TirKind::If(..) => self.translate_if(tir, dest)?,
            TirKind::Variable(value) => self.translate_variable(value)?,
            TirKind::Constructor(..) => self.translate_constructor(tir, dest)?,
            TirKind::FieldAccess(value, field) => {
                self.translate_field_access(value, field, dest)?
            }
            TirKind::Block(..) => self.translate_block(tir, dest)?,
            TirKind::Loop(..) => self.translate_loop(tir, dest)?,
            TirKind::Break(loop_header, value) => self.translate_break(loop_header, value)?,
            TirKind::Assign(a, b) => self.translate_assign(a, b)?,
            TirKind::Access(value) => self.translate_expr(value, dest)?,
            TirKind::IntLit(..) => self.translate_int_lit(ty, span, dest)?,
            TirKind::BoolLit(value) => self.translate_bool_lit(ty, value, dest)?,
            TirKind::CharLit => self.translate_char_lit(ty, span, dest)?,
            TirKind::TakePtr(..) => self.translate_take_pointer(tir, dest)?,
            TirKind::DerefPointer(..) => self.translate_deref_pointer(tir, dest)?,
            _ => todo!("Unhandled Kind::{:?}", kind),
        };

        self.tir_mapping[tir] = value.into();

        Ok(value)
    }

    fn translate_take_pointer(&mut self, tir: Tir, dest: Option<Value>) -> errors::Result<Option<Value>> {
        let TirEnt { ty, kind: TirKind::TakePtr(value), .. } = self.body.ents[tir] else {
            unreachable!();
        };

        let mir_value = self.translate_expr(value, dest)?.unwrap();

        // // funny thing, any stack allocated value treated as
        // // value is pointer but pointer treated as pointer is value
        let value = self.value_from_ty(ty);

        {
            let kind = mir::InstKind::TakePointer(mir_value);
            let inst = InstEnt::new(kind, value.into());
            self.func.add_inst(inst);
        }

        if let Some(dest) = dest {
            let kind = InstKind::Assign(value);
            let ent = InstEnt::new(kind, dest.into());
            self.func.add_inst(ent);
        }

        self.tir_mapping[tir] = value.into();

        Ok(Some(value))
    }

    fn translate_deref_pointer(&mut self, tir: Tir, dest: Option<Value>) -> errors::Result<Option<Value>> {
        let TirEnt { ty, kind: TirKind::DerefPointer(value), .. } = self.body.ents[tir] else {
            unreachable!()
        };

        let mir_value = self.translate_expr(value, dest)?.unwrap();

        let value = self.flagged_value_from_ty(ty, mir::Flags::POINTER);

        {
            let kind = mir::InstKind::DerefPointer(mir_value);
            let inst = InstEnt::new(kind, value.into());
            self.func.add_inst(inst);
        }

        if let Some(dest) = dest {
            let kind = InstKind::Assign(value);
            let ent = InstEnt::new(kind, dest.into());
            self.func.add_inst(ent);
        }

        self.tir_mapping[tir] = value.into();

        Ok(Some(value))
    }

    fn translate_char_lit(
        &mut self,
        ty: Ty,
        span: Span,
        dest: Option<Value>,
    ) -> errors::Result<Option<Value>> {
        let literal = char_value(self.sources, span).unwrap() as u64;

        let value = self.value_from_ty(ty);

        {
            let kind = InstKind::IntLit(literal);
            let ent = InstEnt::new(kind, value.into());
            self.func.add_inst(ent);
        }

        if let Some(dest) = dest {
            let kind = InstKind::Assign(value);
            let ent = InstEnt::new(kind, dest.into());
            self.func.add_inst(ent);
        }

        Ok(Some(value))
    }

    fn translate_assign(&mut self, a: Tir, b: Tir) -> errors::Result<Option<Value>> {
        let a = self.translate_expr(a, None)?.unwrap();
        let b = self.translate_expr(b, Some(a))?.unwrap();

        if a == b {
            return Ok(None);
        }

        {
            let kind = InstKind::Assign(b);
            let ent = InstEnt::new(kind, a.into());
            self.func.add_inst(ent);
        }

        Ok(None)
    }

    fn translate_break(
        &mut self,
        loop_header_marker: Tir,
        value: PackedOption<Tir>,
    ) -> errors::Result<Option<Value>> {
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

    fn translate_loop(&mut self, tir: Tir, dest: Option<Value>) -> errors::Result<Option<Value>> {
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
        tir_header: Tir,
        field: SField,
        dest: Option<Value>,
    ) -> errors::Result<Option<Value>> {
        let ty = self.body.ents[tir_header].ty;
        let header = self.translate_expr(tir_header, None)?.unwrap();

        let (field_id, field_ty) = {
            let ty_id = self.types[ty].id;
            let SFieldEnt { index, ty, .. } = self.sfields[field];
            (ID::raw_field(ty_id, index as u64), ty)
        };

        let Some(&repr::ReprField { offset }) = self.repr_fields.get(field_id) else {
            unreachable!()
        };

        let is_pointer = self.func.values[header].flags.contains(mir::Flags::POINTER);

        let value = {
            let offset = self.func.values[header].offset + offset;
            let ent = ValueEnt::new(field_ty, offset, mir::Flags::POINTER & is_pointer);
            self.func.values.push(ent)
        };

        {
            let kind = InstKind::Offset(header);
            let ent = InstEnt::new(kind, value.into());
            self.func.add_inst(ent);
        }

        if let Some(dest) = dest {
            let kind = InstKind::Assign(value);
            let ent = InstEnt::new(kind, dest.into());
            self.func.add_inst(ent);
        }

        Ok(Some(value))
    }

    fn translate_bool_lit(
        &mut self,
        ty: Ty,
        literal_value: bool,
        dest: Option<Value>,
    ) -> errors::Result<Option<Value>> {
        let value = self.value_from_ty(ty);

        {
            let kind = InstKind::BoolLit(literal_value);
            let ent = InstEnt::new(kind, value.into());
            self.func.add_inst(ent);
        }

        if let Some(dest) = dest {
            let kind = InstKind::Assign(value);
            let ent = InstEnt::new(kind, dest.into());
            self.func.add_inst(ent);
        }

        Ok(Some(value))
    }

    fn translate_constructor(&mut self, tir: Tir, dest: Option<Value>) -> errors::Result<Option<Value>> {
        let TirEnt { ty, kind: TirKind::Constructor(data), .. } = self.body.ents[tir] else {
            unreachable!()
        };

        let on_stack = self.on_stack(tir);
        let Some(mut value) = self.unwrap_dest_low(ty, on_stack, dest).or(dest) else {
            unreachable!();
        };

        if !on_stack && dest.is_none() {
            let kind = InstKind::IntLit(0);
            let ent = InstEnt::new(kind, value.into());
            self.func.add_inst(ent);
        }

        let data_view = self.body.cons.get(data);
        let ty_id = self.types[ty].id;

        for (i, &data) in data_view.iter().enumerate() {
            let id = ID::raw_field(ty_id, i as u64);
            let Some(&repr::ReprField { offset }) = self.repr_fields.get(id) else {
                unreachable!()
            };

            let dest = {
                let of_value = {
                    let mut value = self.func.values[value];
                    value.offset = value.offset + offset;
                    self.func.values.push(value)
                };

                {
                    let kind = InstKind::Offset(value);
                    let ent = InstEnt::new(kind, of_value.into());
                    self.func.add_inst(ent);
                }

                of_value
            };

            self.translate_expr(data, Some(dest))?;

            if !on_stack {
                let new_value = {
                    let value = self.func.values[value];
                    self.func.values.push(value)
                };

                {
                    let kind = InstKind::Offset(dest);
                    let ent = InstEnt::new(kind, new_value.into());
                    self.func.add_inst(ent);
                }

                value = new_value;
            }
        }

        Ok(Some(value))
    }

    fn translate_variable(&mut self, tir: Tir) -> errors::Result<Option<Value>> {
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
            .insert(mir::Flags::ASSIGNABLE & assignable);

        Ok(None)
    }

    fn translate_int_lit(
        &mut self,
        ty: Ty,
        span: Span,
        dest: Option<Value>,
    ) -> errors::Result<Option<Value>> {
        let literal_value = int_value(self.sources, span);

        let value = self.value_from_ty(ty);

        {
            let kind = InstKind::IntLit(literal_value);
            let ent = InstEnt::new(kind, value.into());
            self.func.add_inst(ent);
        }

        if let Some(dest) = dest {
            let kind = InstKind::Assign(value);
            let ent = InstEnt::new(kind, dest.into());
            self.func.add_inst(ent);
        }

        Ok(Some(value))
    }

    fn translate_if(&mut self, tir: Tir, dest: Option<Value>) -> errors::Result<Option<Value>> {
        let TirEnt { ty, kind: TirKind::If(cond, then, otherwise), .. } = self.body.ents[tir] else {
            unreachable!();
        };

        let cond = self.translate_expr(cond, None)?;

        let on_stack = self.on_stack(tir);
        let value = self.unwrap_dest_low(ty, on_stack, dest);
        let block_dest = on_stack.then_some(value.or(dest)).flatten();

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
                (!on_stack).then_some(value).flatten()
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
                (!on_stack).then_some(value).flatten()
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

    fn translate_call(&mut self, tir: Tir, dest: Option<Value>) -> errors::Result<Option<Value>> {
        let TirEnt { ty, kind: TirKind::Call(caller, func, args), .. } = self.body.ents[tir] else {
            unreachable!();
        };

        let func = if let TFuncKind::Bound(bound, index) = self.t_funcs[func].kind {
            let id = {
                let bound = self.types[bound].id;
                let implementor = self.types[caller.unwrap()].id;
                ID::bound_impl(bound, implementor)
            };

            let funcs = self.bound_impls.get(id).unwrap().funcs;
            self.func_lists.get(funcs)[index as usize]
        } else {
            func
        };

        let on_stack = self.on_stack(tir);
        let has_sret = self.has_sret.contains(func);

        let dest = on_stack.then_some(dest).flatten();
        let value = self.unwrap_dest_low(ty, on_stack, dest).or(dest);

        let args = {
            let args_view = self.body.cons.get(args);

            let mut args =
                Vec::with_capacity(args_view.len() + caller.is_some() as usize + has_sret as usize);

            if has_sret {
                args.push(value.unwrap());
            }

            for &arg in args_view {
                args.push(self.translate_expr(arg, None)?.unwrap());
            }
            self.func.value_slices.push(&args)
        };

        if on_stack && !has_sret {
            let temp = {
                let kind = InstKind::Call(func, args);
                let value = self.value_from_ty(ty);
                let ent = InstEnt::new(kind, value.into());
                self.func.add_inst(ent);
                value
            };

            let kind = InstKind::Assign(temp);
            let ent = InstEnt::new(kind, value.into());
            self.func.add_inst(ent);
        } else {
            let kind = InstKind::Call(func, args);
            let inst = InstEnt::new(kind, value);
            self.func.add_inst(inst);
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
                    let size = self.reprs[ret].size;
                    let ent = StackEnt::new(size, self.ptr_ty);
                    self.func.stacks.push(ent)
                };

                let value = self.flagged_value_from_ty(ret, mir::Flags::POINTER);

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

    fn on_stack(&self, tir: Tir) -> bool {
        let TirEnt { ty, flags, .. } = self.body.ents[tir];
        self.reprs[ty].flags.contains(repr::ReprFlags::ON_STACK)
            || flags.contains(TirFlags::SPILLED)
    }

    fn translate_return(&mut self, value: Option<Tir>) -> errors::Result<Option<Value>> {
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
        let value = self.value_from_ty(ty);
        self.tir_mapping[tir] = value.into();
        Some(value)
    }

    fn value_from_ty(&mut self, ty: Ty) -> Value {
        self.flagged_value_from_ty(ty, Default::default())
    }

    fn flagged_value_from_ty(&mut self, ty: Ty, flags: mir::Flags) -> Value {
        let value = ValueEnt::flags(ty, flags);
        self.func.values.push(value)
    }

    /// translates a `source` into the `target` and returns true if
    /// signature has struct return type
    pub fn translate_signature(
        source: &Sig,
        target: &mut ir::Signature,
        reprs: &Reprs,
        types: &Types,
        ty_lists: &TyLists,
        sources: &Sources,
        system_call_convention: CallConv,
        is_entry: bool,
    ) -> errors::Result<bool> {
        let call_conv = if is_entry {
            system_call_convention
        } else if source.call_conv.is_reserved_value() {
            CallConv::Fast
        } else {
            let str = sources.display(source.call_conv.strip_sides());
            if str == "default" {
                system_call_convention
            } else {
                let cc = CallConv::from_str(str);
                // TODO: error handling
                cc.unwrap_or(CallConv::Fast)
            }
        };
        target.clear(call_conv);

        let mut result = false;
        if TyKind::Nothing != types[source.ret].kind {
            let repr::ReprEnt { repr, flags, .. } = reprs[source.ret];
            if flags.contains(repr::ReprFlags::ON_STACK) {
                let ret = ir::AbiParam::special(repr, ir::ArgumentPurpose::StructReturn);
                target.params.push(ret);
                result = true;
                target.returns.push(ret);
            } else {
                target.returns.push(ir::AbiParam::new(repr));
            }
        }

        target.params.extend(
            ty_lists
                .get(source.args)
                .iter()
                .map(|&ty| reprs[ty].repr)
                .map(|repr| ir::AbiParam::new(repr)),
        );

        Ok(result)
    }
}

pub fn int_value(sources: &Sources, span: Span) -> u64 {
    let mut chars = sources.display(span).chars();
    let mut value = 0;
    while let Some(c @ '0'..='9') = chars.next() {
        value = value * 10 + (c as u64 - '0' as u64);
    }

    match chars.next() {
        None => return value,
        _ => todo!("unhandled int literal {:?}", sources.display(span)),
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CharError {
    ExtraCharacters,
    NoCharacter,
}

pub fn char_value(sources: &Sources, span: Span) -> std::result::Result<char, CharError> {
    let mut chars = sources.display(span.strip_sides()).chars();
    let char = chars.next().ok_or(CharError::NoCharacter)?;
    if chars.next().is_some() {
        return Err(CharError::ExtraCharacters);
    }
    Ok(char)
}
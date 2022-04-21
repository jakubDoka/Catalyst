use cranelift_codegen::ir::Type;
use cranelift_codegen::{ir, isa::CallConv, packed_option::PackedOption};
use cranelift_entity::{EntityList, ListPool, PrimaryMap, SecondaryMap, EntitySet};

use crate::{Result, Types};
use crate::*;
use lexer::*;
use typec::{*, func};

pub struct Translator<'a> {
    pub func_id: typec::Func,
    pub system_call_convention: CallConv,
    pub types: &'a Types,
    pub nothing: Ty,
    pub ptr_ty: Type,
    pub t_types: &'a typec::Types,
    pub t_funcs: &'a typec::Funcs,
    pub func: &'a mut Func,
    pub body: &'a tir::Data,
    pub return_dest: Option<Value>,
    pub has_struct_ret: &'a EntitySet<typec::Func>,
    pub tir_mapping: &'a mut SecondaryMap<Tir, PackedOption<Value>>,
    pub sources: &'a Sources,
}

impl Translator<'_> {
    pub fn translate_func(&mut self) -> Result {
        let func::Ent { sig, body, args, .. } = self.t_funcs[self.func_id];
        
        let has_struct_ret = Self::translate_signature(
            &sig, 
            &mut self.func.sig, 
            self.types, 
            self.t_types, 
            self.system_call_convention
        )?;
        
        
        let entry_point = self.func.create_block();
        
        if has_struct_ret {
            let value = self.flagged_value_from_ty(sig.ret, mir::Flags::POINTER);
            self.return_dest = Some(value);
            self.func.push_block_param(entry_point, value);
        }
        
        for &tir in self.body.cons.view(args) {
            let value = self.translate_value(tir).unwrap();
            self.func.push_block_param(entry_point, value);
        }
        self.func.select_block(entry_point);

        let value = self.translate_block(body, self.return_dest)?;

        if !self.func.is_terminated() {
            self.func.add_inst(InstEnt::new(InstKind::Return, value));
        }

        Ok(())
    }

    fn translate_block(&mut self, tir: Tir, dest: Option<Value>) -> Result<Option<Value>> {
        let tir::Kind::Block(stmts) = self.body.ents[tir].kind else {
            unreachable!()
        };

        if stmts.is_empty() {
            return Ok(None);
        }

        let stmts = self.body.cons.view(stmts);
        for &stmt in stmts[..stmts.len() - 1].iter() {
            self.translate_expr(stmt, None)?;
        }

        let value = self.translate_expr(stmts[stmts.len() - 1], dest)?;

        Ok(value)
    }

    fn translate_expr(&mut self, tir: Tir, dest: Option<Value>) -> Result<Option<Value>> {
        if let Some(value) = self.tir_mapping[tir].expand() {
            return Ok(Some(value));
        }

        let tir::Ent { kind, ty, span, .. } = self.body.ents[tir];
        let value = match kind {
            tir::Kind::Return(value) => self.translate_return(value.expand())?,
            tir::Kind::Call(func, args) => self.translate_call(func, args, dest)?,
            tir::Kind::If(cons, then, otherwise) => self.translate_if(cons, then, otherwise, ty, dest)?,
            tir::Kind::IntLit(_) => self.translate_int_lit(ty, span, dest)?,
            tir::Kind::BoolLit(value) => self.translate_bool_lit(ty, value, dest)?,
            tir::Kind::Variable(value) => self.translate_variable(value)?,
            tir::Kind::Constructor(data) => self.translate_constructor(ty, data, dest)?,
            tir::Kind::FieldAccess(value, field) => self.translate_field_access(value, field, dest)?,
            tir::Kind::Block(_) => self.translate_block(tir, dest)?,
            tir::Kind::Loop(body) => self.translate_loop(ty, tir, body, dest)?,
            tir::Kind::Break(loop_header, value) => self.translate_break(loop_header, value)?,
            tir::Kind::Assign(a, b) => self.translate_assign(a, b)?,
            tir::Kind::Access(value) => self.translate_expr(value, dest)?,
            _ => todo!("Unhandled tir::Kind:{:?}", kind),
        };

        self.tir_mapping[tir] = value.into();

        Ok(value)
    }

    fn translate_assign(&mut self, a: Tir, b: Tir) -> Result<Option<Value>> {
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

    fn translate_break(&mut self, loop_header_marker: Tir, value: PackedOption<Tir>) -> Result<Option<Value>> {
        let &Loop { exit, dest, .. } = self.func.loops
            .iter()
            .rev()
            .find(|loop_header| loop_header.marker == loop_header_marker)
            .unwrap();
        
        let value = if let Some(value) = value.expand() {
            dest
                .is_none()
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

    fn translate_loop(&mut self, ty: Ty, tir: Tir, body: Tir, dest: Option<Value>) -> Result<Option<Value>> {
        let enter_block = self.func.create_block();
        let exit_block = self.func.create_block();
        
        let on_stack = self.types.ents[ty].flags.contains(types::Flags::ON_STACK);
        let value = self.unwrap_dest(ty, dest);

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
            self.func.push_block_param(exit_block, value);
        }
        self.func.select_block(exit_block);

        Ok(value)
    }

    fn translate_field_access(&mut self, tir_header: Tir, field: ID, dest: Option<Value>) -> Result<Option<Value>> {
        let ty = self.body.ents[tir_header].ty;
        let header = self.translate_expr(tir_header, None)?.unwrap();

        let (field_id, field_ty) = {
            let ty_id = self.t_types.ents[ty].id;
            let Some(&typec::Field { index, ty, .. }) = self.t_types.fields.get(field) else {
                unreachable!()
            };
            (types::TypeTranslator::field_id(ty_id, index as u64), ty)
        };

        let Some(&types::Field { offset }) = self.types.fields.get(field_id) else {
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

    fn translate_bool_lit(&mut self, ty: Ty, literal_value: bool, dest: Option<Value>) -> Result<Option<Value>> {
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

    fn translate_constructor(&mut self, ty: Ty, data: EntityList<Tir>, dest: Option<Value>) -> Result<Option<Value>> {        
        let Some(mut value) = self.unwrap_dest(ty, dest).or(dest) else {
            unreachable!();
        };

        let on_stack = self.types.ents[ty].flags.contains(types::Flags::ON_STACK);
        if !on_stack && dest.is_none() {
            let kind = InstKind::IntLit(0);
            let ent = InstEnt::new(kind, value.into());
            self.func.add_inst(ent);
        }

        let data_view = self.body.cons.view(data);
        let ty_id = self.t_types.ents[ty].id;

        for (i, &data) in data_view.iter().enumerate() {
            let id = types::TypeTranslator::field_id(ty_id, i as u64);
            let Some(&types::Field { offset }) = self.types.fields.get(id) else {
                unreachable!()
            };
            

            let dest = {
                let of_value = {
                    let mut value = self.func.values[value];
                    value.offset = offset;    
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

    fn translate_variable(&mut self, value: Tir) -> Result<Option<Value>> {
        let ty = self.body.ents[value].ty;
        let on_stack = self.types.ents[ty].flags.contains(types::Flags::ON_STACK);
        let assignable = self.body.ents[value].flags.contains(tir::Flags::ASSIGNABLE);

        let dest = if on_stack {
            self.unwrap_dest(ty, None)
        } else {
            None
        };
        
        let value = self.translate_expr(value, dest)?;

        {
            let kind = InstKind::Variable;
            let ent = InstEnt::new(kind, value);
            self.func.add_inst(ent);
        }
        
        println!("=={:?}", assignable);

        self.func.values[value.unwrap()].flags.insert(mir::Flags::ASSIGNABLE & assignable);
        
        Ok(None)
    }

    fn translate_int_lit(&mut self, ty: Ty, span: Span, dest: Option<Value>) -> Result<Option<Value>> {
        let literal_value = lexer::int_value(self.sources, span);

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

    fn translate_if(&mut self, cons: Tir, then: Tir, otherwise: PackedOption<Tir>, ty: Ty, dest: Option<Value>) -> Result<Option<Value>> {
        let cond = self.translate_expr(cons, None)?;
        
        let on_stack = self.types.ents[ty].flags.contains(types::Flags::ON_STACK);
        let value = self.unwrap_dest(ty, dest);
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
            self.func.push_block_param(skip_block, value);
        };

        self.func.select_block(skip_block);

        Ok(value)
    }

    fn translate_call(&mut self, func: typec::Func, args: EntityList<Tir>, dest: Option<Value>) -> Result<Option<Value>> {
        let has_struct_ret = self.has_struct_ret.contains(func);

        let dest = has_struct_ret.then_some(dest).flatten();

        let value = {
            let ret = self.t_funcs[func].sig.ret;
            self.unwrap_dest(ret, dest).or(dest)
        };
        
        let args = {
            let args_view = self.body.cons.view(args);
            let mut args = Vec::with_capacity(args_view.len() + has_struct_ret as usize);
            if has_struct_ret {
                args.push(value.unwrap());
            }
            for &arg in args_view {
                args.push(self.translate_expr(arg, None)?.unwrap());
            }
            self.func.value_slices.list(&args)
        };

        { 
            let kind = InstKind::Call(func, args);
            let inst = InstEnt::new(kind, value);
            self.func.add_inst(inst);
        }

        Ok(value)
    }

    fn unwrap_dest(&mut self, ret: Ty, dest: Option<Value>) -> Option<Value> {
        let on_stack = self.types.ents[ret].flags.contains(types::Flags::ON_STACK);
        let has_ret = ret != self.nothing;
        (dest.is_none() && has_ret).then(|| {
            if on_stack {
                // stack needs to be allocated to pass valid pointer
                let stack = {
                    let size = self.types.ents[ret].size;
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

    fn translate_return(&mut self, value: Option<Tir>) -> Result<Option<Value>> {
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
        let tir::Ent { ty, ..} = self.body.ents[tir];
        if ty == self.nothing {
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
        source: &typec::Signature,
        target: &mut ir::Signature,
        types: &Types,
        t_types: &typec::Types,
        system_call_convention: CallConv,
    ) -> Result<bool> {
        target.clear(system_call_convention);
        
        let mut result = false;
        if ty::Kind::Nothing != t_types.ents[source.ret].kind {
            let types::Ent { repr, flags, .. } = types.ents[source.ret];
            if flags.contains(types::Flags::ON_STACK) {
                let ret = ir::AbiParam::special(repr, ir::ArgumentPurpose::StructReturn);
                target.params.push(ret);
                result = true;
                target.returns.push(ret);
            } else {
                target.returns.push(ir::AbiParam::new(repr));
            }
        }
        
        target.params.extend(
            t_types.cons.view(source.args)
                .iter()
                .map(|&ty| types.ents[ty].repr)
                .map(|repr| ir::AbiParam::new(repr)),
        );

        Ok(result)
    }
}

#[derive(Debug)]
pub struct Loop {
    _enter: Block,
    exit: Block,
    marker: Tir,
    dest: Option<Value>,
}

#[derive(Debug)]
pub struct Func {
    pub name: Span,
    pub sig: ir::Signature,

    pub values: PrimaryMap<Value, ValueEnt>,
    pub value_slices: ListPool<Value>,
    pub blocks: PrimaryMap<Block, BlockEnt>,
    pub insts: PrimaryMap<Inst, InstEnt>,
    pub stacks: PrimaryMap<Stack, StackEnt>,

    pub current: PackedOption<Block>,
    pub start: PackedOption<Block>,
    pub end: PackedOption<Block>,
    pub loops: Vec<Loop>,
}

impl Func {
    pub fn new() -> Self {
        Func {
            name: Span::default(),
            sig: ir::Signature::new(CallConv::Fast),
            values: PrimaryMap::new(),
            value_slices: ListPool::new(),
            blocks: PrimaryMap::new(),
            insts: PrimaryMap::new(),
            stacks: PrimaryMap::new(),
            current: Default::default(),
            start: Default::default(),
            end: Default::default(),
            loops: Default::default(),
        }
    }

    pub fn create_block(&mut self) -> Block {
        let block = self.blocks.push(Default::default());
        if let Some(end) = self.end.expand() {
            self.blocks[end].next = block.into();
            self.blocks[block].prev = end.into();
        } else {
            self.start = block.into();
        }
        self.end = block.into();
        block
    }

    pub fn push_block_param(&mut self, block: Block, param: Value) {
        self.blocks[block]
            .params
            .push(param, &mut self.value_slices);
    }

    pub fn add_inst(&mut self, inst: InstEnt) -> Inst {
        assert!(!self.is_terminated());
        let block = &mut self.blocks[self.current.unwrap()];
        let inst = self.insts.push(inst);
        if let Some(end) = block.end.expand() {
            self.insts[end].next = inst.into();
            self.insts[inst].prev = end.into();
        } else {
            block.start = inst.into();
        }
        block.end = inst.into();
        inst
    }

    pub fn blocks(&self) -> impl Iterator<Item = (Block, &BlockEnt)> + '_ {
        self.blocks.linked_iter(self.start.expand())
    }

    pub fn block_params(&self, block: Block) -> impl Iterator<Item = (Value, &ValueEnt)> + '_ {
        self.blocks[block]
            .params
            .as_slice(&self.value_slices)
            .iter()
            .map(|&value| (value, &self.values[value]))
    }

    pub fn clear(&mut self) {
        self.sig.clear(CallConv::Fast);
        self.values.clear();
        self.value_slices.clear();
        self.blocks.clear();
        self.insts.clear();
        self.stacks.clear();
        self.start.take();
        self.end.take();
    }

    pub fn is_terminated(&self) -> bool {
        self.current.expand().map_or(false, |current| {
            self.blocks[current].end.expand().map_or(false, |end| {
                self.insts[end].kind.is_terminating()
            })
        })
    }

    pub fn select_block(&mut self, block: Block) {
        self.current = block.into();
    }
}

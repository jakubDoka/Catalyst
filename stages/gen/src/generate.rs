use cranelift_codegen::{
    ir::{
        self, types, AbiParam, ExtFuncData, ExternalName, InstBuilder, MemFlags, StackSlotData,
        StackSlotKind, Type, UserExternalName,
    },
    isa::CallConv,
};
use mir_t::*;
use storage::*;

use typec_t::*;

use crate::*;

impl Generator<'_> {
    pub fn generate(
        &mut self,
        signature: Signature,
        params: &[VRef<Ty>],
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

        self.block(root, builder);

        builder.finalize();
    }

    fn block(&mut self, block: VRef<BlockMir>, builder: &mut GenBuilder) -> ir::Block {
        let BlockMir {
            args,
            insts,
            control_flow,
            ref_count,
        } = builder.body.blocks[block];

        if let Some(block) = self.gen_resources.blocks[block].as_mut() {
            block.visit_count += 1;

            if block.visit_count == ref_count {
                builder.seal_block(block.id);
            }

            return block.id;
        }

        let ir_block = builder.create_block();
        builder.switch_to_block(ir_block);

        for &arg in &builder.body.value_args[args] {
            let ty = self.ty_repr(builder.body.value_ty(arg), builder.ptr_ty());
            self.gen_resources.values[arg] = builder.append_block_param(ir_block, ty).into();
        }

        for &inst in &builder.body.insts[insts] {
            self.inst(inst, builder);
        }

        self.control_flow(control_flow, builder);

        let visit_count = 0;
        if ref_count == visit_count {
            builder.seal_block(ir_block);
        }

        self.gen_resources.blocks[block] = GenBlock {
            id: ir_block,
            visit_count,
        }
        .into();

        ir_block
    }

    fn inst(&mut self, inst: InstMir, builder: &mut GenBuilder) {
        let (ret, value) = match inst {
            InstMir::Int(value, ret) => {
                let ty = self.ty_repr(builder.body.value_ty(ret), builder.ptr_ty());
                let value = builder.ins().iconst(ty, value);
                (ret, Some(value))
            }
            InstMir::Access(..) => return,
            InstMir::Call(call, ret) => (ret, self.call(call, builder)),
            InstMir::Const(id, ret) => (ret, self.r#const(id, ret, builder)),
            InstMir::Constructor(fields, ret) => {
                (ret, Some(self.constructor(fields, ret, builder)))
            }
            InstMir::Deref(target, ret) | InstMir::Ref(target, ret) => {
                (ret, Some(self.load_value(target, builder)))
            }
        };

        if let Some(value) = value {
            self.save_value(ret, value, builder);
        }
    }

    fn constructor(
        &mut self,
        fields: VRefSlice<ValueMir>,
        ret: VRef<ValueMir>,
        builder: &mut GenBuilder,
    ) -> ir::Value {
        let layout = self.ty_layout(builder.body.value_ty(ret), &[], builder.ptr_ty());

        if layout.on_stack {
            self.stack_constructor(fields, layout, builder)
        } else {
            self.register_constructor(fields, layout, builder)
        }
    }

    fn stack_constructor(
        &mut self,
        fields: VRefSlice<ValueMir>,
        layout: Layout,
        builder: &mut GenBuilder,
    ) -> ir::Value {
        let stack_slot = builder.create_sized_stack_slot(StackSlotData {
            kind: StackSlotKind::ExplicitSlot,
            size: layout.size,
        });
        let ptr_ty = builder.ptr_ty();
        let target_config = builder.isa.frontend_config();

        let iter = builder.body.value_args[fields]
            .iter()
            .zip(self.gen_layouts.offsets[layout.offsets].to_bumpvec());
        for (&field, offset) in iter {
            let field_layout = self.ty_layout(builder.body.value_ty(field), &[], ptr_ty);
            let value = self.load_value(field, builder);
            if field_layout.on_stack {
                let dest = builder.ins().stack_addr(ptr_ty, stack_slot, offset as i32);
                builder.emit_small_memory_copy(
                    target_config,
                    dest,
                    value,
                    field_layout.size as u64,
                    layout.align.get(),
                    field_layout.align.get(),
                    true,
                    MemFlags::new(),
                );
            } else {
                builder.ins().stack_store(value, stack_slot, offset as i32);
            }
        }

        builder.ins().stack_addr(ptr_ty, stack_slot, 0)
    }

    fn register_constructor(
        &mut self,
        fields: VRefSlice<ValueMir>,
        layout: Layout,
        builder: &mut GenBuilder,
    ) -> ir::Value {
        let mut value = builder.ins().iconst(layout.repr, 0);

        let iter = builder.body.value_args[fields]
            .iter()
            .zip(self.gen_layouts.offsets[layout.offsets].to_bumpvec());

        for (&field, offset) in iter {
            let mut field_value = self.load_value(field, builder);
            if layout.size
                > self
                    .ty_repr(builder.body.value_ty(field), builder.ptr_ty())
                    .bytes()
            {
                field_value = builder.ins().uextend(layout.repr, field_value);
            }
            field_value = builder.ins().ishl_imm(field_value, offset as i64 * 8);
            value = builder.ins().bor(value, field_value);
        }

        value
    }

    fn r#const(
        &mut self,
        id: VRef<FuncConstMir>,
        ret: VRef<ValueMir>,
        builder: &mut GenBuilder,
    ) -> Option<ir::Value> {
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
            let ControlFlowMir::Return(ret) = control_flow else {
                unreachable!()
            };

            ret.map(|ret| self.load_value(ret, builder))
        } else {
            let value = self.gen_resources.func_constants[id]
                .expect("Constant should be computed before function compilation.");
            let ty = self.ty_repr(builder.body.value_ty(ret), builder.ptr_ty());

            Some(match value {
                GenFuncConstant::Int(val) => builder.ins().iconst(ty, val as i64),
            })
        }
    }

    fn call(
        &mut self,
        CallMir {
            callable,
            params,
            args,
        }: CallMir,
        builder: &mut GenBuilder,
    ) -> Option<ir::Value> {
        match callable {
            CallableMir::Func(func_id) => {
                let args = builder.body.value_args[args]
                    .iter()
                    .map(|&arg| self.load_value(arg, builder))
                    .collect::<BumpVec<_>>();

                if self.typec.funcs[func_id].flags.contains(FuncFlags::BUILTIN) {
                    return self.builtin_call(func_id, args, builder);
                }

                let func_ref = self.instantiate(func_id, params, builder);

                let inst = builder.ins().call(func_ref, &args);
                builder.inst_results(inst).first().copied()
            }
            CallableMir::SpecFunc(_) => todo!(),
            CallableMir::Pointer(_) => todo!(),
        }
    }

    fn builtin_call(
        &mut self,
        func_id: VRef<Func>,
        args: BumpVec<ir::Value>,
        builder: &mut GenBuilder,
    ) -> Option<ir::Value> {
        let Func {
            signature, name, ..
        } = self.typec.funcs[func_id];
        let op_str = &self.interner[name];
        let signed = self.typec.types.is_signed(signature.ret);

        macro_rules! helper {
            (ints) => {
                ir::types::I8 | ir::types::I16 | ir::types::I32 | ir::types::I64
            };
        }

        let value = match *args.as_slice() {
            [a, b] => match (builder.func.dfg.value_type(a), op_str) {
                (helper!(ints), "+") => builder.ins().iadd(a, b),
                (helper!(ints), "-") => builder.ins().isub(a, b),
                (helper!(ints), "*") => builder.ins().imul(a, b),
                (helper!(ints), "/") if signed => builder.ins().sdiv(a, b),
                (helper!(ints), "/") => builder.ins().udiv(a, b),
                val => unimplemented!("{:?}", val),
            },
            _ => unimplemented!(),
        };

        Some(value)
    }

    fn instantiate(
        &mut self,
        func_id: VRef<Func>,
        params: VRefSlice<MirTy>,
        builder: &mut GenBuilder,
    ) -> ir::FuncRef {
        let prefix = if builder.isa.jit { "jit-" } else { "native-" };
        let id = if params.is_empty() {
            let id = self.typec.funcs.id(func_id);
            self.interner
                .intern(ident!(prefix, builder.isa.triple.as_u32(), "&", id))
        } else {
            let start = ident!(
                prefix,
                builder.isa.triple.as_u32(),
                "&",
                self.typec.funcs.id(func_id),
                "["
            );
            let params = ident_join(
                ", ",
                builder.body.ty_params[params]
                    .iter()
                    .map(|&ty| self.typec.types.id(builder.body.dependant_types[ty].ty)),
            );
            let end = ident!("]");
            let segments = start.into_iter().chain(params).chain(end);
            self.interner.intern(segments)
        };

        if let Some(&imported) = self.gen_resources.func_imports.get(&id) {
            return imported;
        }

        let func = self.gen.compiled_funcs.get_or_insert(id, |s| {
            let params = builder.body.ty_params[params]
                .iter()
                .map(|&ty| builder.body.dependant_types[ty].ty);
            self.compile_requests.add_request(s.next(), func_id, params);
            CompiledFunc::new(func_id)
        });

        let func_ref = self.import_compiled_func(func, params, builder);

        self.gen_resources.func_imports.insert(id, func_ref);

        func_ref
    }

    pub fn import_compiled_func(
        &mut self,
        func: VRef<CompiledFunc>,
        params: VRefSlice<MirTy>,
        builder: &mut GenBuilder,
    ) -> ir::FuncRef {
        let name = builder
            .func
            .declare_imported_user_function(UserExternalName::new(
                Gen::FUNC_NAMESPACE,
                func.as_u32(),
            ));

        let func_id = self.gen.compiled_funcs[func].func;
        let Func {
            signature,
            visibility,
            ..
        } = self.typec.funcs[func_id];

        let params = builder.body.ty_params[params]
            .iter()
            .map(|&ty| builder.body.dependant_types[ty].ty)
            .collect::<BumpVec<_>>();

        let signature =
            self.load_signature(signature, &params, builder.system_cc(), builder.ptr_ty());
        let signature = builder.import_signature(signature);

        builder.import_function(ExtFuncData {
            name: ExternalName::User(name),
            signature,
            colocated: visibility != FuncVisibility::Imported,
        })
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
        }
    }

    pub fn load_signature(
        &mut self,
        signature: Signature,
        params: &[VRef<Ty>],
        system_cc: CallConv,
        ptr_ty: Type,
    ) -> ir::Signature {
        let mut sig = ir::Signature::new(CallConv::Fast);
        self.populate_signature(signature, params, &mut sig, system_cc, ptr_ty);
        sig
    }

    pub fn populate_signature(
        &mut self,
        signature: Signature,
        params: &[VRef<Ty>],
        target: &mut ir::Signature,
        system_cc: CallConv,
        ptr_ty: Type,
    ) {
        let cc = self.cc(signature.cc, system_cc);

        target.clear(cc);
        let args = self.typec.ty_slices[signature.args]
            .to_bumpvec()
            .into_iter()
            .map(|ty| {
                let instance = self.typec.instantiate(ty, params, self.interner);
                self.ty_repr(instance, ptr_ty)
            })
            .map(AbiParam::new);
        target.params.extend(args);

        let instance = self.typec.instantiate(signature.ret, params, self.interner);
        let ret = self.ty_repr(instance, ptr_ty);
        target.returns.push(AbiParam::new(ret));
    }

    fn cc(&self, cc: Option<VRef<str>>, system_cc: CallConv) -> CallConv {
        let Some(cc) = cc else {
            return CallConv::Fast;
        };

        if &self.interner[cc] == "default" {
            return system_cc;
        }

        self.interner[cc].parse().unwrap_or(CallConv::Fast)
    }

    fn ty_repr(&mut self, ty: VRef<Ty>, ptr_ty: Type) -> Type {
        self.ty_layout(ty, &[], ptr_ty).repr
    }

    fn ty_layout(&mut self, ty: VRef<Ty>, params: &[VRef<Ty>], ptr_ty: Type) -> Layout {
        if let Some(layout) = self.gen_layouts.mapping[ty] {
            return layout;
        }

        let res = match self.typec.types[ty].kind {
            TyKind::Struct(s) => {
                let mut offsets = bumpvec![cap self.typec.fields[s.fields].len()];

                let layouts = self.typec.fields[s.fields]
                    .to_bumpvec()
                    .into_iter()
                    .map(|field| self.ty_layout(field.ty, params, ptr_ty));

                let mut align = 1;
                let mut size = 0;
                for layout in layouts {
                    align = align.max(layout.align.get());

                    offsets.push(size);

                    let padding = (layout.align.get() - (size as u8 & (layout.align.get() - 1)))
                        & (layout.align.get() - 1);
                    size += padding as u32;
                    size += layout.size;
                }

                let (repr, on_stack) = Self::repr_for_size(size, ptr_ty);

                Layout {
                    repr,
                    offsets: self.gen_layouts.offsets.bump(offsets),
                    align: align.try_into().unwrap(),
                    size,
                    on_stack,
                }
            }
            TyKind::Pointer(..) | TyKind::Integer(TyInteger { size: 0, .. }) => Layout {
                repr: ptr_ty,
                offsets: VSlice::empty(),
                align: (ptr_ty.bytes() as u8).try_into().unwrap(),
                size: ptr_ty.bytes() as u32,
                on_stack: false,
            },
            TyKind::Integer(int) => {
                let (repr, on_stack) = Self::repr_for_size(int.size as u32, ptr_ty);
                Layout {
                    size: int.size as u32,
                    offsets: VSlice::empty(),
                    align: int.size.max(1).try_into().unwrap(),
                    repr,
                    on_stack,
                }
            }
            TyKind::Param(index) => return self.ty_layout(params[index as usize], &[], ptr_ty),
            TyKind::Bool => Layout {
                repr: types::B1,
                offsets: VSlice::empty(),
                align: 1.try_into().unwrap(),
                size: 1,
                on_stack: false,
            },
            TyKind::Instance(inst) => {
                // remap the instance parameters so we can compute the layout correctly
                let params = self.typec.ty_slices[inst.args]
                    .to_bumpvec()
                    .into_iter()
                    .map(|ty| self.typec.instantiate(ty, params, self.interner))
                    .collect::<BumpVec<_>>();
                return self.ty_layout(inst.base, &params, ptr_ty);
            }
            TyKind::Spec(..) => unreachable!(),
        };

        self.gen_layouts.mapping[ty] = res.into();

        res
    }

    fn load_value(&mut self, target: VRef<ValueMir>, builder: &mut GenBuilder) -> ir::Value {
        let loaded = builder.body.values[target]
            .flags
            .contains(ValueMirFlags::LOADED);
        let value = self.gen_resources.values[target].expect("value must be computed by now");
        if loaded {
            let ptr_ty = builder.ptr_ty();
            let layout = self.ty_layout(builder.body.value_ty(target), &[], ptr_ty);
            builder.ins().load(layout.repr, MemFlags::new(), value, 0)
        } else {
            value
        }
    }

    fn save_value(
        &mut self,
        target: VRef<ValueMir>,
        mut value: ir::Value,
        builder: &mut GenBuilder,
    ) {
        let referenced = builder.body.values[target]
            .flags
            .contains(ValueMirFlags::REFERENCED);
        let ptr_ty = builder.ptr_ty();
        let layout = self.ty_layout(builder.body.value_ty(target), &[], ptr_ty);
        if referenced && !layout.on_stack {
            let stack = builder.create_sized_stack_slot(StackSlotData {
                kind: StackSlotKind::ExplicitSlot,
                size: layout.size,
            });

            builder.ins().stack_store(value, stack, 0);
            value = builder.ins().stack_addr(ptr_ty, stack, 0);
        }

        self.gen_resources.values[target] = value.into();
    }

    fn repr_for_size(size: u32, ptr_ty: Type) -> (Type, bool) {
        if size > ptr_ty.bytes() as u32 {
            return (ptr_ty, true);
        }

        (
            match size {
                8.. => types::I64,
                4.. => types::I32,
                2.. => types::I16,
                0.. => types::I8,
            },
            false,
        )
    }
}

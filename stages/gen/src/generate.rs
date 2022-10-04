use cranelift_codegen::{
    ir::{self, types, AbiParam, ExtFuncData, ExternalName, InstBuilder, Type, UserExternalName},
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
        match inst {
            InstMir::Int(value, ret) => {
                let ty = self.ty_repr(builder.body.value_ty(ret), builder.ptr_ty());
                let value = builder.ins().iconst(ty, value);
                self.gen_resources.values[ret] = value.into();
            }
            InstMir::Access(..) => (),
            InstMir::Call(call, ret) => self.call(call, ret, builder),
            InstMir::Const(id, ret) => self.r#const(id, ret, builder),
        }
    }

    fn r#const(&mut self, id: VRef<FuncConstMir>, ret: VRef<ValueMir>, builder: &mut GenBuilder) {
        let value = if builder.isa.jit {
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

            ret.map(|ret| self.gen_resources.values[ret].expect("Value should exist at this point"))
        } else {
            let value = self.gen_resources.func_constants[id]
                .expect("Constant should be computed before function compilation.");
            let ty = self.ty_repr(builder.body.value_ty(ret), builder.ptr_ty());

            Some(match value {
                GenFuncConstant::Int(val) => builder.ins().iconst(ty, val as i64),
            })
        };

        self.gen_resources.values[ret] = value.into();
    }

    fn call(
        &mut self,
        CallMir {
            callable,
            params,
            args,
        }: CallMir,
        ret: VRef<ValueMir>,
        builder: &mut GenBuilder,
    ) {
        match callable {
            CallableMir::Func(func_id) => {
                let args = builder.body.value_args[args]
                    .iter()
                    .map(|&arg| self.gen_resources.values[arg].expand())
                    .collect::<Option<BumpVec<_>>>()
                    .expect("All arguments should be declared.");

                if self.typec.funcs[func_id].flags.contains(FuncFlags::BUILTIN) {
                    self.builtin_call(func_id, args, ret, builder);
                    return;
                }

                let func_ref = self.instantiate(func_id, params, builder);

                let inst = builder.ins().call(func_ref, &args);
                if let Some(&value) = builder.inst_results(inst).first() {
                    self.gen_resources.values[ret] = value.into();
                }
            }
            CallableMir::SpecFunc(_) => todo!(),
            CallableMir::Pointer(_) => todo!(),
        }
    }

    fn builtin_call(
        &mut self,
        func_id: VRef<Func>,
        args: BumpVec<ir::Value>,
        ret: VRef<ValueMir>,
        builder: &mut GenBuilder,
    ) {
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
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        };

        self.gen_resources.values[ret] = value.into();
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
                    let ret = self.gen_resources.values[ret].expect("value mut be defined");
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

                Layout {
                    repr: Self::repr_for_size(size),
                    offsets: self.gen_layouts.offsets.bump(offsets),
                    align: align.try_into().unwrap(),
                    size,
                }
            }
            TyKind::Pointer(..) | TyKind::Integer(TyInteger { size: 0, .. }) => Layout {
                repr: ptr_ty,
                offsets: VSlice::empty(),
                align: (ptr_ty.bytes() as u8).try_into().unwrap(),
                size: ptr_ty.bytes() as u32,
            },
            TyKind::Integer(int) => Layout {
                size: int.size as u32,
                offsets: VSlice::empty(),
                align: int.size.max(1).try_into().unwrap(),
                repr: Self::repr_for_size(int.size as u32),
            },
            TyKind::Param(index) => self.ty_layout(params[index as usize], &[], ptr_ty),
            TyKind::Bool => Layout {
                repr: types::B1,
                offsets: VSlice::empty(),
                align: 1.try_into().unwrap(),
                size: 1,
            },
            TyKind::Instance(inst) => {
                // remap the instance parameters so we can compute the layout correctly
                let params = self.typec.ty_slices[inst.args]
                    .to_bumpvec()
                    .into_iter()
                    .map(|ty| self.typec.instantiate(ty, params, self.interner))
                    .collect::<BumpVec<_>>();
                self.ty_layout(inst.base, &params, ptr_ty)
            }
        };

        self.gen_layouts.mapping[ty] = res.into();

        res
    }

    fn repr_for_size(size: u32) -> Type {
        match size {
            8.. => types::I64,
            4.. => types::I32,
            2.. => types::I16,
            0.. => types::I8,
        }
    }
}

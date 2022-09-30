use cranelift_codegen::{
    ir::{self, types, AbiParam, ExtFuncData, ExternalName, InstBuilder, Type, UserExternalName},
    isa::CallConv,
    Context,
};
use cranelift_frontend::FunctionBuilder;
use mir_t::*;
use storage::*;
use target_lexicon::Triple;
use typec_t::*;

use crate::*;

impl Generator<'_> {
    pub const FUNC_NAMESPACE: u32 = 0;

    pub fn save_compiled_code(&mut self, id: VRef<CompiledFunc>, ctx: &Context) {
        let cc = ctx
            .compiled_code()
            .expect("Expected code already compiled.");
        let relocs = cc
            .buffer
            .relocs()
            .iter()
            .map(|rel| GenReloc {
                offset: rel.offset,
                kind: rel.kind,
                name: match rel.name {
                    ExternalName::User(user) => match &ctx.func.params.user_named_funcs()[user] {
                        &UserExternalName {
                            namespace: Self::FUNC_NAMESPACE,
                            index,
                        } => GenItemName::Func(unsafe { VRef::new(index as usize) }),
                        name => unreachable!("Unexpected name: {:?}", name),
                    },
                    ExternalName::TestCase(_)
                    | ExternalName::LibCall(_)
                    | ExternalName::KnownSymbol(_) => todo!(),
                },
                addend: rel.addend.try_into().expect("Reloc addend too large."),
            })
            .collect::<Vec<_>>();

        let func = CompiledFunc {
            signature: ctx.func.signature.clone(),
            bytecode: cc.buffer.data().to_vec(),
            alignment: cc.alignment as u64,
            relocs,
            ..self.gen.compiled_funcs[id]
        };

        self.gen.compiled_funcs[id] = func;
    }

    pub fn generate(
        &mut self,
        func_id: VRef<Func>,
        func: &FuncMir,
        root: VRef<BlockMir>,
        builder: &mut FunctionBuilder,
        triple: &Triple,
        jit: bool,
    ) {
        builder.func.clear();
        self.gen_resources.clear(triple, jit);

        let Func { signature, .. } = self.typec.funcs[func_id];

        self.load_signature(signature, &mut builder.func.signature);

        self.block(root, func, builder);

        builder.finalize();
    }

    fn block(
        &mut self,
        block: VRef<BlockMir>,
        func: &FuncMir,
        builder: &mut FunctionBuilder,
    ) -> ir::Block {
        let BlockMir {
            args,
            insts,
            control_flow,
            ref_count,
        } = func.blocks[block];

        if let Some(block) = self.gen_resources.blocks[block].as_mut_option() {
            block.visit_count += 1;

            if block.visit_count == ref_count {
                builder.seal_block(block.id);
            }

            return block.id;
        }

        let ir_block = builder.create_block();
        builder.switch_to_block(ir_block);

        for &arg in &func.value_args[args] {
            let ty = self.ty_repr(func.value_ty(arg));
            self.gen_resources.values[arg] = builder.append_block_param(ir_block, ty).into();
        }

        for &inst in &func.insts[insts] {
            self.inst(inst, func, builder);
        }

        self.control_flow(control_flow, func, builder);

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

    fn inst(&mut self, inst: InstMir, func: &FuncMir, builder: &mut FunctionBuilder) {
        match inst {
            InstMir::Int(value, ret) => {
                let ty = self.ty_repr(func.value_ty(ret));
                let value = builder.ins().iconst(ty, value);
                self.gen_resources.values[ret] = value.into();
            }
            InstMir::Access(..) => (),
            InstMir::Call(call, ret) => self.call(call, ret, func, builder),
            InstMir::Const(id, ret) => self.r#const(id, ret, func, builder),
        }
    }

    fn r#const(
        &mut self,
        id: VRef<FuncConstMir>,
        ret: VRef<ValueMir>,
        func: &FuncMir,
        builder: &mut FunctionBuilder,
    ) {
        let value = if self.gen_resources.jit {
            // since this si already compile time, we inline the constant
            // expression

            let block_id = func.constants[id].block;
            let BlockMir {
                insts,
                control_flow,
                ..
            } = func.blocks[block_id];

            for &inst in &func.insts[insts] {
                self.inst(inst, func, builder);
            }

            #[allow(irrefutable_let_patterns)]
            let ControlFlowMir::Return(ret) = control_flow else {
                unreachable!()
            };

            ret.expand().map(|ret| {
                self.gen_resources.values[ret].expect("Value should exist at this point")
            })
        } else {
            let value = self.gen_resources.func_constants[id]
                .expect("Constant should be computed before function compilation.");
            let ty = self.ty_repr(func.value_ty(ret));

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
        func: &FuncMir,
        builder: &mut FunctionBuilder,
    ) {
        match callable {
            CallableMir::Func(func_id) => {
                let args = func.value_args[args]
                    .iter()
                    .map(|&arg| self.gen_resources.values[arg].expand())
                    .collect::<Option<BumpVec<_>>>()
                    .expect("All arguments should be declared.");

                if self.typec.funcs[func_id].flags.contains(FuncFlags::BUILTIN) {
                    self.builtin_call(func_id, args, ret, builder);
                    return;
                }

                let func_ref = self.instantiate(func_id, params, func, builder);

                let inst = builder.ins().call(func_ref, &args);
                if let Some(&value) = builder.inst_results(inst).first() {
                    self.gen_resources.values[ret] = value.into();
                }
            }
            CallableMir::BoundFunc(_) => todo!(),
            CallableMir::Pointer(_) => todo!(),
        }
    }

    fn builtin_call(
        &mut self,
        func_id: VRef<Func>,
        args: BumpVec<ir::Value>,
        ret: VRef<ValueMir>,
        builder: &mut FunctionBuilder,
    ) {
        let Func { signature, loc, .. } = self.typec.funcs[func_id];
        let op_str = &self.interner[loc.name];
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
        func: &FuncMir,
        builder: &mut FunctionBuilder,
    ) -> ir::FuncRef {
        let id = if params.is_empty() {
            let id = self.typec.funcs.id(func_id);
            self.interner
                .intern(ident!(self.gen_resources.triple.as_str(), "&", id))
        } else {
            let start = ident!(
                self.gen_resources.triple.as_str(),
                "&",
                self.typec.funcs.id(func_id),
                "["
            );
            let params = ident_join(
                ", ",
                func.ty_params[params]
                    .iter()
                    .map(|&ty| self.typec.types.id(func.dependant_types[ty].ty)),
            );
            let end = ident!("]");
            let segments = start.into_iter().chain(params).chain(end);
            self.interner.intern(segments)
        };

        if let Some(&imported) = self.gen_resources.func_imports.get(&id) {
            return imported;
        }

        let func = self.gen.compiled_funcs.get_or_insert(id, |s| {
            let params = func.ty_params[params]
                .iter()
                .map(|&ty| func.dependant_types[ty].ty);
            self.compile_requests.add_request(s.next(), func_id, params);
            CompiledFunc::new(func_id)
        });

        let func_ref = self.import_compiled_func(func, builder);

        self.gen_resources.func_imports.insert(id, func_ref);

        func_ref
    }

    pub fn import_compiled_func(
        &mut self,
        func: VRef<CompiledFunc>,
        builder: &mut FunctionBuilder,
    ) -> ir::FuncRef {
        let name = builder
            .func
            .declare_imported_user_function(UserExternalName::new(
                Self::FUNC_NAMESPACE,
                func.index() as u32,
            ));

        let func_id = self.gen.compiled_funcs[func].func;
        let Func {
            signature: sig,
            visibility,
            ..
        } = self.typec.funcs[func_id];

        let mut signature = ir::Signature::new(CallConv::SystemV);
        self.load_signature(sig, &mut signature);
        let signature = builder.import_signature(signature);

        builder.import_function(ExtFuncData {
            name: ExternalName::User(name),
            signature,
            colocated: visibility != FuncVisibility::Imported,
        })
    }

    fn control_flow(
        &mut self,
        control_flow: ControlFlowMir,
        _func: &FuncMir,
        builder: &mut FunctionBuilder,
    ) {
        match control_flow {
            ControlFlowMir::Return(ret) => {
                if let Some(ret) = ret.expand() {
                    let ret = self.gen_resources.values[ret].expect("value mut be defined");
                    builder.ins().return_(&[ret]);
                } else {
                    builder.ins().return_(&[]);
                }
            }
        }
    }

    pub fn load_signature(&mut self, signature: Signature, target: &mut ir::Signature) {
        let cc = self.cc(signature.cc);

        target.clear(cc);
        let args = self.typec.ty_slices[signature.args]
            .iter()
            .map(|&ty| self.ty_repr(ty))
            .map(AbiParam::new);
        target.params.extend(args);

        let ret = self.ty_repr(signature.ret);
        target.returns.push(AbiParam::new(ret));
    }

    fn cc(&self, cc: Maybe<VRef<str>>) -> CallConv {
        let Some(cc) = cc.expand() else {
            return CallConv::Fast;
        };

        self.interner[cc].parse().unwrap_or(CallConv::Fast)
    }

    fn ty_repr(&mut self, ty: VRef<Ty>) -> Type {
        self.ty_layout(ty, &[]).repr
    }

    fn ty_layout(&mut self, ty: VRef<Ty>, params: &[VRef<Ty>]) -> Layout {
        if let Some(layout) = self.gen_layouts.mapping[ty].expand() {
            return layout;
        }

        let res = match self.typec.types[ty].kind {
            TyKind::Struct(s) => {
                let mut offsets = bumpvec![cap self.typec.fields[s.fields].len()];

                let layouts = self.typec.fields[s.fields]
                    .iter()
                    .map(|field| self.ty_layout(field.ty, params));

                let mut align = 1;
                let mut size = 0;
                for layout in layouts {
                    align = align.max(layout.align);

                    offsets.push(size);

                    let padding =
                        (layout.align - (size as u8 & (layout.align - 1))) & (layout.align - 1);
                    size += padding as u32;
                    size += layout.size;
                }

                Layout {
                    repr: Self::repr_for_size(size),
                    offsets: self.gen_layouts.offsets.bump(offsets),
                    align,
                    size,
                }
            }
            TyKind::Pointer(..) | TyKind::Integer(TyInteger { size: 0, .. }) => Layout {
                repr: self.ptr_ty,
                offsets: VSlice::empty(),
                align: self.ptr_ty.bytes() as u8,
                size: self.ptr_ty.bytes() as u32,
            },
            TyKind::Integer(int) => Layout {
                size: int.size as u32,
                offsets: VSlice::empty(),
                align: int.size,
                repr: Self::repr_for_size(int.size as u32),
            },
            TyKind::Param(index) => self.ty_layout(params[index as usize], &[]),
            TyKind::Bool => Layout {
                repr: types::B1,
                offsets: VSlice::empty(),
                align: 1,
                size: 1,
            },
            TyKind::Instance(inst) => {
                // remap the instance parameters so we can compute the layout correctly
                let params = self.typec.ty_slices[inst.args]
                    .iter()
                    .map(|&ty| match self.typec.types[ty].kind {
                        TyKind::Param(index) => params[index as usize],
                        _ => ty,
                    })
                    .collect::<BumpVec<_>>();
                self.ty_layout(inst.base, &params)
            }

            TyKind::Inferred | TyKind::SelfBound => unreachable!(),
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

use cranelift_codegen::{
    entity::EntityRef,
    ir::{
        self, types, AbiParam, ExtFuncData, ExternalName, InstBuilder, Type, UserExternalNameRef,
    },
    isa::CallConv,
    packed_option::PackedOption,
    Context, MachReloc,
};
use cranelift_frontend::FunctionBuilder;
use mir_t::*;
use storage::*;
use typec_t::*;

use crate::*;

impl Generator<'_> {
    pub fn generate(&mut self, func_id: VRef<Func>, func: &FuncMir, builder: &mut FunctionBuilder) {
        builder.func.clear();
        self.ctx.clear();

        let Func { signature, .. } = self.typec.funcs[func_id];

        self.load_signature(signature, &mut builder.func.signature);

        let root = func
            .blocks
            .keys()
            .next()
            .expect("function must have at least one completed block");

        println!("Generating function block {}", root.index());

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

        if let Some(block) = self.ctx.blocks[block].as_mut_option() {
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
            self.ctx.values[arg] = builder.append_block_param(ir_block, ty).into();
        }

        for &inst in &func.insts[insts] {
            self.inst(inst, func, builder);
        }

        self.control_flow(control_flow, func, builder);

        let visit_count = 0;
        if ref_count == visit_count {
            builder.seal_block(ir_block);
        }

        self.ctx.blocks[block] = GenBlock {
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
                self.ctx.values[ret] = value.into();
            }
            InstMir::Access(..) => (),
            InstMir::Call(call, ret) => self.call(call, ret, func, builder),
        }
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
                let func_ref = self.instantiate(func_id, params, func, builder);
                let args = func.value_args[args]
                    .iter()
                    .map(|&arg| self.ctx.values[arg].expand())
                    .collect::<Option<BumpVec<_>>>()
                    .expect("All arguments should be declared.");

                let inst = builder.ins().call(func_ref, &args);
                if let Some(&value) = builder.inst_results(inst).first() {
                    self.ctx.values[ret] = value.into();
                }
            }
            CallableMir::BoundFunc(_) => todo!(),
            CallableMir::Pointer(_) => todo!(),
        }
    }

    fn instantiate(
        &mut self,
        func_id: VRef<Func>,
        params: VRefSlice<MirTy>,
        func: &FuncMir,
        builder: &mut FunctionBuilder,
    ) -> ir::FuncRef {
        let id = if params.is_empty() {
            self.typec.funcs.id(func_id)
        } else {
            let start = ident!(self.typec.funcs.id(func_id), "[");
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

        if let Some(&imported) = self.ctx.func_imports.get(&id) {
            return imported;
        }

        if !self.ctx.compiled_funcs.contains_key(&id) {
            self.ctx.compile_requests.push(CompileRequest {
                id,
                func: func_id,
                params: self.ctx.local_ty_slices.bump(
                    func.ty_params[params]
                        .iter()
                        .map(|&ty| func.dependant_types[ty].ty),
                ),
            });
        }

        let mut signature = ir::Signature::new(CallConv::SystemV);
        let Func {
            signature: sig,
            flags,
            ..
        } = self.typec.funcs[func_id];
        self.load_signature(sig, &mut signature);
        let signature = builder.import_signature(signature);

        let func_ref = builder.import_function(ExtFuncData {
            name: ExternalName::User(UserExternalNameRef::new(id.index())),
            signature,
            colocated: flags.contains(FuncFlags::EXTERN),
        });

        self.ctx.func_imports.insert(id, func_ref);

        func_ref
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
                    let ret = self.ctx.values[ret].expect("value mut be defined");
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
        if let Some(layout) = self.ctx.layouts[ty].expand() {
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
                    offsets: self.ctx.offsets.bump(offsets),
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

        self.ctx.layouts[ty] = res.into();

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

pub type Offset = u32;

pub struct GeneratorCtx {
    // persistent
    pub compiled_funcs: Map<VRef<str>, CompiledFunc>,
    pub compile_requests: Vec<CompileRequest>,
    pub local_ty_slices: BumpMap<VRef<Ty>>,
    pub layouts: ShadowMap<Ty, Maybe<Layout>>,
    pub offsets: BumpMap<Offset>,

    // temp
    pub blocks: ShadowMap<BlockMir, Maybe<GenBlock>>,
    pub values: ShadowMap<ValueMir, PackedOption<ir::Value>>,
    pub func_imports: Map<VRef<str>, ir::FuncRef>,
    pub ctx: Context,
}

impl Default for GeneratorCtx {
    fn default() -> Self {
        Self {
            compiled_funcs: Default::default(),
            compile_requests: Default::default(),
            local_ty_slices: Default::default(),
            layouts: Default::default(),
            offsets: Default::default(),
            blocks: Default::default(),
            values: Default::default(),
            func_imports: Default::default(),
            ctx: Context::new(), // bruh
        }
    }
}

impl GeneratorCtx {
    pub fn clear(&mut self) {
        self.blocks.clear();
        self.values.clear();
        self.func_imports.clear();
    }
}

#[derive(Clone, Copy)]
pub struct Layout {
    pub size: u32,
    pub offsets: VSlice<Offset>,
    pub align: u8,
    pub repr: Type,
}

impl Layout {
    pub const EMPTY: Self = Self {
        size: 0,
        align: 1,
        offsets: VSlice::empty(),
        repr: types::INVALID,
    };
}

impl Invalid for Layout {
    unsafe fn invalid() -> Self {
        Self {
            size: 0,
            align: 0,
            offsets: VSlice::empty(),
            repr: types::INVALID,
        }
    }

    fn is_invalid(&self) -> bool {
        self.align == 0
    }
}

pub struct CompiledFunc {
    pub func: VRef<Func>,
    pub native_bytecode: Vec<u8>,
    pub native_alignment: u64,
    pub target_bytecode: Option<Vec<u8>>,
    pub target_alignment: Option<u64>,
    pub relocs: Vec<MachReloc>,
}

pub struct CompileRequest {
    pub id: VRef<str>,
    pub func: VRef<Func>,
    pub params: VRefSlice<Ty>,
}

#[derive(Clone, Copy)]
pub struct GenBlock {
    pub id: ir::Block,
    pub visit_count: u32,
}

impl Invalid for GenBlock {
    unsafe fn invalid() -> Self {
        Self {
            id: ir::Block::from_u32(0),
            visit_count: u32::MAX,
        }
    }

    fn is_invalid(&self) -> bool {
        self.visit_count == u32::MAX
    }
}

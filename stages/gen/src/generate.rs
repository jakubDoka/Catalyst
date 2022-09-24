use std::num::NonZeroU32;

use cranelift_codegen::{
    ir::{self, types, AbiParam, ExtFuncData, ExternalName, InstBuilder, Type, UserExternalName},
    isa::CallConv,
    packed_option::PackedOption,
    MachReloc,
};
use cranelift_frontend::FunctionBuilder;
use mir_t::*;
use storage::*;
use typec_t::*;

use crate::*;

impl Generator<'_> {
    pub fn generate(&mut self, func_id: VRef<Func>, func: &FuncMir, builder: &mut FunctionBuilder) {
        let Func { signature, .. } = self.typec.funcs[func_id];

        self.load_signature(signature, &mut builder.func.signature);

        let root = func
            .blocks
            .keys()
            .next()
            .expect("function must have at least one completed block");

        self.block(root, func, builder);
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

        let block = builder.create_block();

        for &arg in &func.value_args[args] {
            let ty = self.ty_repr(func.value_ty(arg));
            self.ctx.values[arg] = builder.append_block_param(block, ty).into();
        }

        for &inst in &func.insts[insts] {
            self.inst(inst, func, builder);
        }

        self.control_flow(control_flow, func, builder);

        let visit_count = 0;
        if ref_count == visit_count {}

        todo!()
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

        let name = builder
            .func
            .declare_imported_user_function(UserExternalName {
                namespace: 0,
                index: id.index() as u32,
            });

        let mut signature = ir::Signature::new(CallConv::SystemV);
        let Func {
            signature: sig,
            flags,
            ..
        } = self.typec.funcs[func_id];
        self.load_signature(sig, &mut signature);
        let signature = builder.import_signature(signature);

        let func_ref = builder.import_function(ExtFuncData {
            name: ExternalName::User(name),
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

    fn load_signature(&self, signature: Signature, target: &mut ir::Signature) {
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

    fn ty_repr(&self, ty: VRef<Ty>) -> Type {
        match self.typec.types[ty].kind {
            TyKind::Pointer(..) => self.isa.pointer_type(),
            TyKind::Integer(int) => match (int.width, int.signed) {
                (64, true) => types::I64,
                (64, false) => types::I64,
                (32, true) => types::I32,
                (32, false) => types::I32,
                (16, true) => types::I16,
                (16, false) => types::I16,
                (8, true) => types::I8,
                (8, false) => types::I8,
                _ => self.isa.pointer_type(),
            },
            TyKind::Bool => types::B1,

            TyKind::Struct(..) => todo!(),
            TyKind::Instance(..) => todo!(),
            TyKind::Param(..) | TyKind::SelfBound | TyKind::Inferred => unreachable!(),
        }
    }
}

#[derive(Default)]
pub struct GeneratorCtx {
    // persistent
    pub compiled_funcs: Map<VRef<str>, CompiledFunc>,
    pub compile_requests: Vec<CompileRequest>,
    pub local_ty_slices: BumpMap<VRef<Ty>>,
    pub layouts: ShadowMap<Ty, Maybe<Layout>>,

    // temp
    pub blocks: ShadowMap<BlockMir, Maybe<GenBlock>>,
    pub values: ShadowMap<ValueMir, PackedOption<ir::Value>>,
    pub func_imports: Map<VRef<str>, ir::FuncRef>,
}

pub struct Layout {
    pub size: u32,
    pub align: NonZeroU32,
    pub repr: Type,
}

impl Invalid for Layout {
    unsafe fn invalid() -> Self {
        Self {
            size: 0,
            align: NonZeroU32::new_unchecked(1),
            repr: types::INVALID,
        }
    }

    fn is_invalid(&self) -> bool {
        self.repr == types::INVALID
    }
}

pub struct CompiledFunc {
    pub func: VRef<Func>,
    pub native_bytecode: Vec<u8>,
    pub target_bytecode: Option<Vec<u8>>,
    pub relocs: Vec<MachReloc>,
}

pub struct CompileRequest {
    pub id: VRef<str>,
    pub func: VRef<Func>,
    pub params: VRefSlice<Ty>,
}

impl GeneratorCtx {
    pub fn clear(&mut self) {
        self.blocks.clear();
        self.values.clear();
        self.func_imports.clear();
    }
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
            visit_count: 0,
        }
    }

    fn is_invalid(&self) -> bool {
        self.visit_count == u32::MAX
    }
}

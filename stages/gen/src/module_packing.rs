use cranelift_codegen::ir::InstBuilder;
use cranelift_codegen::{
    entity::EntityRef,
    ir::{self, types, ExternalName, UserExternalName},
    isa::CallConv,
    MachReloc,
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{FuncId, Linkage, Module};
use storage::*;
use target_lexicon::Triple;
use typec_t::*;

use crate::*;

pub const FUNC_NAMESPACE: u32 = 0;
pub const DATA_NAMESPACE: u32 = 1;

impl Generator<'_> {
    pub fn create_entrypoint(
        &mut self,
        name: &str,
        functions: &[VRef<str>],
        func_refs: &mut Map<VRef<str>, FuncId>,
        module: &mut impl Module,
    ) {
        self.ctx.ctx.func.clear();
        self.ctx
            .ctx
            .func
            .signature
            .params
            .extend([types::I64, types::I64].map(ir::AbiParam::new));

        let func_ref = module
            .declare_function(name, Linkage::Export, &self.ctx.ctx.func.signature)
            .expect("expected to call this only once and with unique name");

        let mut ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut self.ctx.ctx.func, &mut ctx);

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        let args = builder.block_params(entry_block).to_vec();

        for &entry_id in functions {
            let func_id = func_refs[&entry_id];
            let param_count = module
                .declarations()
                .get_function_decl(func_id)
                .signature
                .params
                .len();
            let func = module.declare_func_in_func(func_id, builder.func);
            builder
                .ins()
                .call(func, args.get(..param_count).unwrap_or(&[]));
        }

        builder.ins().return_(&[]);
        builder.seal_block(entry_block);

        builder.finalize();

        module
            .define_function(func_ref, &mut self.ctx.ctx)
            .expect("expected to call this only once");
    }

    pub fn declare_funcs(
        &mut self,
        functions: &[VRef<str>],
        func_refs: &mut Map<VRef<str>, FuncId>,
        module: &mut impl Module,
    ) {
        let mut sig = ir::Signature::new(CallConv::Fast);
        for &func_instance_id in functions {
            let CompiledFunc { func, .. } = self.ctx.compiled_funcs[&func_instance_id];
            let Func { signature, .. } = self.typec.funcs[func];
            self.load_signature(signature, &mut sig);

            let func_ref = module
                .declare_function(&self.interner[func_instance_id], Linkage::Export, &sig)
                .expect("declaring function should be performed only once");
            func_refs.insert(func_instance_id, func_ref);
        }
    }

    pub fn define_funcs(
        &mut self,
        functions: &[VRef<str>],
        func_refs: &Map<VRef<str>, FuncId>,
        module: &mut impl Module,
    ) {
        let is_native = module.isa().triple() == &Triple::host();
        let mut reloc_buffer = bumpvec![];
        for &func_instance_id in functions {
            let func_ref = func_refs[&func_instance_id];
            let compiled_func = &self.ctx.compiled_funcs[&func_instance_id];

            self.ctx.ctx.func.clear();
            reloc_buffer.clear();
            reloc_buffer.reserve(compiled_func.relocs.len());
            for reloc in &compiled_func.relocs {
                let ExternalName::User(index) = reloc.name else {
                    unreachable!();
                };

                let id = unsafe { VRef::<str>::new(index.index()) };

                let name = if let Some(&func_ref) = func_refs.get(&id) {
                    let name = UserExternalName {
                        namespace: FUNC_NAMESPACE,
                        index: func_ref.as_u32(),
                    };
                    self.ctx.ctx.func.params.ensure_user_func_name(name)
                } else {
                    unimplemented!();
                };

                let reloc = MachReloc {
                    name: ExternalName::User(name),
                    ..*reloc
                };

                reloc_buffer.push(reloc);
            }

            let (code, alignment) = if is_native {
                (
                    &compiled_func.native_bytecode,
                    compiled_func.native_alignment,
                )
            } else {
                (
                    compiled_func
                        .target_bytecode
                        .as_ref()
                        .expect("target bytecode should be generated ast this point"),
                    compiled_func
                        .target_alignment
                        .expect("target alignment should be present at this point"),
                )
            };

            module
                .define_function_bytes(func_ref, &self.ctx.ctx.func, alignment, code, &reloc_buffer)
                .expect("defining function should be performed only once");
        }
    }
}

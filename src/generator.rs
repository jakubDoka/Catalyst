use cranelift_codegen::{ir::Signature, isa::CallConv};

use crate::*;
use incr::*;

impl Generator<'_> {
    pub fn generate(&mut self) {
        let ptr_ty = self.isa.pointer_type();
        let sys_cc = self.isa.default_call_conv();

        for &(func, _) in self.to_compile.iter() {
            let call_conv = self.funcs[func.meta()].sig.cc;
            let sig = self.funcs[func.meta()].sig;
            self.signatures.insert(
                func,
                translate_signature(
                    call_conv,
                    self.ty_lists.get(sig.args).iter().copied(),
                    sig.ret,
                    &self.reprs,
                    &self.types,
                    sys_cc,
                ),
            );
        }

        while let Some((id, params)) = self.to_compile.pop() {
            let func_ent = self.funcs[id];

            if self.skip_incrementally(id, func_ent.id) {
                continue;
            }

            if !self.funcs[id.meta()].name.is_reserved_value() {
                println!("{}", self.sources. display(self.funcs[id.meta()].name));
            }

            self.load_generic_params(id, params, ptr_ty);

            let tir = std::mem::take(&mut self.funcs[id.meta()].tir_data);
            let result = mir_builder!(self, id, ptr_ty, sys_cc, tir).func();

            self.funcs[id.meta()].tir_data = tir;

            if result.is_err() {
                continue;
            }

            println!("{}", MirDisplay::new(&self.sources, &self.ty_lists, &self.func_ctx, &self.types));

            self.context.func.signature = self.signatures.get(id).unwrap().clone();

            self.build_cir_and_emit(id, true);
        }
    }

    pub fn build_cir_and_emit(&mut self, id: Func, save: bool) {
        let builder = FunctionBuilder::new(
            &mut self.context.func,
            &mut self.generation_context.builder_context,
        );

        cir_builder!(self, builder, *self.isa).generate();

        // if !self.funcs[id.meta()].name.is_reserved_value() {
        //     println!("{}", self.sources. display(self.funcs[id.meta()].name));
        // }
        // println!("{}", self.context.func.display());

        self.generation_context
            .replace_cache
            .replace(self.types, self.reprs);

        let mut bytes = vec![];
        self.context.compile_and_emit(self.isa, &mut bytes).unwrap();
        let relocs = self
            .context
            .mach_compile_result
            .as_ref()
            .unwrap()
            .buffer
            .relocs()
            .to_owned();

        let compile_result = CompileResult { bytes, relocs };

        let signature = std::mem::replace(
            &mut self.context.func.signature,
            Signature::new(CallConv::Fast),
        );

        if save {
            self.save_compile_result(id, signature, &compile_result);
        }

        self.compile_results[id] = compile_result;
        self.context.clear();
    }

    fn save_compile_result(
        &mut self,
        func: Func,
        signature: Signature,
        compile_result: &CompileResult,
    ) {
        let reloc_records = compile_result
            .relocs
            .iter()
            .map(|r| {
                let ExternalName::User {
                namespace,
                index,
            } = r.name else {
                unreachable!();
            };

                let name = match namespace {
                    FUNC_NAMESPACE => self.funcs[Func(index)].id,
                    DATA_NAMESPACE => self.globals[Global(index)].id,
                    _ => unreachable!(),
                };

                IncrRelocRecord {
                    offset: r.offset,
                    srcloc: r.srcloc,
                    kind: r.kind,
                    name,
                    namespace,
                    addend: r.addend,
                }
            })
            .collect::<Vec<_>>();

        let incr_func_data = IncrFunc {
            signature,
            temp_id: None,
            defined: false,
            bytes: compile_result.bytes.clone(),
            reloc_records,
        };

        let source = self.funcs[func.meta()].name.source();
        let module_id = self.modules[source].id;
        let func_id = self.funcs[func].id;

        self.incr_modules
            .get_mut(module_id)
            .unwrap()
            .owned_funcs
            .insert(func_id, ());

        self.incr_funcs.insert(func_id, incr_func_data);
    }

    fn load_generic_params(&mut self, id: Func, params: TyList, ptr_ty: Type) {
        repr_instancing!(self, ptr_ty).load_generic_types(
            params,
            self.funcs[id.meta()].tir_data.used_types,
            &mut self.generation_context.replace_cache,
        );
    }

    pub fn skip_incrementally(&mut self, func: Func, id: ID) -> bool {
        let Some(incr_func_data) = self.incr_funcs.get(id) else {
            return false;
        };

        self.generation_context
            .frontier
            .extend(incr_func_data.function_dependencies());

        let mut used_incr_funcs = vec![];
        while let Some(id) = self.generation_context.frontier.pop() {
            if let Some(shadow) = self.func_instances.insert(id, self.funcs.next_key()) {
                self.func_instances.insert(id, shadow).unwrap();
                continue;
            }

            let func_ent = FuncEnt {
                id,
                ..Default::default()
            };

            let func = self.funcs.push_instance(func_ent, Func::reserved_value());
            self.generation_context.reused_incr_funcs.push(func);
            let incr_func_data = self.incr_funcs.get(id).unwrap();

            used_incr_funcs.push(func);

            self.signatures
                .insert(func, incr_func_data.signature.clone());
            self.generation_context
                .frontier
                .extend(incr_func_data.function_dependencies());
        }

        self.compile_results[func] = self.incr_data_to_compile_result(incr_func_data);

        for func in used_incr_funcs {
            let id = self.funcs[func].id;
            let incr_func_data = self.incr_funcs.get(id).unwrap();
            self.compile_results[func] = self.incr_data_to_compile_result(incr_func_data);
        }

        true
    }

    fn incr_data_to_compile_result(&self, incr_func_data: &IncrFunc) -> CompileResult {
        let bytes = incr_func_data.bytes.clone();
        let relocs = incr_func_data
            .reloc_records
            .iter()
            .map(|rec| {
                let id = match rec.namespace {
                    FUNC_NAMESPACE => self.func_instances.get(rec.name).unwrap().clone().as_u32(),
                    DATA_NAMESPACE => self.global_map.get(rec.name).unwrap().clone().as_u32(),
                    _ => unreachable!(),
                };
                MachReloc {
                    offset: rec.offset,
                    srcloc: rec.srcloc,
                    kind: rec.kind,
                    name: ExternalName::user(rec.namespace, id),
                    addend: rec.addend,
                }
            })
            .collect();

        CompileResult { bytes, relocs }
    }
}

pub struct GenerationContext {
    pub reused_incr_funcs: Vec<Func>,
    pub frontier: Vec<ID>,
    pub builder_context: FunctionBuilderContext,
    pub replace_cache: ReplaceCache,
}

impl GenerationContext {
    pub fn new() -> GenerationContext {
        GenerationContext {
            reused_incr_funcs: Vec::new(),
            frontier: Vec::new(),
            builder_context: FunctionBuilderContext::new(),
            replace_cache: ReplaceCache::new(),
        }
    }
}

use cranelift_codegen::{ir::Signature, isa::CallConv};

use crate::*;

impl Generator<'_> {
    pub fn generate(&mut self) -> Option<ID> {
        let ptr_ty = self.isa.pointer_type();
        let sys_cc = self.isa.default_call_conv();

        for &(func, _) in self.to_compile.iter() {
            let call_conv = self.funcs[func].flags.call_conv();
            let sig = self.func_meta[func].sig;
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

        let mut entry_id = None;
        while let Some((id, params)) = self.to_compile.pop() {

            let func_ent = self.funcs[id];

            if func_ent.flags.contains(FuncFlags::ENTRY) {
                if entry_id.is_some() {
                    println!("multiple entry points");
                    exit!();
                }
                entry_id = Some(func_ent.id);
            }

            if self.skip_incrementally(id, func_ent.id) {
                continue;
            }

            let parent = self.load_generic_params(
                id, 
                params, 
                func_ent.parent, 
                ptr_ty, 
            );

            let result = mir_builder!(self, parent, ptr_ty, sys_cc, self.func_bodies[parent])
                .func();

            if result.is_err() {
                continue;
            }

            // println!("{}", MirDisplay::new(&self.sources, &self.ty_lists, &self.func_ctx, &self.types));

            self.context.func.signature = self.signatures.get(id).unwrap().clone();

            let builder = FunctionBuilder::new(
                &mut self.context.func, 
                &mut self.generation_context.builder_context
            );


            cir_builder!(self, builder, *self.isa).generate();
            
            // for (id, repr) in self.reprs.iter() {
            //     if repr.repr == INVALID {
            //         println!("{}", ty_display!(self, id));
            //     }
            // }
            
            self.generation_context.replace_cache.replace(self.types, self.reprs);

            let mut bytes = vec![];
            self.context.compile_and_emit(self.isa, &mut bytes)
                .unwrap();
            let relocs = self.context
                .mach_compile_result
                .as_ref()
                .unwrap()
                .buffer
                .relocs()
                .to_vec();

            let compile_result = CompileResult { bytes, relocs };

            let signature = std::mem::replace(&mut self.context.func.signature, Signature::new(CallConv::Fast));

            self.save_compile_result(parent, func_ent.id, signature, &compile_result);

            self.compile_results[id] = compile_result;
            self.context.clear();
        }
        
        entry_id
    }

    fn save_compile_result(&mut self, parent: Func, func_id: ID, signature: Signature, compile_result: &CompileResult) {        
        let reloc_records = compile_result.relocs.iter().map(|r| {
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
        }).collect::<Vec<_>>();

        let incr_func_data = IncrFunc {
            signature,
            temp_id: None,
            defined: false,
            bytes: compile_result.bytes.clone(),
            reloc_records,
        };

        let source = self.func_meta[parent].name.source();
        let module_id = self.modules[source].id;

        self.incr_modules.get_mut(module_id)
            .unwrap().owned_funcs.insert(func_id, ());

        self.incr_funcs.insert(func_id, incr_func_data);
    }

    fn load_generic_params(
        &mut self,
        id: Func,
        params: TyList,
        parent: PackedOption<Func>,
        ptr_ty: Type,
    ) -> Func {
        let Some(parent) = parent.expand() else {
            return id;
        };

        ReprInstancing {
            types: &mut self.types,
            ty_lists: &mut self.ty_lists,
            instances: &mut self.instances,
            ty_comps: &self.ty_comps,
            sources: &self.sources,
            repr_fields: &mut self.repr_fields,
            reprs: &mut self.reprs,
            ptr_ty,
        }
        .load_generic_types(
            params, 
            self.func_bodies[parent].used_types, 
            &mut self.generation_context.replace_cache
        );

        parent
    }

    pub fn skip_incrementally(
        &mut self,
        func: Func,
        id: ID,
    ) -> bool {
        let Some(incr_func_data) = self.incr_funcs.get(id) else {
            return false;
        };

        self.generation_context.frontier.extend(incr_func_data.dependencies());

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


            let func = self.funcs.push(func_ent);
            self.generation_context.reused_incr_funcs.push(func);
            let incr_func_data = self.incr_funcs.get(id).unwrap();

            used_incr_funcs.push(func);
            
            self.signatures.insert(func, incr_func_data.signature.clone());
            self.generation_context.frontier.extend(incr_func_data.dependencies());
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
                    DATA_NAMESPACE => todo!(),
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
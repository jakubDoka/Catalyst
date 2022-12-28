use cranelift_codegen::{
    ir::{AbiParam, ArgumentPurpose, ExtFuncData, ExternalName, UserExternalName},
    isa::CallConv,
};

use super::*;

impl Generator<'_> {
    pub fn func_instance_name(
        jit: bool,
        triple: &str,
        func_id: FragRef<Func>,
        mut params: impl Iterator<Item = Ty>,
        typec: &Typec,
        interner: &mut Interner,
    ) -> Ident {
        use std::fmt::Write;
        let prefix = if jit { "jit-" } else { "native-" };
        interner.intern_with(|s, t| {
            t.push_str(prefix);
            write!(t, "{triple}").unwrap();
            typec.func_name(func_id, t, s);
            if let Some(first) = params.next() {
                t.push('[');
                typec.display_ty_to(first, t, s);
                for ty in params.by_ref() {
                    t.push_str(", ");
                    typec.display_ty_to(ty, t, s);
                }
                t.push(']');
            };
        })
    }

    pub fn import_compiled_func(
        &mut self,
        func: FragRef<CompiledFunc>,
        params: impl Iterator<Item = Ty>,
        builder: &mut GenBuilder,
    ) -> (ir::FuncRef, bool) {
        if let Some(&res) = self.gen_resources.func_imports.get(&func) {
            return res;
        }

        let name = builder
            .func
            .declare_imported_user_function(UserExternalName::new(
                Gen::FUNC_NAMESPACE,
                func.to_u32(),
            ));

        let func_id = self.gen[func].func;
        let Func {
            signature,
            visibility,
            ..
        } = self.typec[func_id];

        let params = params.collect::<BumpVec<_>>();

        let (signature, struct_ret) = self.load_signature(signature, &params, builder.system_cc());
        let signature = builder.import_signature(signature);

        let res = (
            builder.import_function(ExtFuncData {
                name: ExternalName::User(name),
                signature,
                colocated: visibility != FuncVisibility::Imported,
            }),
            struct_ret,
        );

        self.gen_resources.func_imports.insert(func, res);

        res
    }

    pub fn load_signature(
        &mut self,
        signature: Signature,
        params: &[Ty],
        system_cc: CallConv,
    ) -> (ir::Signature, bool) {
        let mut sig = self.gen_resources.reuse_signature();
        let struct_ret = self.populate_signature(signature, params, &mut sig, system_cc);
        (sig, struct_ret)
    }

    pub fn populate_signature(
        &mut self,
        signature: Signature,
        params: &[Ty],
        target: &mut ir::Signature,
        system_cc: CallConv,
    ) -> bool {
        let cc = self.cc(signature.cc, system_cc);
        target.clear(cc);

        let instance = self.typec.instantiate(signature.ret, params, self.interner);
        let on_stack = self.ty_layout(instance).on_stack;
        if on_stack {
            target.params.push(AbiParam::special(
                self.gen_layouts.ptr_ty,
                ArgumentPurpose::StructReturn,
            ));
        }

        let args = self.typec[signature.args]
            .to_bumpvec()
            .into_iter()
            .filter_map(|ty| {
                let instance = self.typec.instantiate(ty, params, self.interner);
                let layout = self.ty_layout(instance);
                (layout.size != 0).then_some(layout.repr)
            })
            .map(AbiParam::new);
        target.params.extend(args);

        let instance_layout = self.ty_layout(instance);
        if instance_layout.size != 0 && !on_stack {
            target.returns.push(AbiParam::new(instance_layout.repr));
        }
        on_stack
    }

    fn cc(&self, cc: Option<Ident>, system_cc: CallConv) -> CallConv {
        let Some(cc) = cc else {
            return CallConv::Fast;
        };

        if &self.interner[cc] == "default" {
            return system_cc;
        }

        self.interner[cc].parse().unwrap_or(CallConv::Fast)
    }
}

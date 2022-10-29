use cranelift_codegen::{
    ir::{AbiParam, ArgumentPurpose, ExtFuncData, ExternalName, UserExternalName},
    isa::CallConv,
};

use super::*;

impl Generator<'_> {
    pub fn func_instance_name(
        jit: bool,
        triple: &str,
        func_id: VRef<Func>,
        mut params: impl Iterator<Item = Ty>,
        typec: &Typec,
        interner: &mut Interner,
    ) -> VRef<str> {
        use std::fmt::Write;
        let prefix = if jit { "jit-" } else { "native-" };
        interner.intern_with(|s, t| {
            t.push_str(prefix);
            write!(t, "{}", triple).unwrap();
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
        func: VRef<CompiledFunc>,
        params: impl Iterator<Item = Ty>,
        builder: &mut GenBuilder,
    ) -> (ir::FuncRef, bool) {
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

        let params = params.collect::<BumpVec<_>>();

        let (signature, struct_ret) = self.load_signature(signature, &params, builder.system_cc());
        let signature = builder.import_signature(signature);

        (
            builder.import_function(ExtFuncData {
                name: ExternalName::User(name),
                signature,
                colocated: visibility != FuncVisibility::Imported,
            }),
            struct_ret,
        )
    }

    pub fn load_signature(
        &mut self,
        signature: Signature,
        params: &[Ty],
        system_cc: CallConv,
    ) -> (ir::Signature, bool) {
        let mut sig = ir::Signature::new(CallConv::Fast);
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

        let args = self.typec.args[signature.args]
            .to_bumpvec()
            .into_iter()
            .map(|ty| {
                let instance = self.typec.instantiate(ty, params, self.interner);
                self.ty_repr(instance)
            })
            .map(AbiParam::new);
        target.params.extend(args);

        if instance != Ty::UNIT {
            let ret = self.ty_repr(instance);
            let ret = if on_stack {
                AbiParam::special(ret, ArgumentPurpose::StructReturn)
            } else {
                AbiParam::new(ret)
            };
            target.returns.push(ret);
        }
        on_stack
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
}

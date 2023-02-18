use {
    super::*,
    cranelift_codegen::{
        ir::{AbiParam, ArgumentPurpose, ExtFuncData, ExternalName},
        isa::CallConv,
    },
};

pub mod abi {
    use cranelift_codegen::{
        ir::{AbiParam, ArgumentExtension, ArgumentPurpose, Type},
        isa::CallConv,
    };
    use target_lexicon::Architecture;
    use typec_t::Ty;

    use crate::{Isa, Layout};

    pub mod x86_64;

    #[derive(Clone, Copy, Debug)]
    pub enum PassMode {
        Pair(Type, Type),
        Single(Type, ArgumentExtension),
        Indirect(Type, u32),
    }

    impl PassMode {
        pub(crate) fn as_abi_array(self) -> [Option<AbiParam>; 2] {
            match self {
                PassMode::Pair(a, b) => [a, b].map(AbiParam::new).map(Some),
                PassMode::Single(s, ext) => [
                    Some(AbiParam {
                        value_type: s,
                        purpose: ArgumentPurpose::Normal,
                        extension: ext,
                    }),
                    None,
                ],
                PassMode::Indirect(ptr, size) => [
                    Some(AbiParam::special(
                        ptr,
                        ArgumentPurpose::StructArgument(Layout::aligned_to(size, ptr.bytes())),
                    )),
                    None,
                ],
            }
        }
    }

    #[derive(Default, Clone)]
    pub struct PassSignature {
        pub args: Vec<PassMode>,
        pub ret: Option<PassMode>,
    }

    pub(super) fn compute_abi_info(
        generator: &mut crate::Generator,
        cc: CallConv,
        signature: typec_t::Signature,
        target: &mut PassSignature,
        params: &[Ty],
        isa: &Isa,
    ) {
        use Architecture::*;
        match isa.triple().architecture {
            Unknown => todo!(),
            Arm(_) => todo!(),
            AmdGcn => todo!(),
            Aarch64(_) => todo!(),
            Asmjs => todo!(),
            Avr => todo!(),
            Bpfeb => todo!(),
            Bpfel => todo!(),
            Hexagon => todo!(),
            X86_32(_) => todo!(),
            M68k => todo!(),
            Mips32(_) => todo!(),
            Mips64(_) => todo!(),
            Msp430 => todo!(),
            Nvptx64 => todo!(),
            Powerpc => todo!(),
            Powerpc64 => todo!(),
            Powerpc64le => todo!(),
            Riscv32(_) => todo!(),
            Riscv64(_) => todo!(),
            S390x => todo!(),
            Sparc => todo!(),
            Sparc64 => todo!(),
            Sparcv9 => todo!(),
            Wasm32 => todo!(),
            Wasm64 => todo!(),
            X86_64 => match cc {
                CallConv::Cold => todo!(),
                CallConv::SystemV | CallConv::Fast => {
                    x86_64::compute_abi_info(generator, signature, params, target)
                }
                CallConv::WindowsFastcall => todo!(),
                CallConv::AppleAarch64 => todo!(),
                CallConv::Probestack => todo!(),
                CallConv::WasmtimeSystemV => todo!(),
                CallConv::WasmtimeFastcall => todo!(),
                CallConv::WasmtimeAppleAarch64 => todo!(),
            },
            XTensa => todo!(),
            _ => todo!(),
        }
    }

    fn extension_for(ty: Ty) -> ArgumentExtension {
        if ty.is_signed() {
            ArgumentExtension::Sext
        } else if ty.is_unsigned() {
            ArgumentExtension::Uext
        } else {
            ArgumentExtension::None
        }
    }
}

impl Generator<'_> {
    pub fn func_instance_name(
        jit: bool,
        triple: &str,
        func_id: FragRef<Func>,
        mut params: impl Iterator<Item = Ty>,
        typec: &Typec,
        interner: &mut Interner,
    ) -> FragRef<&'static str> {
        use std::fmt::Write;
        let prefix = if jit { "jit-" } else { "native-" };
        interner.intern_with_compressed(|s, t| {
            t.push_str(prefix);
            write!(t, "{triple}")?;
            typec_u::display_func_name(typec, s, func_id, t)?;
            typec_u::display_list(typec, s, params.by_ref(), t, ['[', ',', ']'])
        })
    }

    pub fn import_compiled_func(
        &mut self,
        func: CompiledFuncRef,
        params: impl Iterator<Item = Ty>,
        builder: &mut GenBuilder,
    ) -> (ir::FuncRef, bool, PassSignature) {
        if let Some(res) = self.gen_resources.func_imports.get(&func) {
            return res.clone();
        }

        let name = builder
            .func
            .declare_imported_user_function(GenItemName::encode_func(func));

        let func_id = self.gen.get_func_direct(func).func();
        let Func {
            signature,
            visibility,
            ..
        } = self.typec[func_id];

        let params = params.collect::<BumpVec<_>>();

        let (signature, struct_ret, pass_signature) =
            self.load_signature(signature, &params, builder.isa);
        let signature = builder.import_signature(signature);

        let res = (
            builder.import_function(ExtFuncData {
                name: ExternalName::User(name),
                signature,
                colocated: visibility != FuncVisibility::Imported,
            }),
            struct_ret,
            pass_signature,
        );

        self.gen_resources.func_imports.insert(func, res.clone());

        res
    }

    pub fn load_signature(
        &mut self,
        signature: Signature,
        params: &[Ty],
        isa: &Isa,
    ) -> (ir::Signature, bool, PassSignature) {
        let (mut sig, mut pass_sig) = self.gen_resources.reuse_signature();
        let struct_ret = self.populate_signature(signature, params, &mut sig, &mut pass_sig, isa);
        (sig, struct_ret, pass_sig)
    }

    pub fn populate_signature(
        &mut self,
        signature: Signature,
        params: &[Ty],
        target: &mut ir::Signature,
        pass_target: &mut PassSignature,
        isa: &Isa,
    ) -> bool {
        let cc = self.cc(signature.cc, isa.default_call_conv());
        abi::compute_abi_info(self, cc, signature, pass_target, params, isa);
        target.clear(cc);

        let on_stack = matches!(pass_target.ret, Some(PassMode::Indirect(..)));
        if on_stack {
            target.params.push(AbiParam::special(
                self.layouts.ptr_ty,
                ArgumentPurpose::StructReturn,
            ));
        }

        pass_target
            .args
            .iter()
            .flat_map(|a| a.as_abi_array())
            .flatten()
            .collect_into(&mut target.params);

        if !on_stack && let Some(ret) = pass_target.ret {
            target.returns.extend(ret.as_abi_array().into_iter().flatten());
        }

        on_stack
    }

    fn cc(&self, cc: Option<Ident>, system_cc: CallConv) -> CallConv {
        let Some(cc) = cc else {
            return CallConv::Fast;
        };

        if cc.get(self.interner) == "default" {
            return system_cc;
        }

        cc.get(self.interner).parse().unwrap_or(CallConv::Fast)
    }
}

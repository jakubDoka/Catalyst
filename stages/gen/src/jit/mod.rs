use std::ffi::CStr;
use std::ops::Not;
use std::{default::default, mem};

use cranelift_codegen::ir::{self, ArgumentPurpose, InstBuilder};
use cranelift_codegen::CompiledCode;
use cranelift_codegen::{binemit::Reloc, ir::LibCall};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use lexing::*;
use mir::Mir;
use storage::*;
use types::*;

use crate::*;

pub const EXPOSED_FUNCS: &[(&str, *const u8)] = &[("ctl_lexer_next", ctl_lexer_next as _)];

pub trait Jitter {
    fn jit(&mut self, ctx: &mut JitterCtx) -> Result<(), JitterError>;
}

pub struct JitterCtx<'a> {
    pub func: FragRef<Func>,
    pub params: &'a [Ty],
    pub gen: &'a mut Gen,
    pub mir: &'a Mir,
    pub types: &'a mut Types,
    pub interner: &'a mut Interner,
    pub layouts: &'a mut GenLayouts,
    pub jit: &'a mut JitContext,
}

compose_error!(JitterError {});

#[derive(Clone, Copy)]
pub struct JittedFunc<'a> {
    ptr: *const u8,
    adapter: extern "C" fn(*const u8, *const i64, *mut i64),
    lt: std::marker::PhantomData<&'a ()>,
}

impl<'a> JittedFunc<'a> {
    /// # Safety
    /// Caller has to guarantee that the function pointer has correct signature for the adapter
    /// and that arguments have correct lenght as well as rets.
    pub unsafe fn call(&self, args: &[i64], ret: *mut i64) {
        (self.adapter)(self.ptr, args.as_ptr(), ret);
    }
}

#[derive(Clone, Copy)]
struct InternalJittedFunc {
    ptr: JittedFuncPtr,
    adapter: Option<Code>,
}

#[derive(Clone, Copy)]
enum JittedFuncPtr {
    Generated(Code),
    Static(*const u8),
}

impl JittedFuncPtr {
    fn as_raw_ptr(self, gen: &Gen) -> *const u8 {
        match self {
            JittedFuncPtr::Generated(code) => {
                let code = gen.finalized_code(&code);
                code.instructions().as_ptr()
            }
            JittedFuncPtr::Static(s) => s,
        }
    }
}

pub struct JitContext {
    exposed_funcs: Map<&'static str, *const u8>,
    functions: Map<CompiledFuncRef, InternalJittedFunc>,
    runtime_lookup: RuntimeFunctionLookup,
}

impl JitContext {
    pub fn new(exposed_funcs: impl IntoIterator<Item = (&'static str, *const u8)>) -> Self {
        Self {
            exposed_funcs: exposed_funcs
                .into_iter()
                .chain(EXPOSED_FUNCS.iter().copied())
                .collect(),
            //       functions: Map::default(),
            //      resources: JitResources::new(),
            functions: default(),
            runtime_lookup: RuntimeFunctionLookup::new(),
        }
    }

    /// # Safety
    /// Caller has to guarantee that the function pointer has correct signature.
    pub fn get_function<'a>(
        &mut self,
        func: CompiledFuncRef,
        gen: &'a mut Gen,
        ctx: &mut cranelift_codegen::Context,
        fctx: &mut FunctionBuilderContext,
        isa: &Isa,
    ) -> Option<JittedFunc<'a>> {
        let f = self.functions.get_mut(&func)?;
        let ptr = f.ptr.as_raw_ptr(gen);
        let adapter = {
            let code = f.adapter.get_or_insert_with(|| {
                let func = gen.get_func_direct(func);
                let compiled = adapter_for(&func.signature, ctx, fctx, isa);
                drop(func);
                gen.save_adapter(compiled)
            });
            let code = gen.finalized_code(code);
            unsafe { mem::transmute(code.instructions().as_ptr()) }
        };
        Some(JittedFunc {
            ptr,
            adapter,
            lt: std::marker::PhantomData,
        })
    }

    pub fn get_function_ptr(&self, func: CompiledFuncRef, gen: &Gen) -> Option<*const u8> {
        self.functions.get(&func).map(|f| f.ptr.as_raw_ptr(gen))
    }

    /// If function is already loaded this call does nothing.
    /// # Safety
    /// The caller must ensure signature of `func` matches the function pointer
    /// signature passed as `ptr`, `func` also has to have default call convention
    /// `ptr` should be `export "C"`.
    // pub unsafe fn load_runtime_function(&mut self, func: CompiledFuncRef, ptr: *const u8) {
    //     if self.functions.get(&func).is_some() {
    //         return;
    //     }

    //     self.functions.insert(
    //         func,
    //         InternalJittedFunc {
    //             ptr: JittedFuncPtr::Static(ptr),
    //         },
    //     );
    // }

    /// Used for loading batch of functions that are currently needed to execute.
    /// Functions can depend on each other. If some function already has implementation
    /// it is ignored.
    pub fn load_functions(
        &mut self,
        funcs: impl IntoIterator<Item = CompiledFuncRef>,
        gen: &Gen,
        types: &Types,
        interner: &Interner,
    ) -> Result<(), JitRelocError> {
        let filtered_funcs = funcs
            .into_iter()
            .filter_map(|func| self.functions.contains_key(&func).not().then_some(func))
            .map(|func| {
                let compiled_func = gen.get_func_direct(func);
                let Func {
                    visibility, name, ..
                } = types[compiled_func.func()];

                if visibility == FuncVisibility::Imported {
                    let code = self
                        .runtime_lookup
                        .lookup(name.get(interner))
                        .or_else(|| self.exposed_funcs.get(name.get(interner)).cloned())
                        .ok_or(JitRelocError::MissingSymbol(GenItemName::Func(func)))?;
                    return Ok((
                        func,
                        InternalJittedFunc {
                            ptr: JittedFuncPtr::Static(code),
                            adapter: None,
                        },
                    ));
                }

                Ok((
                    func,
                    InternalJittedFunc {
                        ptr: JittedFuncPtr::Generated(
                            compiled_func
                                .bytecode
                                .ok_or(JitRelocError::MissingSymbol(GenItemName::Func(func)))?,
                        ),
                        adapter: None,
                    },
                ))
            })
            .collect::<Result<BumpVec<_>, _>>()?;

        for &(func, code) in filtered_funcs.iter() {
            self.functions.insert(func, code);
        }

        for (func, code) in filtered_funcs {
            let JittedFuncPtr::Generated(ref code) = code.ptr else {continue};
            let func_ent = gen.get_func_direct(func);
            Self::perform_jit_relocations(
                func,
                gen.code(code, true).try_instructions_mut().unwrap(),
                &func_ent.relocs,
                |name| match name {
                    GenItemName::Func(func) => self.get_function_ptr(func, gen),
                    GenItemName::LibCall(libcall) => {
                        RuntimeFunctionLookup::new().lookup(match libcall {
                            LibCall::Probestack => todo!(),
                            LibCall::CeilF32 => todo!(),
                            LibCall::CeilF64 => todo!(),
                            LibCall::FloorF32 => todo!(),
                            LibCall::FloorF64 => todo!(),
                            LibCall::TruncF32 => todo!(),
                            LibCall::TruncF64 => todo!(),
                            LibCall::NearestF32 => todo!(),
                            LibCall::NearestF64 => todo!(),
                            LibCall::FmaF32 => todo!(),
                            LibCall::FmaF64 => todo!(),
                            LibCall::Memcpy => "memcpy",
                            LibCall::Memset => todo!(),
                            LibCall::Memmove => "memmove",
                            LibCall::Memcmp => todo!(),
                            LibCall::ElfTlsGetAddr => todo!(),
                            LibCall::ElfTlsGetOffset => todo!(),
                        })
                    }
                },
                |_| todo!("get got entry"),
                |_| todo!("get plt entry"),
            )?;
        }

        Ok(())
    }

    pub fn perform_jit_relocations(
        func: CompiledFuncRef,
        region: &mut [u8],
        relocs: &[GenReloc],
        get_address: impl Fn(GenItemName) -> Option<*const u8>,
        get_got_entry: impl Fn(GenItemName) -> Option<*const u8>,
        get_plt_entry: impl Fn(GenItemName) -> Option<*const u8>,
    ) -> Result<(), JitRelocError> {
        use std::ptr::write_unaligned;

        for &GenReloc {
            kind,
            offset,
            name,
            addend,
        } in relocs
        {
            if (offset as usize) >= region.len() {
                return Err(JitRelocError::OffsetOutOfBounds);
            }

            let offset = isize::try_from(offset).map_err(|_| JitRelocError::OffsetOverflow)?;
            let at = unsafe { region.as_mut_ptr().offset(offset) };
            let args = (name, addend, at, func, region.as_mut_ptr());

            fn perform_reloc<T>(
                get_address: impl Fn(GenItemName) -> Option<*const u8>,
                calc_offset: impl Fn(*const u8, *const u8) -> Result<T, JitRelocError>,
                (name, addend, at, self_ref, base_ptr): (
                    GenItemName,
                    isize,
                    *mut u8,
                    CompiledFuncRef,
                    *mut u8,
                ),
            ) -> Result<(), JitRelocError> {
                let base = if GenItemName::Func(self_ref) == name {
                    base_ptr
                } else {
                    get_address(name).ok_or(JitRelocError::MissingSymbol(name))?
                };
                let what = unsafe { base.offset(addend) };
                let offset = calc_offset(what, at)?;

                #[allow(clippy::cast_ptr_alignment)]
                unsafe {
                    write_unaligned(at as *mut T, offset)
                };

                Ok(())
            }

            let relative_calc = |what, at| {
                ((what as isize) - (at as isize))
                    .try_into()
                    .map_err(|_| JitRelocError::OffsetOverflow)
            };

            match kind {
                Reloc::Abs4 => perform_reloc::<u32>(
                    &get_address,
                    |what, _| {
                        (what as usize)
                            .try_into()
                            .map_err(|_| JitRelocError::OffsetOverflow)
                    },
                    args,
                ),
                Reloc::Abs8 => perform_reloc::<u64>(
                    &get_address,
                    |what, _| Ok(what as u64), // cannot fail
                    args,
                ),
                Reloc::X86PCRel4 | Reloc::X86CallPCRel4 => {
                    perform_reloc::<i32>(&get_address, relative_calc, args)
                }
                Reloc::X86GOTPCRel4 => perform_reloc(&get_got_entry, relative_calc, args),
                Reloc::X86CallPLTRel4 => perform_reloc(&get_plt_entry, relative_calc, args),
                Reloc::S390xPCRel32Dbl | Reloc::S390xPLTRel32Dbl => perform_reloc::<i32>(
                    &get_address,
                    |what, at| {
                        (((what as isize) - (at as isize)) >> 1)
                            .try_into()
                            .map_err(|_| JitRelocError::OffsetOverflow)
                    },
                    args,
                ),
                Reloc::Arm64Call => {
                    let base = get_address(name).ok_or(JitRelocError::MissingSymbol(name))?;
                    // The instruction is 32 bits long.
                    let iptr = at as *mut u32;
                    // The offset encoded in the `bl` instruction is the
                    // number of bytes divided by 4.
                    let diff = ((base as isize) - (at as isize)) >> 2;
                    // Sign propagating right shift disposes of the
                    // included bits, so the result is expected to be
                    // either all sign bits or 0, depending on if the original
                    // value was negative or positive.
                    assert!((diff >> 26 == -1) || (diff >> 26 == 0));
                    // The lower 26 bits of the `bl` instruction form the
                    // immediate offset argument.
                    let chop = 32 - 26;
                    let imm26 = (diff as u32) << chop >> chop;
                    let ins = unsafe { iptr.read_unaligned() } | imm26;
                    unsafe {
                        iptr.write_unaligned(ins);
                    }

                    Ok(())
                }
                reloc => Err(JitRelocError::UnsupportedReloc(reloc)),
            }?;
        }

        Ok(())
    }
}

pub fn adapter_for<'a>(
    sig: &ir::Signature,
    ctx: &'a mut cranelift_codegen::Context,
    fctx: &mut FunctionBuilderContext,
    isa: &Isa,
) -> &'a CompiledCode {
    ctx.func.signature.clear(isa.default_call_conv());
    ctx.func
        .signature
        .params
        .extend([isa.pointer_type(); 3].map(ir::AbiParam::new));

    let mut builder = FunctionBuilder::new(&mut ctx.func, fctx);
    let entry = builder.create_block();
    builder.append_block_params_for_function_params(entry);
    builder.switch_to_block(entry);

    let &[func, args, ret] = builder.block_params(entry) else {
        unreachable!();
    };

    let loaded_args = sig.params.iter().enumerate().map(|(i, param)| {
        builder.ins().load(
            param.value_type,
            ir::MemFlags::trusted(),
            args,
            i as i32 * 8,
        )
    });

    let ret_arg = sig
        .params
        .first()
        .filter(|arg| arg.purpose == ArgumentPurpose::StructReturn)
        .map(|_| ret);

    let final_args = ret_arg
        .into_iter()
        .chain(loaded_args)
        .collect::<BumpVec<_>>();

    let sig = builder.import_signature(sig.clone());
    let call = builder.ins().call_indirect(sig, func, &final_args);

    for (i, ret_component) in builder
        .inst_results(call)
        .to_bumpvec()
        .into_iter()
        .enumerate()
    {
        builder
            .ins()
            .store(ir::MemFlags::trusted(), ret_component, ret, i as i32 * 8);
    }

    builder.ins().return_(&[]);
    builder.seal_block(entry);

    builder.finalize();

    ctx.compile(&**isa).unwrap();

    ctx.compiled_code().unwrap()
}

// macro_rules! gen_macro_structs {
//     (
//         $(
//             $backing:ident $name:ident $func:ident {
//                 $($fn_name:ident)+
//             }
//         )*
//     ) => {
//         $(
//             #[derive(Clone, Copy)]
//             pub struct $name {
//                 pub layout: Layout,
//                 pub name: Ident,
//                 $(pub $fn_name: CompiledFuncRef),+
//             }

//             impl $name {
//                 pub fn new(layout: Layout, name: Ident, mut fns: impl Iterator<Item = CompiledFuncRef>) -> Option<Self> {
//                     Some(Self {
//                         layout,
//                         name,
//                         $($fn_name: fns.next()?),+
//                     })
//                 }
//             }
//         )*

//         impl JitContext {
//             $(
//                 pub fn $func(&self, spec: &$name) -> Option<$backing> {
//                     unsafe {
//                         Some($backing {
//                             layout: spec.layout,
//                             $($fn_name: self.get_function(spec.$fn_name)?),+
//                         })
//                     }
//                 }
//             )+
//         }
//     };
// }

// gen_macro_structs! {
//     TokenMacroSpec TokenMacroOwnedSpec token_macro {
//         new next drop
//     }
// }

#[derive(Debug)]
pub enum JitRelocError {
    MissingSymbol(GenItemName),
    OffsetOverflow,
    OffsetOutOfBounds,
    UnsupportedReloc(Reloc),
    MissingBytecode(CompiledFuncRef),
}

pub struct RuntimeFunctionLookup {
    tmp_name: String,
    #[cfg(windows)]
    ucrtbase: std::os::windows::io::RawHandle,
}

impl RuntimeFunctionLookup {
    fn new() -> Self {
        Self {
            tmp_name: String::new(),
            #[cfg(windows)]
            ucrtbase: unsafe {
                windows_sys::Win32::System::LibraryLoader::GetModuleHandleA(
                    "ucrtbase.dll\0".as_ptr() as _,
                ) as _
            },
        }
    }

    #[cfg(not(windows))]
    fn lookup(&mut self, name: &str) -> Option<*const u8> {
        let c_str = self.load_cstr(name).as_ptr();
        let sym = unsafe { libc::dlsym(libc::RTLD_DEFAULT, c_str) };
        if sym.is_null() {
            None
        } else {
            Some(sym as *const u8)
        }
    }

    #[cfg(windows)]
    fn lookup(&mut self, name: &str) -> Option<*const u8> {
        use std::ptr;
        use windows_sys::Win32::Foundation::HINSTANCE;
        use windows_sys::Win32::System::LibraryLoader;

        let c_str = self.load_cstr(name).as_ptr().cast();

        unsafe {
            [ptr::null_mut(), self.ucrtbase]
                .into_iter()
                .find_map(|handle| LibraryLoader::GetProcAddress(handle as HINSTANCE, c_str))
                .map(|ptr| ptr as *const u8)
        }
    }

    fn load_cstr(&mut self, name: &str) -> &CStr {
        self.tmp_name.clear();
        self.tmp_name.push_str(name);
        self.tmp_name.push('\0');
        unsafe { CStr::from_bytes_with_nul_unchecked(self.tmp_name.as_bytes()) }
    }
}

impl Default for RuntimeFunctionLookup {
    fn default() -> Self {
        Self::new()
    }
}

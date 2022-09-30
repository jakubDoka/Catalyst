use std::alloc;
use std::ptr::{slice_from_raw_parts_mut, NonNull};

use cranelift_codegen::binemit::Reloc;
use storage::*;

use crate::{context::Isa, CompiledFunc, Gen, GenItemName, GenReloc};

pub struct JitContext {
    functions: ShadowMap<CompiledFunc, Option<JitFunction>>,
    resources: JitResources,
    pub isa: Isa,
}

impl JitContext {
    pub fn new(isa: Isa) -> Self {
        Self {
            functions: ShadowMap::new(),
            resources: JitResources::new(),
            isa,
        }
    }

    pub fn get_function(&self, func: VRef<CompiledFunc>) -> Option<&[u8]> {
        self.functions[func]
            // SAFETY: Lifetime of the returned slice is limited by the lifetime of the JitContext
            .map(|f| unsafe { f.code.as_ref() })
    }

    /// If function is already loaded this call does nothing.
    /// # Safety
    /// The caller must ensure signature of `func` matches the function pointer
    /// signature passed as `ptr`, `func` also has to have default call convention
    /// `ptr` should be `export "C"`.
    pub unsafe fn load_runtime_function(&mut self, func: VRef<CompiledFunc>, ptr: *const u8) {
        if self.functions[func].is_some() {
            return;
        }

        self.functions[func] = Some(JitFunction {
            code: NonNull::new_unchecked(slice_from_raw_parts_mut(ptr as *mut _, 0)),
        });
    }

    /// Used for loading batch of functions that are currently needed to execute.
    /// Functions can depend on each other. If some function already has implementation
    /// it is ignored.
    pub fn load_functions(
        &mut self,
        funcs: impl IntoIterator<Item = VRef<CompiledFunc>>,
        gen: &Gen,
        temp: bool,
    ) -> Result<(), JitRelocError> {
        let filtered_funcs = funcs
            .into_iter()
            .filter_map(|func| self.functions[func].is_none().then_some(func))
            .map(|func| {
                let func_ent = &gen.compiled_funcs[func];
                let body = &func_ent.bytecode;

                let layout = alloc::Layout::for_value(body.as_slice())
                    .align_to(func_ent.alignment as usize)
                    .unwrap();
                let mut code = if temp {
                    self.resources.temp_executable.alloc(layout)
                } else {
                    self.resources.executable.alloc(layout)
                };

                // SAFETY: `code` has the same layout and only we own it.
                unsafe {
                    code.as_mut()
                        .as_mut_ptr()
                        .copy_from_nonoverlapping(body.as_ptr(), body.len());
                }

                (func, code)
            })
            .collect::<BumpVec<_>>();

        for &(func, code) in filtered_funcs.iter() {
            self.functions[func] = Some(JitFunction { code });
        }

        for (func, code) in filtered_funcs {
            let func_ent = &gen.compiled_funcs[func];
            Self::perform_jit_relocations(
                // SAFETY: We just allocated the very code.
                unsafe { code.as_ref() },
                &func_ent.relocs,
                |name| match name {
                    GenItemName::Func(func) => self.get_function(func).map(|code| code.as_ptr()),
                },
                |_| todo!("get got entry"),
                |_| todo!("get plt entry"),
            )?;
        }

        Ok(())
    }

    pub fn perform_jit_relocations(
        region: &[u8],
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
            let at = unsafe { region.as_ptr().offset(offset) };
            let args = (name, addend, at);

            fn perform_reloc<T>(
                get_address: impl Fn(GenItemName) -> Option<*const u8>,
                calc_offset: impl Fn(*const u8, *const u8) -> Result<T, JitRelocError>,
                (name, addend, at): (GenItemName, isize, *const u8),
            ) -> Result<(), JitRelocError> {
                let base = get_address(name).ok_or(JitRelocError::MissingSymbol(name))?;
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

    /// Prepares for reuse.
    /// # Safety
    /// Caller must ensure no function pointers from
    /// jit context are used after this.
    pub unsafe fn clear(&mut self) {
        self.functions.clear();
        self.resources.clear();
    }

    pub fn prepare_for_execution(&mut self) {
        self.resources.seal();
    }

    /// # Safety
    /// Caller must ensure no function pointers from
    /// jit context are used after this. This applies to
    /// functions that are declared as temporary.
    pub unsafe fn clear_temp(&mut self) {
        self.resources.clear_temp();
    }
}

#[derive(Clone, Copy)]
struct JitFunction {
    code: NonNull<[u8]>,
}

struct JitResources {
    writable: ProtectedAllocator,
    readonly: ProtectedAllocator,
    executable: ProtectedAllocator,
    temp_executable: ProtectedAllocator,
}

impl JitResources {
    /// Prepares for reuse.
    /// # Safety
    /// Caller must ensure no function pointers from
    /// jit context are used after this.
    unsafe fn clear(&mut self) {
        self.writable.clear();
        self.readonly.clear();
        self.executable.clear();
        self.temp_executable.clear();
    }

    unsafe fn clear_temp(&mut self) {
        self.temp_executable.clear();
    }

    pub fn seal(&mut self) {
        self.writable.seal();
        self.readonly.seal();
        self.executable.seal();
        self.temp_executable.seal();
    }

    const WRITABLE_CHUNK_SIZE: usize = 1 << 12;
    const READONLY_CHUNK_SIZE: usize = 1 << 12;
    const EXECUTABLE_CHUNK_SIZE: usize = 1 << 14;

    fn new() -> Self {
        Self {
            writable: ProtectedAllocator::with_chunk_size(
                Self::WRITABLE_CHUNK_SIZE,
                region::Protection::READ_WRITE,
            ),
            readonly: ProtectedAllocator::with_chunk_size(
                Self::READONLY_CHUNK_SIZE,
                region::Protection::READ,
            ),
            executable: ProtectedAllocator::with_chunk_size(
                Self::EXECUTABLE_CHUNK_SIZE,
                region::Protection::READ_EXECUTE,
            ),
            temp_executable: ProtectedAllocator::with_chunk_size(
                Self::EXECUTABLE_CHUNK_SIZE >> 2,
                region::Protection::READ_EXECUTE,
            ),
        }
    }
}

impl Default for JitResources {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub enum JitRelocError {
    MissingSymbol(GenItemName),
    OffsetOverflow,
    OffsetOutOfBounds,
    UnsupportedReloc(Reloc),
}
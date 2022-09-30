use std::alloc;
use std::ptr::{slice_from_raw_parts_mut, NonNull};

use storage::*;

use crate::{perform_jit_relocations, CompiledFunc, Gen, GenItemName};

#[derive(Default)]
pub struct JitContext {
    functions: ShadowMap<CompiledFunc, Option<JitFunction>>,
    resources: JitResources,
}

impl JitContext {
    pub fn new() -> Self {
        Self::default()
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
    pub fn load_functions(&mut self, funcs: &[VRef<CompiledFunc>], gen: &Gen) {
        let filtered_funcs = funcs
            .iter()
            .filter_map(|&func| self.functions[func].is_none().then_some(func))
            .collect::<BumpVec<_>>();

        for &func in filtered_funcs.iter() {
            let func_ent = &gen.compiled_funcs[func];
            let body = &func_ent.bytecode;
            // SAFETY: Since layout is identical to Vectors allocation layout, this must be valid.
            let layout = alloc::Layout::array::<u8>(body.len())
                .unwrap()
                .align_to(func_ent.alignment as usize)
                .unwrap();
            let mut code = self.resources.executable.alloc(layout);

            // SAFETY: `code` has the same layout and only we own it.
            unsafe {
                code.as_mut()
                    .as_mut_ptr()
                    .copy_from_nonoverlapping(body.as_ptr(), body.len());
            }

            self.functions[func] = Some(JitFunction { code })
        }

        for func in filtered_funcs {
            let func_ent = &gen.compiled_funcs[func];
            let jit_func = self.functions[func].as_ref().unwrap();
            perform_jit_relocations(
                // SAFETY: We just allocated the very code.
                unsafe { jit_func.code.as_ref() },
                &func_ent.relocs,
                |name| match name {
                    GenItemName::Func(func) => self
                        .get_function(func)
                        .expect("This function should be loaded.")
                        .as_ptr(),
                },
                |_| todo!("get got entry"),
                |_| todo!("get ptr entry"),
            )
        }
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
}

#[derive(Clone, Copy)]
struct JitFunction {
    code: NonNull<[u8]>,
}

struct JitResources {
    writable: ProtectedAllocator,
    readonly: ProtectedAllocator,
    executable: ProtectedAllocator,
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
    }

    pub fn seal(&mut self) {
        self.writable.seal();
        self.readonly.seal();
        self.executable.seal();
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
        }
    }
}

impl Default for JitResources {
    fn default() -> Self {
        Self::new()
    }
}

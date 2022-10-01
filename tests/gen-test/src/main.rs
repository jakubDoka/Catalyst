#![feature(let_else)]
#![feature(fs_try_exists)]
#![feature(thread_id_value)]

use std::{fmt::Write, fs, iter, mem, path::Path, process::Command, vec};

use cranelift_codegen::{ir::InstBuilder, settings, Context};
use cranelift_frontend::FunctionBuilderContext;

use diags::*;
use gen::*;
use mir::*;
use mir_t::*;
use packaging::*;
use packaging_t::*;
use parsing::*;
use scope::*;
use storage::*;
use target_lexicon::Triple;
use testing::*;
use typec::*;
use typec_t::*;

struct LaterInit {
    object_context: ObjectContext,
    context: Context,
    func_ctx: FunctionBuilderContext,
    jit_context: JitContext,
}

#[derive(Default)]
struct TestState {
    interner: Interner,
    scope: Scope,
    typec: Typec,
    workspace: Workspace,
    packages: Packages,
    package_graph: PackageGraph,
    typec_ctx: TyCheckerCtx,
    mir_ctx: MirBuilderCtx,
    gen: Gen,
    gen_resources: GenResources,
    gen_layouts: GenLayouts,
    compile_requests: CompileRequests,
    functions: String,
    later_init: Option<LaterInit>,
    mir: Mir,
    entry_points: Vec<VRef<CompiledFunc>>,
}

impl TestState {
    fn get_init_later(&mut self) -> LaterInit {
        self.later_init.take().unwrap_or_else(|| {
            let object_isa = Isa::new(
                Triple::host(),
                settings::Flags::new(settings::builder()),
                false,
                &mut self.interner,
            )
            .unwrap();
            let jit_isa = Isa::new(
                Triple::host(),
                settings::Flags::new(settings::builder()),
                true,
                &mut self.interner,
            )
            .unwrap();
            LaterInit {
                object_context: ObjectContext::new(object_isa).unwrap(),
                context: Context::new(),
                func_ctx: FunctionBuilderContext::new(),
                jit_context: JitContext::new(jit_isa),
            }
        })
    }

    fn collect_entry_points(&mut self, triple: VRef<str>) -> Vec<VRef<CompiledFunc>> {
        let entry_points = self
            .mir_ctx
            .just_compiled
            .drain(..)
            .filter(|&func| self.typec.funcs[func].flags.contains(FuncFlags::ENTRY))
            .map(|func| {
                let id = self
                    .interner
                    .intern(ident!(triple, "&", self.typec.funcs.id(func)));
                self.gen
                    .compiled_funcs
                    .insert_unique(id, CompiledFunc::new(func))
            })
            .collect::<Vec<_>>();

        self.entry_points.extend(&entry_points);

        entry_points
    }

    fn compile_func(&mut self, current_func: VRef<CompiledFunc>, later_init: &mut LaterInit) {
        later_init
            .context
            .compile(&*later_init.object_context.isa)
            .unwrap();

        self.gen
            .save_compiled_code(current_func, &later_init.context)
            .unwrap();
    }

    fn generate_entry_point(&mut self, later_init: &mut LaterInit) {
        let default_cc = later_init.object_context.isa.default_call_conv();

        later_init.context.clear();
        later_init.context.func.signature.clear(default_cc);

        let dummy = FuncMir::default();
        let mut builder = GenBuilder::new(
            &later_init.jit_context.isa,
            &dummy,
            &mut later_init.context.func,
            &mut later_init.func_ctx,
        );

        let entry_point = builder.create_block();
        builder.append_block_params_for_function_params(entry_point);
        builder.switch_to_block(entry_point);

        for func in self.entry_points.drain(..) {
            let func_ref = generator!(self).import_compiled_func(func, &mut builder);
            builder.ins().call(func_ref, &[]);
            builder.ins().return_(&[]);
        }

        let id = self.interner.intern_str(gen::ENTRY_POINT_NAME);
        let func_id = self.typec.funcs.insert_unique(
            id,
            Func {
                visibility: FuncVisibility::Exported,
                ..Default::default()
            },
        );
        let entry_point = self
            .gen
            .compiled_funcs
            .insert_unique(id, CompiledFunc::new(func_id));

        self.compile_func(entry_point, later_init);

        later_init
            .object_context
            .load_functions(
                iter::once(entry_point),
                &self.gen,
                &self.typec,
                &self.interner,
            )
            .unwrap();
    }

    fn compute_func_constant(
        &mut self,
        target_func: VRef<CompiledFunc>,
        later_init: &mut LaterInit,
    ) {
        let target_func = self.gen.compiled_funcs[target_func].func;

        if self.mir.bodies[target_func]
            .as_ref()
            .map_or(false, |body| body.constants.is_empty())
        {
            return;
        }

        let body = self.mir.bodies[target_func]
            .take()
            .expect("Should be generated.");

        let mut compile_queue = vec![];

        let root_func = self
            .gen
            .compiled_funcs
            .get_or_insert(self.interner.intern_str("anon_const"), |_| {
                CompiledFunc::new(Func::ANON_TEMP)
            });

        let mut signature = Signature {
            cc: self.interner.intern_str("windows_fastcall").into(),
            ..Default::default()
        };

        for (key, const_mir) in body.constants.iter() {
            let mut builder = GenBuilder::new(
                &later_init.jit_context.isa,
                &body,
                &mut later_init.context.func,
                &mut later_init.func_ctx,
            );

            let root_block = const_mir.block;
            signature.ret = body.dependant_types[const_mir.ty].ty;
            generator!(self).generate(signature, root_block, &mut builder);

            write!(
                self.functions,
                "const {}\n\n",
                later_init.context.func.display()
            )
            .unwrap();

            self.compile_func(root_func, later_init);

            let next_iter = self
                .compile_requests
                .queue
                .drain(..)
                .map(|request| request.id);

            compile_queue.extend(next_iter);

            let mut compiled_funcs = vec![];

            while let Some(current_func) = compile_queue.pop() {
                compiled_funcs.push(current_func);

                let CompiledFunc { func, .. } = self.gen.compiled_funcs[current_func];
                let Func {
                    signature,
                    visibility,
                    ..
                } = self.typec.funcs[func];

                if visibility == FuncVisibility::Imported {
                    continue;
                }

                let fall_back = (func == target_func).then_some(&body);
                let body = self.mir.bodies[func]
                    .as_ref()
                    .or(fall_back)
                    .expect("should be generated");

                let root_block = body
                    .blocks
                    .keys()
                    .next()
                    .expect("function without blocks is invalid");

                let mut builder = GenBuilder::new(
                    &later_init.jit_context.isa,
                    body,
                    &mut later_init.context.func,
                    &mut later_init.func_ctx,
                );

                generator!(self).generate(signature, root_block, &mut builder);

                write!(self.functions, "{}\n\n", later_init.context.func.display()).unwrap();

                self.compile_func(current_func, later_init);

                let next_iter = self
                    .compile_requests
                    .queue
                    .drain(..)
                    .map(|request| request.id);

                compile_queue.extend(next_iter);
            }

            later_init
                .jit_context
                .load_functions(
                    compiled_funcs,
                    &self.gen,
                    &self.typec,
                    &self.interner,
                    false,
                )
                .unwrap();
            later_init
                .jit_context
                .load_functions(
                    iter::once(root_func),
                    &self.gen,
                    &self.typec,
                    &self.interner,
                    true,
                )
                .unwrap();

            later_init.jit_context.prepare_for_execution();

            let fn_ptr = later_init.jit_context.get_function(root_func).unwrap();
            let constant = if body.dependant_types[const_mir.ty].ty == Ty::UINT {
                let func: extern "C" fn() -> usize = unsafe { mem::transmute(fn_ptr.as_ptr()) };
                GenFuncConstant::Int(func() as u64)
            } else if body.dependant_types[const_mir.ty].ty == Ty::U32 {
                let func: extern "C" fn() -> u32 = unsafe { mem::transmute(fn_ptr.as_ptr()) };
                GenFuncConstant::Int(func() as u64)
            } else {
                todo!()
            };

            self.gen_resources.func_constants[key] = constant.into();
        }

        self.mir.bodies[target_func] = Some(body);
    }
}

impl Scheduler for TestState {
    fn resources(&mut self) -> packaging::PackageLoader {
        package_loader!(self)
    }

    fn init(&mut self, _: &Path) {
        self.typec.init_builtin_types(&mut self.interner);
        self.typec.init_builtin_funcs(&mut self.interner);
    }

    fn before_parsing(&mut self, module: storage::VRef<str>) {
        typec::build_scope(module, &mut self.scope, &self.packages, &self.typec);
    }

    fn parse_segment(&mut self, module: storage::VRef<str>, items: GroupedItemsAst) {
        let mut later_init = self.get_init_later();
        unsafe {
            later_init.jit_context.clear_temp();
        }

        let mut type_checked_funcs = vec![];
        ty_checker!(self, module).execute(items, &mut self.typec_ctx, &mut type_checked_funcs);
        mir_checker!(self, module).funcs(&mut self.mir_ctx, &mut type_checked_funcs);

        if self.workspace.has_errors() {
            return;
        }

        let mut compiled_funcs = vec![];
        let mut compile_queue = self.collect_entry_points(later_init.object_context.isa.triple);

        while let Some(current_func) = compile_queue.pop() {
            compiled_funcs.push(current_func);

            let CompiledFunc { func, .. } = self.gen.compiled_funcs[current_func];
            let Func {
                signature,
                visibility,
                ..
            } = self.typec.funcs[func];

            if visibility == FuncVisibility::Imported {
                continue;
            }

            self.compute_func_constant(current_func, &mut later_init);

            let body = self.mir.bodies[func].as_ref().expect("should be generated");

            let root_block = body
                .blocks
                .keys()
                .next()
                .expect("function without blocks is invalid");

            let mut builder = GenBuilder::new(
                &later_init.object_context.isa,
                body,
                &mut later_init.context.func,
                &mut later_init.func_ctx,
            );

            generator!(self).generate(signature, root_block, &mut builder);

            write!(self.functions, "{}\n\n", later_init.context.func.display()).unwrap();

            self.compile_func(current_func, &mut later_init);

            let next_iter = self
                .compile_requests
                .queue
                .drain(..)
                .map(|request| request.id);

            compile_queue.extend(next_iter);
        }

        later_init
            .object_context
            .load_functions(compiled_funcs, &self.gen, &self.typec, &self.interner)
            .unwrap();

        self.later_init = Some(later_init);
    }

    fn finally(&mut self) {
        self.workspace.push(snippet! {
            info: ("mir repr of functions:\n{}", self.functions);
        });

        if self.entry_points.is_empty() {
            self.workspace.push(snippet! {
                err: "no entry points found";
                help: "add '#[entry]' to a function that should take this role";
            });
        }

        if self.workspace.has_errors() {
            return;
        }

        let Some(mut later_init) = self.later_init.take() else {
            return;
        };

        self.generate_entry_point(&mut later_init);

        let thread_id = std::thread::current().id().as_u64().to_string();
        let exe_path = format!("o-{}.exe", thread_id);
        let obj_path = format!("o-{}.obj", thread_id);

        let emitted = later_init.object_context.emit().unwrap();
        fs::write(&obj_path, emitted).unwrap();

        let host = Triple::host().to_string();
        let target = later_init.object_context.isa.triple().to_string();

        let compiler = cc::Build::new()
            .opt_level(0)
            .target(&target)
            .host(&host)
            .cargo_metadata(false)
            .get_compiler();

        let args = if compiler.is_like_msvc() {
            vec![
                "ucrt.lib".into(),
                format!("-link /ENTRY:{} /SUBSYSTEM:CONSOLE", gen::ENTRY_POINT_NAME,),
            ]
        } else if compiler.is_like_clang() {
            todo!()
        } else if compiler.is_like_gnu() {
            todo!()
        } else {
            unimplemented!("unknown compiler");
        };

        compiler
            .to_command()
            .arg(&obj_path)
            .args(args)
            .status()
            .unwrap();

        let path = Path::new(&exe_path).canonicalize().unwrap();

        let output = Command::new(path)
            .current_dir(std::env::current_dir().unwrap())
            .status()
            .unwrap();

        fs::remove_file(obj_path).unwrap();
        fs::remove_file(exe_path).unwrap();

        assert_eq!(output.code(), Some(0), "{:x?}", output.code().unwrap());
    }
}

fn main() {
    gen_test! {
        TestState,
        false,
        simple "functions" {
            #[entry];
            fn main -> uint => pass(0);
            fn pass(a: uint) -> uint { return a };
            fn pass_with_implicit_return(a: uint) -> uint { a };
        }

        simple "recursion" {
            #[entry];
            fn main -> uint => 0;

            fn infinity(a: uint) => infinity(a);
        }

        simple "operators" {
            #[entry];
            fn main -> uint => 1 + 2 * 2 - 4 / 2 - 3;
        }

        simple "compile-time" {
            fn sub(a: uint, b: uint) -> uint => a - b;

            #[entry];
            fn main -> uint => const sub(1, 1);
        }

        simple "external" {
            fn "default" putchar(c: char) -> u32 extern;

            #[entry];
            fn main -> uint {
                const putchar('a');
                putchar('\n');
                0
            };
        }
    }
}

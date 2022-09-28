#![feature(let_else)]
#![feature(fs_try_exists)]

use std::{fmt::Write, fs, path::Path, process::Command};

use cranelift_codegen::{
    isa::{self, TargetIsa},
    settings::{self, Flags},
    Context,
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};

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
    isa: Box<dyn TargetIsa>,
    context: Context,
    func_ctx: FunctionBuilderContext,
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
}

impl Scheduler for TestState {
    fn resources(&mut self) -> packaging::PackageLoader {
        package_loader!(self)
    }

    fn init(&mut self, _: &Path) {
        self.typec.init_builtin_types(&mut self.interner);
    }

    fn before_parsing(&mut self, module: storage::VRef<str>) {
        typec::build_scope(module, &mut self.scope, &self.packages, &self.typec);
    }

    fn parse_segment(&mut self, module: storage::VRef<str>, items: GroupedItemsAst) {
        let mut later_init = self.later_init.take().unwrap_or_else(|| {
            let flag_builder = settings::builder();
            let builder = isa::lookup(Triple::host()).unwrap();
            let isa = builder.finish(Flags::new(flag_builder)).unwrap();
            let object_context = ObjectContext::new(&*isa).unwrap();
            LaterInit {
                isa,
                object_context,
                context: Context::new(),
                func_ctx: FunctionBuilderContext::new(),
            }
        });

        let mut type_checked_funcs: &[_] = &[];
        ty_checker!(self, module).execute(items, &mut self.typec_ctx, &mut type_checked_funcs);

        mir_checker!(self, module).funcs(&mut self.mir_ctx, type_checked_funcs);

        let main = self.interner.intern_str("main");
        let mut compile_queue = self
            .mir_ctx
            .just_compiled
            .drain(..)
            .find(|&func| self.typec.funcs[func].loc.name == main)
            .map(|func| {
                self.gen
                    .compiled_funcs
                    .insert_unique(main, CompiledFunc::new(func))
            })
            .into_iter()
            .collect::<Vec<_>>();

        let mut compiled_funcs = vec![];

        while let Some(current_func) = compile_queue.pop() {
            let CompiledFunc { func, .. } = self.gen.compiled_funcs[current_func];
            let body = self.mir.bodies[func].as_ref().expect("should be generated");

            generator!(self, later_init.isa.pointer_type()).generate(
                func,
                body,
                &mut FunctionBuilder::new(&mut later_init.context.func, &mut later_init.func_ctx),
            );

            write!(self.functions, "{}\n\n", later_init.context.func.display()).unwrap();

            later_init.context.compile(&*later_init.isa).unwrap();

            generator!(self, later_init.isa.pointer_type())
                .save_compiled_code(current_func, &later_init.context);
            compiled_funcs.push(current_func);

            let next_iter = self
                .compile_requests
                .queue
                .drain(..)
                .map(|request| request.id);
            compile_queue.extend(next_iter);
        }

        later_init.object_context.load_functions(
            &compiled_funcs,
            &self.gen,
            &self.typec,
            &self.interner,
        );

        self.later_init = Some(later_init);
    }

    fn finally(&mut self) {
        self.workspace.push(snippet! {
            info: ("mir repr of functions:\n{}", self.functions);
        });

        if self.workspace.has_errors() {
            return;
        }

        return;

        let Some(ref mut later_init) = self.later_init else {
            return;
        };

        let emitted = later_init.object_context.emit().unwrap();
        let path = Path::new("o.obj");
        fs::write(path, emitted).unwrap();

        let host = Triple::host().to_string();
        let target = later_init.isa.triple().to_string();

        let compiler = cc::Build::new()
            .opt_level(0)
            .target(&target)
            .host(&host)
            .get_compiler();

        let args = if compiler.is_like_msvc() {
            vec![format!("-link /ENTRY:{}", gen::ENTRY_POINT_NAME)]
        } else if compiler.is_like_clang() {
            todo!()
        } else if compiler.is_like_gnu() {
            todo!()
        } else {
            unimplemented!("unknown compiler");
        };

        compiler.to_command().arg(path).args(args).status().unwrap();

        let path = Path::new("o.exe").canonicalize().unwrap();

        let output = Command::new(path)
            .current_dir(std::env::current_dir().unwrap())
            .output()
            .unwrap();

        fs::remove_file("o.obj").unwrap();
        fs::remove_file("o.exe").unwrap();

        assert_eq!(output.status.code(), Some(0));
    }
}

fn main() {
    gen_test! {
        TestState,
        false,
        simple "functions" {
            #[entry]
            fn main -> uint => pass(0);
            fn pass(a: uint) -> uint { return a };
            fn pass_with_implicit_return(a: uint) -> uint { a };
        }

        simple "recursion" {
            #[entry]
            fn main -> uint => 0;

            fn infinity(a: uint) => infinity(a);
        }
    }
}

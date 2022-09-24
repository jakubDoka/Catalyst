use std::fmt::Write;

use cranelift_codegen::{ir::Function, isa, settings};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use target_lexicon::Triple;

use diags::*;
use gen::*;
use mir::*;
use mir_t::*;
use packaging::*;
use packaging_t::*;
use parsing::*;
use scope::*;
use storage::*;
use testing::*;
use typec::*;
use typec_t::*;

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
    gen_ctx: GeneratorCtx,
    functions: String,
}

impl Scheduler for TestState {
    fn resources(&mut self) -> packaging::PackageLoader {
        package_loader!(self)
    }

    fn init(&mut self) {
        self.typec.init_builtin_types(&mut self.interner);
    }

    fn before_parsing(&mut self, module: storage::VRef<str>) {
        typec::build_scope(module, &mut self.scope, &self.packages, &self.typec);
    }

    fn parse_segment(&mut self, module: storage::VRef<str>, items: GroupedItemsAst) {
        let mut type_checked_funcs: &[_] = &[];
        ty_checker!(self, module).execute(items, &mut self.typec_ctx, &mut type_checked_funcs);

        mir_checker!(self, module).funcs(&mut self.mir_ctx, type_checked_funcs);

        let shared_builder = settings::builder();
        let shared_flags = settings::Flags::new(shared_builder);

        let isa = isa::lookup(Triple::host())
            .unwrap()
            .finish(shared_flags)
            .unwrap();

        let mut builder_ctx = FunctionBuilderContext::new();
        let mut function = Function::new();

        for &(func, ref body) in &self.mir_ctx.mir_funcs {
            generator!(self, *isa, &mut self.gen_ctx).generate(
                func,
                body,
                &mut FunctionBuilder::new(&mut function, &mut builder_ctx),
            );

            write!(self.functions, "{}\n\n", function.display()).unwrap();
        }
    }

    fn finally(&mut self) {
        self.workspace.push(snippet! {
            info: ("mir repr of functions:\n{}", self.functions);
        });
    }
}

fn main() {
    gen_test! {
        TestState,
        true,
        simple "functions" {
            fn main -> uint => 0;
            fn pass(a: uint) -> uint { return a };
        }

        simple "recursion" {
            fn infinity(a: uint) => infinity(a);
        }
    }
}

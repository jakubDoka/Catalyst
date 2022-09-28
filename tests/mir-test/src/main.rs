use std::path::Path;

use diags::*;
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
    functions: String,
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
        let mut type_checked_funcs: &[_] = &[];
        ty_checker!(self, module).execute(items, &mut self.typec_ctx, &mut type_checked_funcs);

        mir_checker!(self, module)
            .funcs(&mut self.mir_ctx, type_checked_funcs)
            .display_funcs(&self.mir_ctx.just_compiled, &mut self.functions)
            .unwrap();

        self.mir_ctx.just_compiled.clear();
    }

    fn finally(&mut self) {
        self.workspace.push(snippet! {
            info: ("mir repr of functions:\n {}", self.functions);
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
    }
}

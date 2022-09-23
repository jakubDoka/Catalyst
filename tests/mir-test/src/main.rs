use diags::*;
use mir::*;
use mir_t::*;
use packaging::*;
use packaging_t::*;
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
    arena: Arena,
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

    fn parse_segment(&mut self, module: storage::VRef<str>, items: parsing::ItemsAst) {
        self.arena.clear();
        let mut structs = bumpvec![];
        let mut funcs = bumpvec![];
        let mut type_checked_funcs = bumpvec![];
        ty_checker!(self, module)
            .collect_structs(items, &mut structs)
            .collect_funcs(items, &mut funcs)
            .build_structs(&mut structs)
            .build_funcs(&self.arena, &mut funcs, &mut type_checked_funcs);

        let mut out = bumpvec![];
        let mut ctx = MirBuilderCtx::default();
        mir_checker!(self, module)
            .funcs(&mut ctx, &mut type_checked_funcs, &mut out)
            .display_funcs(&out, &mut self.functions)
            .unwrap();
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

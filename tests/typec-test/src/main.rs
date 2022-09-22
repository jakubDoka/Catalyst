#![feature(let_else)]

use std::path::Path;

use diags::*;
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
    arena: Arena,
    functions: String,
}

impl Scheduler for TestState {
    fn resources(&mut self) -> PackageLoader {
        package_loader!(self)
    }

    fn init(&mut self) {
        self.typec.init_builtin_types(&mut self.interner);
    }

    fn before_parsing(&mut self, module: VRef<str>) {
        typec::build_scope(module, &mut self.scope, &self.packages, &self.typec);
    }

    fn parse_segment(&mut self, module: VRef<str>, items: ItemsAst) {
        self.arena.clear();
        let mut structs = bumpvec![];
        let mut funcs = bumpvec![];
        let mut type_checked_funcs = bumpvec![];
        ty_checker!(self, module)
            .collect_structs(items, &mut structs)
            .collect_funcs(items, &mut funcs)
            .build_structs(&mut structs)
            .build_funcs(&self.arena, &mut funcs, &mut type_checked_funcs)
            .display_funcs(&type_checked_funcs, &mut self.functions)
            .unwrap();
    }

    fn finally(&mut self) {
        self.workspace.push(snippet! {
            info: ("tir repr of functions:\n {}", self.functions);
        });
    }
}

impl Testable for TestState {
    fn exec(mut self, name: &str) -> (Workspace, Packages) {
        self.execute(Path::new(name));

        (self.workspace, self.packages)
    }

    fn set_packages(&mut self, packages: Packages) {
        self.packages = packages;
    }
}

impl TestState {}

fn main() {
    gen_test! {
        TestState,
        true,
        simple "struct-decl" {
            struct A;
            struct B;
            struct C {
                a: A;
                b: B
            }
        }

        simple "function" {
            fn "default" main() -> uint {
                return 0
            };

            fn pass(a: uint) -> uint => a;
        }
    }
}

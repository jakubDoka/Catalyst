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
    typec_ctx: TyCheckerCtx,
    functions: String,
}

impl Scheduler for TestState {
    fn resources(&mut self) -> PackageLoader {
        package_loader!(self)
    }

    fn init(&mut self, _: &Path) {
        self.typec.init_builtin_types(&mut self.interner);
    }

    fn before_parsing(&mut self, module: VRef<str>) {
        typec::build_scope(module, &mut self.scope, &self.packages, &self.typec);
    }

    fn parse_segment(&mut self, module: VRef<str>, items: GroupedItemsAst) {
        let mut type_checked_funcs: &[_] = &[];
        ty_checker!(self, module)
            .execute(items, &mut self.typec_ctx, &mut type_checked_funcs)
            .display_funcs(type_checked_funcs, &mut self.functions)
            .unwrap();
    }

    fn finally(&mut self) {
        self.workspace.push(snippet! {
            info: ("tir repr of functions:\n {}", self.functions);
        });
    }
}

fn main() {
    gen_test! {
        TestState,
        true,
        simple "struct-decl" {
            struct A;
            struct C {
                a: A;
                b: B
            };
            struct B;
        }

        simple "struct-cycle" {
            struct A {
                b: B
            };

            struct B {
                a: A
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

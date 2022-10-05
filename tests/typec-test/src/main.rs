#![feature(let_else)]

use std::path::Path;

use diags::*;
use packaging::*;
use packaging_t::*;
use parsing::*;

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
    resources: Resources,
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

    fn before_parsing(&mut self, module: VRef<Module>) {
        typec::build_scope(module, &mut self.scope, &self.resources, &self.typec);
    }

    fn parse_segment(&mut self, module: VRef<Module>, items: GroupedItemsAst) {
        let mut type_checked_funcs = vec![];
        ty_checker!(self, module)
            .execute(items, &mut self.typec_ctx, &mut type_checked_funcs)
            .display_funcs(&type_checked_funcs, &mut self.functions)
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
        false,
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

        simple "spec" {
            priv spec Clay;

            priv struct [C: Clay] AnyClay {
                c: C
            }
        }

        simple "impl-block" {
            impl uint {
                fn pass(s: Self) -> uint => s;
            };

            fn main() -> uint => 0.pass();
        }

        "cross-module-item-access" {
            file "package.ctlm" {}
            dir "root" {
                file "a.ctl" {
                    impl uint {
                        fn pass(s: Self) -> uint => s;
                    };
                    fn [T] pass(v: T) -> T => v;
                    struct A;
                    impl A {
                        fn pass(s: uint) -> uint => s;
                    };
                }
                file "b.ctl" {
                    impl uint {
                        fn pass(s: Self) -> uint => s;
                        fn [T] pass_other(s: Self, p: T) -> T => p;
                    };
                    fn [T] pass(v: T) -> T => v;
                    struct [T] A;
                    impl [T] A[T] {
                        fn pass(s: T) -> T => s;
                    };
                }
            }
            file "root.ctl" {
                use {
                    "./a";
                    "./b"
                };

                fn main() -> uint => a::pass(uint::a::pass(a::A::a::pass(0).b::pass()));
                fn other_main() -> uint => a::pass::[uint](0.pass_other::[uint](0));
                fn third_main() -> uint =>  b::A::[uint]::b::pass(b::A::b::pass(0));
            }
        }
    }
}

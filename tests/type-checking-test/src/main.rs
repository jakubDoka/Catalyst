#![feature(let_else)]

use std::path::Path;

use diags::*;
use packaging::*;
use packaging_t::*;
use parsing::*;
use parsing_t::*;
use scope::*;
use storage::*;
use testing::*;
use type_checking::*;
use type_checking_t::*;

#[derive(Default)]
struct TestState {
    workspace: Workspace,
    packages: Packages,
    interner: Interner,
    package_graph: PackageGraph,
    scope: Scope,
    typec: Typec,
    item_context: ItemContext,
    ast_data: AstData,
    func_parser_ctx: FuncParserCtx,
}

impl TestState {
    fn run(name: &str) -> (Workspace, Packages) {
        let mut s = Self::default();
        drop(s.run_low(name));
        (s.workspace, s.packages)
    }

    fn run_low(&mut self, name: &str) -> errors::Result<()> {
        builtin_builder!(self).build();

        package_loader!(self).load(Path::new(name))?;

        let modules = self
            .packages
            .module_order
            .iter()
            .copied()
            .filter(|&m| {
                matches!(
                    self.packages.modules.get(&m).unwrap().kind,
                    ModKind::Module { .. }
                )
            })
            .collect::<Vec<_>>();

        let mut state = ParserState::new();
        for module in modules {
            for &ty in TyALL {
                self.scope.insert_builtin(self.typec.types.id(ty), ty);
            }

            for dep in &self.packages.conns[self.packages.modules.get(&module).unwrap().deps] {
                let ModKind::Module { ref items, .. } = self.packages.modules.get(&dep.ptr).unwrap().kind else {
                    unreachable!();
                };

                let dep_id = self
                    .interner
                    .intern_str(self.packages.span_str(module, dep.name));
                for item in items {
                    self.scope
                        .insert(module, item.to_scope_item(dep_id), &mut self.interner)
                        .unwrap();
                }
            }

            loop {
                let source = &self.packages.modules.get(&module).unwrap().content;
                state.start(source, module);

                let mut parser =
                    Parser::new(source, &mut state, &mut self.ast_data, &mut self.workspace);
                parser.skip_imports();
                let (ast, done) = parser.parse_items();

                if self.workspace.has_errors() {
                    break;
                }

                item_collector!(self, module).collect(ast, &mut self.item_context);
                ty_builder!(self, module).types(&mut self.item_context.types);
                func_parser!(self, module).funcs(self.item_context.funcs.drain(..));
                func_parser!(self, module).bound_impls(self.item_context.bound_impls.drain(..));

                if done {
                    break;
                }
            }

            self.scope.clear();
        }

        let iter = self.typec.defs.values().filter_map(|def| {
            let loc = def.loc;
            let span = def.loc.span(&self.interner);
            Some((loc.file.expand()?, span.expand()?, def))
        });

        let mut str = String::new();
        for (source, span, def) in iter {
            let name = self.packages.span_str(source, span);
            tir_display!(self, source, def.body, name, &def.tir_data)
                .display(&mut str)
                .unwrap()
        }

        if !str.is_empty() {
            self.workspace.push(diag! {
                (none) information => "functions:\n{}" { str }
            });
        }

        Ok(())
    }
}

fn main() {
    gen_test! {
        false
        simple "basic-structs" {
            struct pub A;
            struct priv B;
            struct C;
            struct D {
                pub mut a: A;
                priv b: B;
                use c: C
            }
        }
        simple "generic-structs" {
            struct pub [T] A;
            struct priv G {
                l: A[G]
            }
        }
        simple "complex-generic-struct" {
            struct A;
            struct B;
            struct [A, B] C {
                a: A;
                b: B
            };
            struct D {
                l: C[A, B]
            }
        }
        simple "pointers" {
            struct A;
            struct B;

            struct C {
                a: ^^^A;
                b: ^B
            }
        }
        simple "builtin-types" {
            struct C {
                a: int;
                b: i8;
                c: i16;
                d: i32;
                e: i64;
                f: uint;
                g: u8;
                h: u16;
                i: u32;
                j: u64
            }
        }
        "cross-module" {
            dir "root" {
                file "module.ctl" {
                    struct A;
                    struct B;
                }
            }
            file "root.ctl" {
                use { "./module" }

                struct C {
                    a: A;
                    b: B
                }
            }
            file "package.ctlm" {}
        }
        "cross-module-collision" {
            dir "root" {
                file "a.ctl" {
                    struct A;
                }
                file "b.ctl" {
                    struct A;
                }
            }
            file "root.ctl" {
                use { "./a"; "./b" }

                struct B {
                    a: A
                }
            }
            file "package.ctlm" {}
        }
        "cross-module-collision-fixed" {
            dir "root" {
                file "a.ctl" {
                    struct A;
                }
                file "b.ctl" {
                    struct A;
                }
            }
            file "root.ctl" {
                use { "./a"; f "./b" };

                struct B {
                    aa: a::A;
                    fa: f::A
                }
            }
            file "package.ctlm" {}
        }
        simple "functions" {
            fn main() {};
            fn priv foo(a: int, b: u32, c: bool) {};
            fn pub [T] bar(a: T, b: T) {};
            fn "default" malloc(size: int) -> ^u8 extern
        }
        simple "function-with-return" {
            fn main() -> int {
                return 0;
            }
        }
        simple "bound" {
            bound pub [T] Something {
                fn foo(a: T)
            };

            struct A;

            impl A {
                fn foo(a: A) {};
                fn goo(a: int) {};
            };

            impl Something[u8] for A {
                fn foo(a: u8) {}
            };

            impl Something[A] for A;

            impl Something[int] for A {
                use Self::goo as foo
            };
        }
        simple "bound-associated-types" {
            bound Something {
                type A;
                type B;

                fn foo(a: Self::A, b: Self::B);
            };

            struct F;

            impl Something for F {
                type A = u32;
                type B = u32;
                fn foo(a: Self::A, b: u32) {}
            }
        }
        // "binary-operators-and-precedence" {
        //     file "root.ctl" {
        //         fn add_square(a: u8, b: u8) -> u8 {
        //             return a + b * b
        //         };

        //         fn more_ops(c: u8) -> bool {
        //             return c + c - c == c
        //         }
        //     }
        //     file "package.ctlm" {}
        // }
        simple "function-calls" {
            struct A;

            impl pub A {
                fn a() {};
                fn b() {
                    Self::a()
                };
                fn c(i: i8) -> i8 {
                    return i
                };
                fn d(i: i8) -> i8 {
                    return Self::c(i)
                };
                fn e(i: i8, j: i8) {
                    Self::e(i, j)
                }
            };
        }
        simple "complex-bound-in-use" {
            bound [OPERAND] Add {
                type Out;
                fn add(a: Self, b: OPERAND) -> Self::Out
            };

            fn [A, B: Add[A]] add(a: A, b: B) -> B::Out {
                return B::add(b, a)
            }
        }
    }
}

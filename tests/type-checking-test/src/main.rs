#![feature(scoped_threads)]
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
    types: Types,
    funcs: Funcs,
    item_context: ItemContext,
    visibility: Visibility,
    ast_data: AstData,
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
            .filter(|&m| matches!(self.packages.modules[m].kind, ModKind::Module { .. }))
            .collect::<Vec<_>>();

        let mut state = ParserState::new();
        for module in modules {
            for &ty in BuiltinTypes::ALL {
                self.scope.insert_builtin(self.types.ents.id(ty), ty);
            }

            for dep in &self.packages.conns[self.packages.modules[module].deps] {
                let ModKind::Module { ref items, .. } = self.packages.modules[dep.ptr].kind else {
                    unreachable!();
                };

                for item in items {
                    self.scope
                        .insert(module, item.to_scope_item(dep.ptr), &mut self.interner)
                        .unwrap();
                }
            }

            loop {
                let source = &self.packages.modules[module].content;
                state.start(source, module);
                let mut parser =
                    Parser::new(source, &mut state, &mut self.ast_data, &mut self.workspace);
                drop(parser.skip_imports());
                let (ast, done) = parser.parse_code();

                if self.workspace.has_errors() {
                    break;
                }

                drop(item_collector!(self, module).collect(ast));
                drop(ty_builder!(self, module).types(&mut self.item_context.types));

                if done {
                    break;
                }
            }

            self.scope.clear();
        }

        //std::thread::sleep(std::time::Duration::from_secs(10));

        Ok(())
    }
}

fn main() {
    gen_test! {
        "basic-structs" {
            file "root.ctl" {
                struct pub A;
                struct priv B;
                struct C;
                struct D {
                    pub mut a: A;
                    priv b: B;
                    use c: C
                }
            }
            file "package.ctlm" {}
        }
        "generic-structs" {
            file "root.ctl" {
                struct pub [T] A;
                struct priv G {
                    l: A[G]
                }
            }
            file "package.ctlm" {}
        }
        "complex-generic-struct" {
            file "root.ctl" {
                struct A;
                struct B;
                struct [A, B] C {
                    a: A;
                    b: B
                }
                struct D {
                    l: C[A, B]
                }
            }
            file "package.ctlm" {}
        }
        "pointers" {
            file "root.ctl" {
                struct A;
                struct B;

                struct C {
                    a: ^^^A;
                    b: ^B
                }
            }
            file "package.ctlm" {}
        }
        "builtin-types" {
            file "root.ctl" {
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
            file "package.ctlm" {}
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
                use { "./a"; f "./b" }

                struct B {
                    aa: a.A
                    fa: f.A
                }
            }
            file "package.ctlm" {}
        }
    }
}

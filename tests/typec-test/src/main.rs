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
    ast_data: AstData,
    arena: Arena,
}

impl Testable for TestState {
    fn exec(mut self, name: &str) -> (Workspace, Packages) {
        package_loader!(self).load(Path::new(name));

        let mut parse_state = ParsingState::new();

        for module in self.packages.module_order.clone() {
            self.build_scope(module);

            let mod_ent = self.packages.modules.get(&module).unwrap();
            parse_state.start(&mod_ent.content, module);
            loop {
                self.ast_data.clear();
                let mod_ent = self.packages.modules.get(&module).unwrap();
                let items = {
                    let mut parser = ParsingCtx::new(
                        &mod_ent.content,
                        &mut parse_state,
                        &self.ast_data,
                        &mut self.workspace,
                        &mut self.interner,
                    );
                    ItemsAst::parse(&mut parser)
                };

                let Some(items) = items else {
                    break;
                };

                let finished = items.end.is_empty();

                {
                    self.arena.clear();
                    let mut structs = bumpvec![];
                    let mut funcs = bumpvec![];
                    let mut type_checked_funcs = bumpvec![];
                    ty_checker!(self, module)
                        .collect_structs(items, &mut structs)
                        .collect_funcs(items, &mut funcs)
                        .build_structs(&mut structs)
                        .build_funcs(&self.arena, &mut funcs, &mut type_checked_funcs);

                    dbg!(type_checked_funcs);
                }

                if finished {
                    break;
                }
            }
        }

        (self.workspace, self.packages)
    }

    fn set_packages(&mut self, packages: Packages) {
        self.packages = packages;
    }
}

impl TestState {
    fn build_scope(&mut self, module: VRef<str>) {
        self.scope.clear();

        self.typec.init_builtin_types(&mut self.interner);

        for &ty in Ty::ALL {
            let id = self.typec.types.id(ty);
            self.scope.insert_builtin(id, ty);
        }

        let mod_ent = self.packages.modules.get(&module).unwrap();
        for dep in &self.packages.conns[mod_ent.deps] {
            let mod_ent = self.packages.modules.get(&dep.ptr).unwrap();
            let ModKind::Module { ref items, .. } = mod_ent.kind else {
                unreachable!();
            };
            let r#mod = self.packages.ident_as_mod(dep.ptr).unwrap();
            let item = ModItem::new(dep.name, r#mod, dep.name_span, dep.name_span, Vis::Priv);
            self.scope
                .insert_current(item.to_scope_item(dep.ptr))
                .unwrap();
            for &item in items {
                self.scope
                    .insert(module, item.to_scope_item(dep.ptr))
                    .unwrap();
            }
        }
    }
}

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

            fn "default" pass(a: uint) -> uint => a
        }
    }
}

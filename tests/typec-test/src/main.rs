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
}

impl Testable for TestState {
    fn run(name: &str) -> (Workspace, Packages) {
        let mut ts = TestState::default();

        package_loader!(ts).load(Path::new(name));

        let mut parse_state = ParsingState::new();

        for module in ts.packages.module_order.to_vec() {
            ts.build_scope(module);

            let mod_ent = ts.packages.modules.get(&module).unwrap();
            parse_state.start(&mod_ent.content, module);
            loop {
                ts.ast_data.clear();
                let mod_ent = ts.packages.modules.get(&module).unwrap();
                let items = {
                    let mut parser = ParsingCtx::new(
                        &mod_ent.content,
                        &mut parse_state,
                        &ts.ast_data,
                        &mut ts.workspace,
                        &mut ts.interner,
                    );
                    ItemsAst::parse(&mut parser)
                };

                let Ok(items) = items else {
                    break;
                };

                let finished = items.end.len() == 0;

                let mut types = bumpvec![];
                let mut funcs = bumpvec![];
                item_collector!(ts, module).types(items, &mut types);
                item_collector!(ts, module).funcs(items, &mut funcs);
                ty_builder!(ts, module).types(&mut types);

                if finished {
                    break;
                }
            }
        }

        (ts.workspace, ts.packages)
    }
}

impl TestState {
    fn build_scope(&mut self, module: VRef<str>) {
        self.scope.clear();

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
    }
}

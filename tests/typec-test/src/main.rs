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
        let mut types = vec![];

        for module in ts.packages.module_order.to_vec() {
            ts.build_scope(module);

            let mod_ent = ts.packages.modules.get(&module).unwrap();
            parse_state.start(&mod_ent.content, module, false);
            loop {
                ts.ast_data.clear();
                let mod_ent = ts.packages.modules.get(&module).unwrap();
                let (items, finished) = ParsingCtx::new(
                    &mod_ent.content,
                    &mut parse_state,
                    &mut ts.ast_data,
                    &mut ts.workspace,
                )
                .parse_items();

                item_collector!(ts, module).types(items, &mut types);
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
    fn build_scope(&mut self, module: Ident) {
        self.scope.clear();

        let mod_ent = self.packages.modules.get(&module).unwrap();
        let iter = self.packages.conns[mod_ent.deps].iter().map(|dep| dep.ptr);
        for dep in iter {
            let mod_ent = self.packages.modules.get(&dep).unwrap();
            let ModKind::Module { ref items, .. } = mod_ent.kind else {
                unreachable!();
            };
            for &item in items {
                self.scope
                    .insert(module, item.to_scope_item(dep), &mut self.interner)
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

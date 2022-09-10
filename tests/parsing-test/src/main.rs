use std::path::Path;

use diags::*;
use packaging::*;
use packaging_t::*;
use parsing::*;
use parsing_t::*;
use storage::*;
use testing::*;

#[derive(Default)]
struct TestState {
    workspace: Workspace,
    packages: Packages,
    interner: Interner,
    package_graph: PackageGraph,
}

impl Testable for TestState {
    fn run(name: &str) -> (Workspace, Packages) {
        let mut ts = Self::default();

        package_loader!(ts).load(Path::new(name));

        let mut ast_data = AstData::new();
        let mut parse_state = ParserState::new();

        for module in ts.packages.module_order.to_vec() {
            let module_ent = ts.packages.modules.get(&module).unwrap();
            parse_state.start(&module_ent.content, module, false);
            let imports = Parser::new(
                &module_ent.content,
                &mut parse_state,
                &mut ast_data,
                &mut ts.workspace,
            )
            .parse_imports();
            if let Some(imports) = imports {
                let snippet = parsing::to_snippet(imports, &ast_data, module);
                ts.workspace.push(snippet);
            }

            loop {
                ast_data.clear();
                let (items, finished) = Parser::new(
                    &module_ent.content,
                    &mut parse_state,
                    &mut ast_data,
                    &mut ts.workspace,
                )
                .parse_items();

                for &item in &ast_data[items] {
                    let snippet = parsing::to_snippet(item, &ast_data, module);
                    ts.workspace.push(snippet);
                }

                if finished {
                    break;
                }
            }
        }

        (ts.workspace, ts.packages)
    }
}

fn main() {
    gen_test! {
        TestState,
        true,
        simple "struct-ast" {
            struct pub RichStruct {
                pub use mut field: Something[Very, Complex];
                other_field: SimpleStuff;
                priv pointer: ^mut^int;
            }
        }
    }
}

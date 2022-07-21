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

const DIR: &str = "test_project";

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
    fn run() -> (Workspace, Packages) {
        let mut s = Self::default();
        drop(s.run_low());
        (s.workspace, s.packages)
    }

    fn run_low(&mut self) -> errors::Result<()> {
        package_loader!(self).load(Path::new(DIR))?;

        let modules = self
            .packages
            .module_order
            .iter()
            .copied()
            .filter(|&m| matches!(self.packages.modules[m].kind, ModKind::Module { .. }))
            .collect::<Vec<_>>();

        let mut state = ParserState::new();
        for module in modules {
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
        }

        //std::thread::sleep(std::time::Duration::from_secs(10));

        Ok(())
    }
}

fn main() {
    test_case("basic-structs", || {
        quick_file_system!(
            (DIR)
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

        );

        TestState::run()
    });

    test_case("generic-structs", || {
        quick_file_system!(
            (DIR)
            file "root.ctl" {
                struct pub [T] A;
                struct priv G {
                    l: A[G]
                }
            }
            file "package.ctlm" {}
        );

        TestState::run()
    });
}

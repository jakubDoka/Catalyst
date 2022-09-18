#![feature(default_free_fn)]

use std::{default::default, path::Path};

use diags::*;
use lexing_t::*;
use packaging::*;
use packaging_t::*;
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
    fn exec(mut self, name: &str) -> (Workspace, Packages) {
        package_loader!(self).load(Path::new(name));

        for &module in &self.packages.module_order {
            let module_ent = self
                .packages
                .modules
                .get(&module)
                .expect("module should exist since it is in module order");

            self.workspace.push(Snippet {
                slices: vec![Slice {
                    span: Span::new(0..module_ent.content.len()),
                    origin: module,
                    ..default()
                }
                .into()],
                ..default()
            })
        }

        (self.workspace, self.packages)
    }

    fn set_packages(&mut self, packages: Packages) {
        self.packages = packages;
    }
}

fn main() {
    gen_test! {
        TestState,
        false,
        simple "struct-ast" r"
            // comment
            pub /* comment */ struct /* comment */ RichStruct /* comment */ { // comment
                // comment
                pub /* comment */ use /* comment */ mut /* comment */ field: Something[
                    Very, // uuu
                    Complex,
                ] // comment  
                // comment
                other_field: SimpleStuff[Hell, /* gear */]; priv pointer: ^mut^int
                // comment
            } // comment
        "
        simple "one-line-struct" {
            pub struct OneLineStruct {
                pub use mut filed: Trough;
                other_field: SimpleStuff;
                priv pointer: ^mut^int;
            }
        }
    }
}

use std::path::Path;

use packaging::*;
use testing::*;
use packaging_t::*;
use diags::*;
use storage::*;

const DIR: &str = "test_project";

#[derive(Default)]
struct TestState {
    workspace: Workspace,
    packages: Packages,
    interner: Interner,
    package_graph: PackageGraph,
}

fn main() {
    test_case("github", || {
        quick_file_system!(
            (DIR)
            file "root.ctl" { use { "water"; "a"; "b" } }
            file "package.ctlm" {
                deps {
                    water git "github.com/jakubDoka/water" "v1.*.*";
                    a "a";
                    b "b"
                }
            }
            dir "a" { 
                file "foo.ctl" { use { b "b" } }
                file "package.ctlm" {
                    root: "root.ctl";
                    deps { b "../b" }
                }
            }
            dir "b" {
                file "bar.ctl" { use { a "a" } }
                file "package.ctlm" {
                    root: "bar.ctl";
                    deps {
                        water git "github.com/jakubDoka/water" "v1.*.*"
                    }
                }
            }
        );

        let mut ts = TestState::default();

        drop(package_loader!(ts).load(Path::new(DIR)));

        (ts.workspace, ts.packages)
    })
}

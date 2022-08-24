use std::path::Path;

use diags::*;
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

impl TestState {
    fn run(name: &str) -> (Workspace, Packages) {
        let mut ts = TestState::default();
        drop(package_loader!(ts).load(Path::new(name)));
        (ts.workspace, ts.packages)
    }
}

fn main() {
    gen_test! {
        true
        "github" {
            file "root.ctl" { use { "water"; "a"; "b" } }
            file "package.ctlm" {
                deps {
                    water git "github.com/jakubDoka/water" "v0.*.*";
                    a "a";
                    b "b"
                }
            }
            dir "a" {
                file "foo.ctl" { use { b "b" } }
                file "package.ctlm" {
                    root: "foo.ctl";
                    deps { b "../b" }
                }
            }
            dir "b" {
                file "bar.ctl" {}
                file "package.ctlm" {
                    root: "bar.ctl";
                    deps {
                        water git "github.com/jakubDoka/water" "v0.*.*"
                    }
                }
            }
        }
        "self-import" {
            file "root.ctl" { use { "." } }
            file "package.ctlm" {}
        }
        "cycle" {
            file "root.ctl" { use { "./a" } }
            file "package.ctlm" {}
            dir "root" {
                file "a.ctl" { use { "./a/b" } }
                dir "a" {
                    file "b.ctl" { use { "." } }
                }
            }
        }
        "invalid-module" {
            file "root.ctl" { use { "./a" } }
            file "package.ctlm" {}
            dir "root" {
                file "a.ctl" { use { "./a/b" } }
            }
        }
        "invalid-version" {
            file "root.ctl" {}
            file "package.ctlm" {
                deps {
                    water git "github.com/jakubDoka/water" "ajdakjshdkajshdka"
                }
            }
        }
        "invalid-link" {
            file "root.ctl" {}
            file "package.ctlm" {
                deps {
                    water git "github.com/jakubDoka/water-kun"
                }
            }
        }
    };
}

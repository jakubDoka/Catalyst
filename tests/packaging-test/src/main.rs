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

impl Testable for TestState {
    fn exec(mut self, name: &str) -> (Workspace, Packages) {
        package_loader!(self).load(Path::new(name));
        (self.workspace, self.packages)
    }

    fn set_packages(&mut self, packages: Packages) {
        self.packages = packages;
    }
}

fn main() {
    gen_test! {
        TestState,
        true,
        "github" {
            file "root.ctl" { use { "water"; "a"; "b" } }
            file "package.ctlm" {
                deps {
                    git water "github.com/jakubDoka/water" "v0.*.*";
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
                        git "github.com/jakubDoka/water" "v0.*.*"
                    }
                }
            }
            remote_dir "github.com/jakubDoka/water#v0.1.0" {
                file "water.ctl" {}
                file "package.ctlm" {
                    root: "water.ctl";
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
                    git "github.com/jakubDoka/water" "non existent version"
                }
            }
            remote_dir "github.com/jakubDoka/water#main" {
                file "water.ctl" {}
                file "package.ctlm" {
                    root: "water.ctl";
                }
            }
        }
        "invalid-link" {
            file "root.ctl" {}
            file "package.ctlm" {
                deps {
                    git "github.com/jakubDoka/water-kun"
                }
            }
        }
    };
}

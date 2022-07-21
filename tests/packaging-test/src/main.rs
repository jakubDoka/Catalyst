use std::path::Path;

use diags::*;
use packaging::*;
use packaging_t::*;
use storage::*;
use testing::*;

const DIR: &str = "test_project";

#[derive(Default)]
struct TestState {
    workspace: Workspace,
    packages: Packages,
    interner: Interner,
    package_graph: PackageGraph,
}

fn main() {
    macro_rules! run {
        () => {{
            let mut ts = TestState::default();
            drop(package_loader!(ts).load(Path::new(DIR)));
            (ts.workspace, ts.packages)
        }};
    }

    test_case("github", || {
        quick_file_system!(
            (DIR)
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
        );

        run!()
    });

    test_case("self-import", || {
        quick_file_system!(
            (DIR)
            file "root.ctl" { use { "." } }
            file "package.ctlm" {}
        );

        run!()
    });

    test_case("cycle", || {
        quick_file_system!(
            (DIR)
            file "root.ctl" { use { "./a" } }
            file "package.ctlm" {}
            dir "root" {
                file "a.ctl" { use { "./a/b" } }
                dir "a" {
                    file "b.ctl" { use { "." } }
                }
            }
        );

        run!()
    });

    test_case("invalid-module", || {
        quick_file_system!(
            (DIR)
            file "root.ctl" { use { "./a" } }
            file "package.ctlm" {}
            dir "root" {
                file "a.ctl" { use { "./a/b" } }
            }
        );

        run!()
    });

    test_case("invalid-version", || {
        quick_file_system!(
            (DIR)
            file "root.ctl" {}
            file "package.ctlm" {
                deps {
                    water git "github.com/jakubDoka/water" "ajdakjshdkajshdka"
                }
            }
        );

        run!()
    });

    test_case("invalid-link", || {
        quick_file_system!(
            (DIR)
            file "root.ctl" {}
            file "package.ctlm" {
                deps {
                    water git "github.com/jakubDoka/water-kun"
                }
            }
        );

        run!()
    });
}

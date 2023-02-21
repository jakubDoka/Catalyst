#![feature(default_free_fn)]

use std::{default::default, path::Path};

use diags::*;
use packaging::*;
use resources::*;
use storage::*;
use testing::{items::TestResources, *};

#[derive(Default)]
struct TestState {
    workspace: Workspace,
    resources: Resources,
    interner: Interner,
    package_graph: PackageGraph,
}

impl Testable for TestState {
    fn exec(mut self, name: &str, db: &mut TestResources) -> (Workspace, Resources) {
        let mut ctx = default();
        PackageLoader {
            resources: &mut self.resources,
            workspace: &mut self.workspace,
            interner: &mut self.interner,
            package_graph: &mut self.package_graph,
            db,
        }
        .reload(Path::new(name), &mut ctx);
        (self.workspace, self.resources)
    }
}

fn main() {
    gen_test! {
        TestState,
        false,
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
                        git "github.com/jakubDoka/water" "v0.*.*";
                    }
                }
            }
            remote_dir "github.com/jakubDoka/water#v0.1.0" {
                file "water.ctl" {}
                file "package.ctlm" {
                    moist: true;
                    root: "water.ctl";
                }
            }
        }
        "self-import" {
            file "root.ctl" { use { "." } }
            file "package.ctlm" {
                deps { git "github.com/jakubDoka/water" }
            }
            remote_dir "github.com/jakubDoka/water#main" {
                file "water.ctl" {}
                file "package.ctlm" {
                    moist: true;
                    root: "water.ctl";
                }
            }
        }
        "cycle" {
            file "root.ctl" { use { "./a" } }
            file "package.ctlm" {
                deps { git "github.com/jakubDoka/water" }
            }
            dir "root" {
                file "a.ctl" { use { "./a/b" } }
                dir "a" {
                    file "b.ctl" { use { "." } }
                }
            }
            remote_dir "github.com/jakubDoka/water#main" {
                file "water.ctl" {}
                file "package.ctlm" {
                    moist: true;
                    root: "water.ctl";
                }
            }
        }
        "invalid-module" {
            file "root.ctl" { use { "./a" } }
            file "package.ctlm" {
                deps { git "github.com/jakubDoka/water" }
            }
            dir "root" {
                file "a.ctl" { use { "./a/b" } }
            }
            remote_dir "github.com/jakubDoka/water#main" {
                file "water.ctl" {}
                file "package.ctlm" {
                    moist: true;
                    root: "water.ctl";
                }
            }
        }
        "invalid-version" {
            file "root.ctl" {}
            file "package.ctlm" {
                deps {
                    git "github.com/jakubDoka/water";
                    git "github.com/jakubDoka/water" "non existent version";
                }
            }
            remote_dir "github.com/jakubDoka/water#main" {
                file "water.ctl" {}
                file "package.ctlm" {
                    moist: true;
                    root: "water.ctl";
                }
            }
        }
        "invalid-link" {
            file "root.ctl" {}
            file "package.ctlm" {
                deps {
                    git "github.com/jakubDoka/water-kun";
                    git "github.com/jakubDoka/water";
                }
            }
            remote_dir "github.com/jakubDoka/water#main" {
                file "water.ctl" {}
                file "package.ctlm" {
                    moist: true;
                    root: "water.ctl";
                }
            }
        }
        "package-cycle" {
            file "root.ctl" {}
            file "package.ctlm" {
                deps {
                    "a";
                    git "github.com/jakubDoka/water";
                }
            }
            dir "a" {
                file "root.ctl" {}
                file "package.ctlm" {
                    deps {
                        "..";
                    }
                }
            }
            remote_dir "github.com/jakubDoka/water#main" {
                file "water.ctl" {}
                file "package.ctlm" {
                    moist: true;
                    root: "water.ctl";
                }
            }
        }
    };
}

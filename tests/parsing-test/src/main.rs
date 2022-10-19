#![feature(default_free_fn)]

use std::default::default;

use diags::*;
use lexing_t::*;
use packaging::*;
use packaging_t::*;
use storage::*;
use testing::*;

#[derive(Default)]
struct TestState {
    workspace: Workspace,
    resources: Resources,
    interner: Interner,
    package_graph: PackageGraph,
}

impl Scheduler for TestState {
    fn loader(&mut self) -> PackageLoader {
        package_loader!(self)
    }

    fn before_parsing(&mut self, module: VRef<Module>) {
        let module_ent = &self.resources.modules[module];
        let content = &self.resources.sources[module_ent.source].content;

        self.workspace.push(Snippet {
            slices: vec![Slice {
                span: Span::new(0..content.len()),
                origin: module_ent.source,
                annotations: default(),
                fold: false,
            }
            .into()],
            title: default(),
            footer: default(),
            origin: default(),
        });
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

        simple "binary-operators" {
            fn do_something(a: uint, b: uint) -> uint {
                a + b * a + b * a + b * a + b * a + b * a + b *
                a + b * a + b * a + b * a + b * a + b * a + b + a
            }
        }

        simple "spec" {
            pub spec [T] Some {
                fn new() -> Self;
                fn get(s: ^Self) -> T;
                fn [D] set(s: ^mut Self, v: D, t: T);
            }
        }

        simple "impl-block" {
            struct Struct {
                field: u32;
            };

            impl Struct {
                fn constructor() -> Self =>
                    Self::{ field: 0 };

                fn method(s: ^Self) -> uint =>
                    self.field;
            };
        }

        simple "match" {
            struct B {
                a: uint;
                b: uint
            };

            fn main -> uint => match B::{ a: 0; b: 3 } {
                ::{ a: 6, b: 7 } => 0;
                ::{ a: 0, b: 3 } => 1;
                ::{ a: 0, b } => b;
                ::{ a, b } => a + b;
            }
        }

        simple "if-statement" {
            #[entry];
            fn main() -> uint =>
                if 0 == 0 => 0;
                elif 0 == 69 => 89;
                elif 0 == 69 { 89 };
                elif 0 == 69 == 0 == 0 == 0 == 0 ==
                    0 == 0 == 0 == 0 == 0 == 0 == 0 ==
                    0 == 0 == 0 == 0 == 0 == 0 == 0 ==
                    0 == 0 == 0 == 0 == 0 == 0 == 0 ==
                    0 == 0 == 0 == 0 == 0 == 0 == 0 ==
                    0 == 0 == 0 == 0 == 0 == 0 == 0 ==
                    0 == 0 == 0 == 0 == 0 == 0 == 0 == 0 => 89;
                elif 0 == 69 => 89;
                elif 0 == 69 => 89;
                else => 1;
        }

        simple "let-binding" {
            struct A {
                a: uint;
                b: uint;
            };

            #[entry];
            fn main() -> uint {
                let ::{ mut a, b } = A::{ a: 0; b: 3 };
                a = a + b;
                a - 3
            };
        }
    }
}

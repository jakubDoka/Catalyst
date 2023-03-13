#![feature(default_free_fn)]

use core::slice;
use std::default::default;

use ast::ManifestAst;
use diags::*;
use lexing::NoTokenMeta;
use parsing::Parser;
use resources::*;
use storage::*;
use testing::{
    fmt::{AstHandler, MiddlewareArgs},
    *,
};

#[derive(Default)]
struct TestState;

#[derive(Default)]
struct Output(Vec<VRef<Source>>);

impl AstHandler for Output {
    type Meta = NoTokenMeta;

    type Imports<'a> = ();

    type Chunk<'a> = ();

    fn parse_chunk<'a>(&mut self, _parser: Parser<'_, 'a, Self::Meta>) -> Option<Self::Chunk<'a>> {
        None
    }

    fn chunk(
        &mut self,
        _items: Self::Chunk<'_>,
        _ctx: fmt::BaseSourceCtx,
        _macros: fmt::MacroSourceCtx,
    ) -> bool {
        true
    }

    fn parse_imports<'a>(
        &mut self,
        mut parser: Parser<'_, 'a, Self::Meta>,
    ) -> Option<Self::Imports<'a>> {
        parser.skip_imports()
    }

    fn parse_manifest<'a>(
        &mut self,
        _parser: Parser<'_, 'a, Self::Meta>,
    ) -> Option<ManifestAst<'a, Self::Meta>> {
        None
    }

    fn should_skip_module(&mut self, ctx: fmt::BaseSourceCtx) -> bool {
        if !ctx.shared.resources.is_external(ctx.module) {
            self.0.push(ctx.shared.resources.modules[ctx.module].source);
        }

        true
    }
}

impl Testable for TestState {
    fn exec<'a>(
        &'a mut self,
        name: &str,
        middleware: &'a mut fmt::Middleware,
        resources: &'a mut items::TestResources,
    ) -> (&'a mut Workspace, &'a Resources) {
        let mut output = Output::default();
        let args = MiddlewareArgs {
            path: name.into(),
            ..default()
        };
        middleware.traverse_source_ast(&args, slice::from_mut(&mut output));

        let view = middleware.unwrap_view();
        let mut out = String::new();
        for source in output.0 {
            let source = &view.resources.sources[source].path;
            let file = resources.read_to_string(source).unwrap();
            out.push_str(&file);
        }
        view.workspace.push(MirRepr { repr: out });
        (view.workspace, view.resources)
    }
}

ctl_errors! {
    #[info => "formatted:\n {repr}"]
    error MirRepr {
        repr ref: String,
    }
}

fn main() {
    gen_test! {
        TestState,
        true,
        simple "struct-ast" r"
            // comment
            pub /* comment */ struct /* comment */ RichStruct /* comment */ { // comment
                // comment
                pub /* comment */ use /* comment */ mut /* comment */ field: Something\[
                    Very, // uuu
                    Complex,
                ] // comment
                // comment
                other_field: SimpleStuff\[Hell, /* gear */]; priv pointer: ^mut^int
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

            fn main -> uint => match B::{ a: 0, b: 3 } {
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
                let ::{ mut a, b } = A::{ a: 0, b: 3 };
                a = a + b;
                a - 3
            };
        }

        simple "attributes" {
            #[macro swap];
            enum Swap {
                Two: TwoTokens;
                Last: LastToken;
                Empty;
            };
        }

        simple "long-const-compute" {
            fn expensive(num: uint) -> uint => {
                let mut i = 0;
                loop if i == num => break else {
                    i = i + 1;
                };
                i
            };
            break;
            const C = expensive(1000) * 100;
            const D = expensive(100000);
            #[entry];
            fn main -> uint => C * 100 - D;
        }
    }
}

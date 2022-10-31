#![feature(let_else)]

use std::path::Path;

use diags::*;
use packaging::*;
use packaging_t::*;
use parsing::*;

use storage::*;
use testing::*;
use typec::*;
use typec_t::*;

#[derive(Default)]
struct TestState {
    interner: Interner,
    scope: Scope,
    typec: Typec,
    workspace: Workspace,
    resources: Resources,
    package_graph: PackageGraph,
    typec_ctx: TyCheckerCtx,
    ast_transfer: AstTransfer<'static>,
    functions: String,
}

impl Scheduler for TestState {
    fn loader(&mut self) -> PackageLoader {
        package_loader!(self)
    }

    fn init(&mut self, _: &Path) {
        self.typec.init(&mut self.interner);
    }

    fn before_parsing(&mut self, module: VRef<Module>) {
        typec::build_scope(
            module,
            &mut self.scope,
            &self.resources,
            &self.typec,
            &mut self.interner,
        );
    }

    fn parse_segment(&mut self, module: VRef<Module>, items: GroupedItemsAst) {
        let mut type_checked_funcs = bumpvec![];
        let arena = Arena::new();
        let mut ctx = TirBuilderCtx::default();
        ty_checker!(self, module)
            .execute(
                &arena,
                items,
                &mut self.typec_ctx,
                &mut ctx,
                self.ast_transfer.activate(),
                &mut type_checked_funcs,
            )
            .display_funcs(&type_checked_funcs, &mut self.functions)
            .unwrap();
    }

    fn finally(&mut self) {
        self.workspace.push(snippet! {
            info: ("tir repr of functions:\n {}", self.functions);
        });
    }
}

fn main() {
    gen_test! {
        TestState,
        false,
        simple "struct-decl" {
            struct A;
            struct C {
                a: A;
                b: B
            };
            struct B;
        }

        simple "struct-cycle" {
            struct A {
                b: B
            };

            struct B {
                a: A
            }
        }

        simple "function" {
            fn "default" main() -> uint {
                return 0
            };

            fn pass(a: uint) -> uint => a;
        }

        simple "spec" {
            priv spec Clay;

            priv struct [C: Clay] AnyClay {
                c: C
            }
        }

        simple "impl-block" {
            impl uint {
                fn pass(s: Self) -> uint => s;
            };

            fn main() -> uint => 0.pass();
        }

        "cross-module-item-access" {
            file "package.ctlm" {}
            dir "root" {
                file "a.ctl" {
                    impl uint {
                        fn pass(s: Self) -> uint => s;
                    };
                    fn [T] pass(v: T) -> T => v;
                    struct A;
                    impl A {
                        fn pass(s: uint) -> uint => s;
                    };
                }
                file "b.ctl" {
                    impl uint {
                        fn pass(s: Self) -> uint => s;
                        fn [T] pass_other(s: Self, p: T) -> T => p;
                    };
                    fn [T] pass(v: T) -> T => v;
                    struct [T] A;
                    impl [T] A[T] {
                        fn pass(s: T) -> T => s;
                    };
                }
            }
            file "root.ctl" {
                use {
                    "./a";
                    "./b"
                };

                fn main() -> uint => a::pass(uint::a::pass(a::A::a::pass(0).b::pass()));
                fn other_main() -> uint => a::pass::[uint](0.pass_other::[uint](0));
                fn third_main() -> uint => b::A::[uint]::b::pass(b::A::b::pass(0));
            }
        }

        simple "spec-call" {
            priv spec Clay;

            impl Clay for uint;

            struct [T] Foo {
                a: T
            };

            impl [T: Clay] Clay for Foo[T];

            fn [T: Clay] pass(v: T) -> T => v;

            fn main() -> uint => pass(0);
            fn other_main() -> u32 => pass(0);
            fn last_main() -> Foo[uint] => pass(::{ a: 0 });
        }

        simple "spec-with-funcs" {
            struct [T] Glued {
                inner: T
            };

            spec Glue {
                fn new -> Self;
                fn [T] use_on(s: ^Self, value: T) -> Glued[T];
            };

            impl Glue for uint {
                fn new -> Self => 0;
                fn [T] use_on(s: ^Self, value: T) -> Glued[T] => ::{ inner: value };
            };

            impl Glue for uint;

            impl Glue for u32;

            struct A;

            impl Glue for A {
                fn new -> uint => 0;
                fn [T] use_on(s: ^uint) -> Glued[T] => ::{ inner: value };
            };

            impl uint {
                fn new {}
            };

            fn [G: Glue, T] glue_up(value: T) -> Glued[T] {
                G::new().use_on(value)
            };

            spec [T] GenericSpec {
                fn take(t: T) -> Self;
            };

            impl GenericSpec[uint] for uint {
                fn take(t: uint) -> Self => t;
            };

            fn [B, T: GenericSpec[B]] take(t: B) -> T => T::take(t);

            fn main() -> uint => uint::new() + take(0);
        }

        simple "compile-time" {
            fn sub(a: uint, b: uint) -> uint => a - b;

            #[entry];
            fn main -> uint => const sub(1, 1);
        }

        simple "match" {
            struct Matched {
                a: uint;
                b: uint
            };

            fn main() -> uint => match Matched::{ a: 0; b: 1 } {
                ::{ a: 0; b: 1 } => 0;
                ::{ a: 1; b: 0 } => 1;
                ::{ a; b: 0 } => a;
                ::{ a; b } => a + b;
            };
        }

        simple "enum" {
            enum [T] Option {
                Some: T;
                None;
            };

            fn main() -> uint => match Option::Some~0 {
                ::Some~4 => 5;
                ::Some~1 => 2;
                ::Some~a => a;
                ::None => 3;
            }
        }

        simple "if-statement" {
            #[entry];
            fn main() -> uint =>
                if 0 == 0 => 0;
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

        simple "macro-impl" {
            use {
                "water/option";
                "water/macros/tokens";
            };
            // use {
            //     w "water"
            // };

            // TODO: Solution for macro name collisions
            // type WSwap = w::Swap[uint];
            // break;

            struct LastToken {
                last: MacroToken;
            };

            struct TwoTokens {
                second: MacroToken;
                first: MacroToken;
            };

            #[macro swap];
            enum Swap {
                Two: TwoTokens;
                Last: LastToken;
                Empty;
            };

            impl TokenMacro for Swap {
                fn new(s: ^Self, lexer: MacroLexer) {
                    *s = ::Two~::{
                        first: lexer.next();
                        second: lexer.next();
                    };
                };

                fn next(s: ^Self, lexer: MacroLexer) -> Option[MacroToken] =>
                    ::Some~match *s {
                        ::Two~::{ first, second } {
                            *s = ::Last~::{ last: first };
                            second
                        };
                        ::Last~::{ last } {
                            *s = ::Empty;
                            last
                        };
                        ::Empty => return ::None;
                    };

                fn drop(s: ^Self) {};
            };
        }
    }
}

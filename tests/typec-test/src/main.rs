use std::{mem, path::Path};

use diags::*;
use packaging::*;
use parsing::*;
use resources::*;

use storage::*;
use testing::*;
use type_creator::type_creator;
use typec::*;
use types::*;

#[derive(Default)]
struct TestState {
    interner: Interner,
    types: Types,
    workspace: Workspace,
    resources: Resources,
    package_graph: PackageGraph,
    typec_ctx: TypecCtx,
    typec_transfere: TypecTransfere<'static>,
    arena: Arena,
    functions: String,
    builtin_funcs: Vec<FragRef<Func>>,
}

impl Scheduler for TestState {
    fn init(&mut self, _: &Path) {
        type_creator!(self).init(&mut self.builtin_funcs);
    }

    fn before_parsing(&mut self, module: storage::VRef<Module>) {
        self.typec_ctx.build_scope(
            module,
            &self.resources,
            &self.types,
            &mut self.interner,
            &self.builtin_funcs,
        );
    }

    fn parse_segment(&mut self, module: storage::VRef<Module>, items: GroupedItemsAst) {
        let mut active = Active::take(&mut self.typec_transfere);
        let mut ext = TypecExternalCtx {
            types: &mut self.types,
            interner: &mut self.interner,
            workspace: &mut self.workspace,
            resources: &self.resources,
            transfere: &mut active,
            folder: &mut ConstFolderImpl {},
        };
        let meta = TypecMeta::new(&self.resources, module);

        TypecParser::new(&self.arena, &mut self.typec_ctx, ext.clone_borrow(), meta).execute(items);
        if !self.resources.is_external(module) {
            let funcs = ext.transfere.checked_funcs().to_bumpvec();
            ext.display_funcs(&funcs, &mut self.functions).unwrap();
        }

        self.typec_transfere = active.erase();
        self.arena.clear();
        self.typec_ctx.cast_checks().for_each(drop);
    }

    fn finally(&mut self) {
        self.workspace.push(TirRepr {
            repr: mem::take(&mut self.functions),
        });
    }

    fn loader<'a>(&'a mut self, db: &'a mut dyn ResourceDb) -> PackageLoader<'a> {
        PackageLoader {
            resources: &mut self.resources,
            workspace: &mut self.workspace,
            interner: &mut self.interner,
            package_graph: &mut self.package_graph,
            db,
        }
    }
}

struct ConstFolderImpl {}
impl ConstFolder for ConstFolderImpl {
    fn fold(&mut self, _: Ty, _: TirNode, _: ConstFolderContext) -> FolderValue {
        unimplemented!()
    }
}

ctl_errors! {
    #[info => "tir repr of functions:\n{repr}"]
    error TirRepr {
        repr ref: String,
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
            file "package.ctlm" {
                deps { git "github.com/jakubDoka/water" }
            }
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
                    impl [T] A::[T] {
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

        "access-violation" {
            file "package.ctlm" {
                deps { git "github.com/jakubDoka/water" }
            }
            dir "root" {
                file "a.ctl" {
                    priv fn pass(v: uint) -> uint => v;

                    struct A {
                        priv a: uint;
                    };

                    impl A {
                        fn new() -> A => ::{ a: 0 };
                    };
                }
            }
            file "root.ctl" {
                use {
                    "./a";
                };

                fn main() -> uint => pass(0);
                fn other_main() -> uint => A::new().a;
            }
        }

        simple "spec-call" {
            priv spec Clay;

            impl Clay for uint;

            struct [T] Foo {
                a: T
            };

            impl [T: Clay] Clay for Foo::[T];

            fn [T: Clay] pass(v: T) -> T => v;

            fn main() -> uint => pass(0);
            fn other_main() -> u32 => pass(0);
            fn last_main() -> Foo::[uint] => pass(::{ a: 0 });
        }

        simple "spec-with-funcs" {
            struct [T] Glued {
                inner: T
            };

            spec Glue {
                fn new -> Self;
                fn [T] use_on(s: ^Self, value: T) -> Glued::[T];
            };

            impl Glue for uint {
                fn new -> Self => 0;
                fn [T] use_on(s: ^Self, value: T) -> Glued::[T] => ::{ inner: value };
            };

            impl Glue for uint;

            impl Glue for u32;

            struct A;

            impl Glue for A {
                fn new -> uint => 0;
                fn [T] use_on(s: ^uint) -> Glued::[T] => ::{ inner: value };
            };

            impl uint {
                fn new {}
            };

            fn [G: Glue, T] glue_up(value: T) -> Glued::[T] {
                G::new().use_on(value)
            };

            spec [T] GenericSpec {
                fn take(t: T) -> Self;
            };

            impl GenericSpec::[uint] for uint {
                fn take(t: uint) -> Self => t;
            };

            fn [B, T: GenericSpec::[B]] take(t: B) -> T => T::take(t);

            fn main() -> uint => uint::new() + take(0);
        }

        simple "match" {
            struct Matched {
                a: uint;
                b: uint
            };

            fn main() -> uint => match Matched::{ a: 0, b: 1 } {
                ::{ a: 0, b: 1 } => 0;
                ::{ a: 1, b: 0 } => 1;
                ::{ a, b: 0 } => a;
                ::{ a, b } => a + b;
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
                let ::{ mut a, b } = A::{ a: 0, b: 3 };
                a = a + b;
                a - 3
            };
        }

        // simple "macro-impl" {
        //     use {
        //         "water/option";
        //         "water/ptr";
        //         "water/macros/tokens";
        //     };
        //     // use {
        //     //     w "water"
        //     // };

        //     // TODO: Solution for macro name collisions
        //     // type WSwap = w::Swap[uint];
        //     // break;

        //     struct LastToken {
        //         last: MacroToken;
        //     };

        //     struct TwoTokens {
        //         second: MacroToken;
        //         first: MacroToken;
        //     };

        //     enum SwapState {
        //         Two: TwoTokens;
        //         Last: LastToken;
        //         Empty;
        //     };

        //     #[macro swap];
        //     struct Swap {
        //         state: SwapState;
        //         lexer: MacroLexer;
        //     };

        //     impl TokenMacro for Swap {
        //         fn "default" new(s: ^Self, lexer: MacroLexer) {
        //             ptr::write(s, ::{
        //                 state: ::Two~::{
        //                     first: lexer.next();
        //                     second: lexer.next();
        //                 };
        //                 lexer;
        //             });
        //         };

        //         fn "default" next(s: ^Self) -> Option::[MacroToken] =>
        //             ::Some~match s.state {
        //                 ::Two~::{ first, second } {
        //                     s.state = ::Last~::{ last: first };
        //                     second
        //                 };
        //                 ::Last~::{ last } {
        //                     s.state = ::Empty;
        //                     last
        //                 };
        //                 ::Empty => return ::None;
        //             };

        //         fn "default" drop(s: ^Self) -> MacroLexer {
        //             ptr::read(^s.lexer)
        //         };
        //     };
        // }

        simple "vec" {
            use {
                "water/vec";
            };

            #[entry];
            fn main -> uint {
                let mut vec = vec::Vec::[uint]::new();
                vec.push(0);
                *vec.get_ptr(0);
            }
        }

        simple "fake-return" {
            struct A;

            fn drop_unused() {
                A::{};
            };
        }

        simple "simple-spec-inheritance" {
            spec A {
                fn a -> uint;
            };

            spec B {
                fn a -> uint;
            };

            spec C: A + B;

            impl C for uint;
            impl A for uint {
                fn a -> uint => 0;
            };
            impl uint {
                fn a -> uint => 1;
            };
            impl B for uint;

            fn [T: C] sum() -> uint => T::A::a() + T::B::a();

            fn main() -> uint => {
                sum::[uint]() - uint::A::a() - uint::B::a()
            };
        }

        simple "loop-outsider" {
            use { "builtin" };
            struct W;
            impl Drop for W {fn drop(_: ^mut Self) {}};
            impl W {fn do_something(_: ^mut Self) {}};

            #[entry];
            fn main {
                let mut window = W::{};
                loop if true => break else {;
                    window.do_something()
                }
            }
        }
    }
}

use std::path::Path;

use diags::*;
use mir::*;
use mir_t::*;
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
    mir_ctx: MirCtx,
    arena: Arena,
    mir_move_ctx: MirMoveCtx,
    functions: String,
    mir: Mir,
}

impl Scheduler for TestState {
    fn loader(&mut self) -> packaging::PackageLoader {
        package_loader!(self)
    }

    fn init(&mut self, _: &Path) {
        self.typec.init(&mut self.interner);
    }

    fn before_parsing(&mut self, module: storage::VRef<Module>) {
        typec::build_scope(
            module,
            &mut self.scope,
            &self.resources,
            &self.typec,
            &mut self.interner,
        );
    }

    fn parse_segment(&mut self, module: storage::VRef<Module>, items: GroupedItemsAst) {
        let mut type_checked_funcs = bumpvec![];
        let mut ctx = TirBuilderCtx::default();
        ty_checker!(self, module).execute(
            &self.arena,
            items,
            &mut self.typec_ctx,
            &mut ctx,
            self.ast_transfer.activate(),
            &mut type_checked_funcs,
        );

        mir_checker!(self, module)
            .funcs(&mut type_checked_funcs)
            .display_funcs(&mut self.functions)
            .unwrap();

        self.mir_ctx.just_compiled.clear();
    }

    fn finally(&mut self) {
        self.workspace.push(snippet! {
            info: ("mir repr of functions:\n {}", self.functions);
        });
    }
}

fn main() {
    gen_test! {
        TestState,
        false,
        simple "functions" {
            fn main -> uint => 0;
            fn pass(a: uint) -> uint { return a };
        }

        simple "auto-ref-deref" {
            impl uint {
                fn reference(s: ^^Self) -> ^^Self => s;
                fn dereference(s: Self) -> Self => s;
            };

            fn main -> uint => 0.reference().dereference();
        }

        // simple "compile-time" {
        //     fn sub(a: uint, b: uint) -> uint => a - b;

        //     #[entry];
        //     fn main -> uint => const sub(1, 1);
        // }

        simple "match" {
            struct Matched {
                a: uint;
                b: uint
            };

            fn main() -> uint => match Matched::{ a: 0; b: 1 } {
                ::{ a: 1; b: 0 } => 1;
                ::{ a: 0; b: 1 } => 0;
                ::{ a; b: 0 } => a;
                ::{ a; b } => a + b;
            };
        }

        simple "match-with-missing-patterns" {
            struct Matched {
                a: uint;
                b: uint
            };

            fn main() -> uint => match Matched::{ a: 0; b: 1 } {
                ::{ a: 1; b: 0 } => 1;
                ::{ a: 0; b: 1 } => 0;
                ::{ a; b: 0 } => a;
            };
        }

        simple "match-with-struct-return" {
            struct Returned {
                a: uint;
                b: uint
            };

            #[entry];
            fn main() -> uint => match 0 {
                0 => Returned::{ a: 0; b: 1 };
                a => Returned::{ a: a; b: 0 };
            }.a;
        }

        simple "enum" {
            enum [T] Option {
                Some: T;
                None;
            };

            fn main() -> uint => match Option::Some~Option::Some~0 {
                ::Some~::None => 5;
                ::Some~::Some~1 => 2;
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
                "water/marker";
                "water/ptr";
            };

            // TODO: Solution for macro name collisions
            // use {
            //     w "water"
            // };
            //
            // #[macro w_swap]
            // type WSwap = w::Swap[uint];
            // break;

            struct LastToken {
                last: MacroToken;
            };

            impl Copy for LastToken;

            struct TwoTokens {
                second: MacroToken;
                first: MacroToken;
            };

            impl Copy for TwoTokens;

            enum SwapState {
                Two: TwoTokens;
                Last: LastToken;
                Empty;
            };

            impl Copy for SwapState;

            #[macro swap];
            struct Swap {
                state: SwapState;
                lexer: MacroLexer;
            };

            impl TokenMacro for Swap {
                fn new(s: ^Self, lexer: MacroLexer) {
                    ptr::write(s, ::{
                        state: ::Two~::{
                            first: lexer.next();
                            second: lexer.next();
                        };
                        lexer;
                    });
                };

                fn next(s: ^Self) -> Option[MacroToken] =>
                    ::Some~match s.state {
                        ::Two~::{ first, second } {
                            s.state = ::Last~::{ last: first };
                            second
                        };
                        ::Last~::{ last } {
                            s.state = ::Empty;
                            last
                        };
                        ::Empty => return ::None;
                    };

                fn drop(s: ^Self) -> MacroLexer {
                    ptr::read(^s.lexer)
                };
            };
        }

        simple "simple-moves" {
            struct A;

            fn [T] drop(t: T) {};

            fn double_move() {
                let a = A::{};
                drop(a);
                drop(a);
            };

            fn conditional_double_move() {
                let a = A::{};
                if 0 == 0 {
                    drop(a);
                };
                drop(a);
            };

            fn conditional_move() {
                let a = A::{};
                if 0 == 0 {
                    drop(a);
                    return;
                };
                drop(a);
            };

            fn reassign() {
                let mut a = A::{};
                drop(a);
                a = A::{};
                drop(a);
            };

            fn conditional_reassign() {
                let mut a = A::{};
                drop(a);
                if 0 == 0 => a = A::{};
                else => a = A::{};
                drop(a);
            };

            fn conditional_incomplete_reassign() {
                let mut a = A::{};
                drop(a);
                if 0 == 0 => a = A::{};
                drop(a);
            };

            enum E {
                A: A;
                B;
            };

            fn enum_move() {
                let e = E::A~::{};
                match e {
                    ::A~a => drop(a);
                    ::B {};
                };
                drop(e);
            };

            fn enum_just_match() {
                let e = E::A~::{};
                match e {
                    ::A~_ {};
                    ::B {};
                };
                drop(e);
            };

            fn enum_whole_move() {
                let e = E::A~::{};
                match e {
                    ::A~_ {};
                    e {};
                };
                drop(e);
            }
        }

        simple "partial-moves" {
            struct A;
            struct B {
                a0: A;
                a1: A;
            };
            impl B {
                fn new() -> Self => ::{ a0: ::{}; a1: ::{} };
            };

            fn [T] drop(t: T) {};

            fn double_move() {
                let b = B::new();
                drop(b.a0);
                drop(b.a1);
                drop(b.a0);
            };

            fn move_of_partially_moved() {
                let b = B::new();
                drop(b.a0);
                drop(b);
            };

            fn partial_move_and_assign() {
                let b = B::new();
                drop(b.a0);
                b.a0 = A::{};
                drop(b);
            };

            fn conditional_partial_move() {
                let b = B::new();
                if 0 == 0 => drop(b.a0);
                drop(b);
            };

            fn conditional_assign() {
                let b = B::new();
                drop(b.a0);
                if 0 == 0 => b.a0 = A::{};
                else => return;
                drop(b);
            };

            fn reference_partial_move() {
                let b = B::new();
                drop(b.a0);
                let a = ^b;
            };

            fn partial_reference_move() {
                let b = B::new();
                drop(b);
                let a = ^b.a0;
            };
        }

        simple "drop-gen" {
            use {
                "water/marker";
            };

            struct A;

            impl A {
                fn f(s: ^Self) {};
            };

            impl Drop for A {
                fn drop(v: ^mut Self) {};
            };

            fn drop_unused() {
                A::{};
            };

            fn drop_referenced() {
                A::{}.f();
            };

            fn drop_variable() {
                let a = A::{};
                a.f();
            };

            fn drop_refed_variable() {
                let a = ^A::{};
                a.f();
            };
        }
    }
}

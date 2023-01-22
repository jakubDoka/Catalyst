use std::{mem, path::Path};

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
    builtin_funcs: Vec<FragRef<Func>>,
}

impl Scheduler for TestState {
    fn init(&mut self, _: &Path) {
        self.typec.init(&mut self.interner, &mut self.builtin_funcs);
    }

    fn before_parsing(&mut self, module: storage::VRef<Module>) {
        typec::build_scope(
            module,
            &mut self.scope,
            &self.resources,
            &self.typec,
            &mut self.interner,
            &self.builtin_funcs,
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

        let mir_module = self.mir.modules.next();
        let mut checker = mir_checker!(self, module);
        checker.funcs(mir_module, &mut type_checked_funcs);
        if !self.resources.is_external(module) {
            checker.display_funcs(&mut self.functions).unwrap();
        }

        self.mir_ctx.module.clear();
        self.mir_ctx.just_compiled.clear();
    }

    fn finally(&mut self) {
        self.workspace.push(MirRepr {
            repr: mem::take(&mut self.functions),
        });
    }

    fn loader<'a>(&'a mut self, resources: &'a mut dyn ResourceDb) -> PackageLoader<'a> {
        package_loader!(self, *resources)
    }
}

ctl_errors! {
    #[info => "mir repr of functions:\n{repr}"]
    error MirRepr {
        repr ref: String
    }
}

fn main() {
    gen_test! {
        TestState,
        true,
        simple "functions" {
            fn main -> uint => 0;
            fn pass(a: uint) -> uint { return a };
        }

        simple "auto-ref-deref" {
            impl uint {
                fn reference(s: ^^Self) -> ^^Self => s;
                fn dereference(s: Self) -> Self => s;
            };

            fn main -> uint {
                let zero = 0;
                zero.reference().dereference();
            };
        }

        simple "additional-param-garbage" {
            fn [T] pass(value: T) -> T => value;

            struct B;

            #[entry];
            fn main -> uint => pass::[uint, B](0uint, 'h');
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

            fn main() -> uint => match Matched::{ a: 0, b: 1 } {
                ::{ a: 1, b: 0 } => 1;
                ::{ a: 0, b: 1 } => 0;
                ::{ a, b: 0 } => a;
                ::{ a, b } => a + b;
            };
        }

        simple "match-with-missing-patterns" {
            struct Matched {
                a: uint;
                b: uint
            };

            fn main() -> uint => match Matched::{ a: 0, b: 1 } {
                ::{ a: 1, b: 0 } => 1;
                ::{ a: 0, b: 1 } => 0;
                ::{ a, b: 0 } => a;
            };
        }

        simple "match-with-struct-return" {
            struct Returned {
                a: uint;
                b: uint
            };

            #[entry];
            fn main() -> uint => match 0 {
                0 => Returned::{ a: 0, b: 1 };
                a => Returned::{ a: a, b: 0 };
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
                let ::{ mut a, b } = A::{ a: 0, b: 3 };
                a = a + b;
                a - 3
            };
        }

        // simple "macro-impl" {
        //     use {
        //         "water/option";
        //         "water/macros/tokens";
        //         "water/marker";
        //         "water/ptr";
        //     };

        //     // TODO: Solution for macro name collisions
        //     // use {
        //     //     w "water"
        //     // };
        //     //
        //     // #[macro w_swap]
        //     // type WSwap = w::Swap[uint];
        //     // break;

        //     struct LastToken {
        //         last: MacroToken;
        //     };

        //     impl Copy for LastToken;

        //     struct TwoTokens {
        //         second: MacroToken;
        //         first: MacroToken;
        //     };

        //     impl Copy for TwoTokens;

        //     enum SwapState {
        //         Two: TwoTokens;
        //         Last: LastToken;
        //         Empty;
        //     };

        //     impl Copy for SwapState;

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
                mut a0: A;
                mut a1: A;
            };
            impl B {
                fn new() -> Self => ::{ a0: ::{}, a1: ::{} };
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
                let mut b = B::new();
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
                let mut b = B::new();
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
                "builtin";
            };

            fn [T] drop(value: T) {};

            fn "default" putchar(c: char) -> u32 extern;

            struct A {
                mut ch: char;
            };

            impl A {
                fn new(ch: char) -> Self => ::{ ch };
                fn set_char(s: ^mut Self, ch: char) => s.ch = ch;
            };

            impl Drop for A {
                fn drop(v: ^mut Self) {
                    putchar(v.ch);
                    putchar(' ');
                };
            };

            fn drop_unused() {
                A::new('a');
            };

            fn drop_referenced() {
                A::new('a').set_char('b');
            };

            fn drop_variable() {
                let mut a = A::new('a');
                a.set_char('c');
            };

            fn drop_refed_variable() {
                let a = ^mut A::new('a');
                a.set_char('d');
            };

            fn move_in_drop() {
                let mut a = A::new('e');
                a = A::new('f');
            };

            fn drop_cond() {
                let a = A::new('g');
                if true => drop(a);
            };

            #[entry];
            fn main() -> uint {
                drop_unused();
                drop_referenced();
                drop_variable();
                drop_refed_variable();
                move_in_drop();
                drop_cond();
                0
            };
        }

        simple "loop-drops" {
            fn [T] drop(t: T) {};

            struct A;

            fn loop_drop() {
                let a = A::{};
                loop {
                    drop(a);
                };
            };

            fn loop_break_drop() {
                let a = A::{};
                loop {
                    drop(a);
                    break;
                };
            };

            fn linear_loop_branch_drop() {
                let a = A::{};
                loop {
                    if true => drop(a);
                    else => continue;
                    break;
                };
                drop(a);
            };

            fn loop_branch_drop() {
                let a = A::{};
                loop {
                    if true => drop(a);
                    else => break;
                };
            };

            fn loop_drop_and_move_in() {
                let mut a = A::{};
                loop {
                    if true => drop(a);
                    a = A::{};
                };

                drop(a);
            };
        }

        simple "register struct init and use" {
            struct RegStruct {
                field: u32;
            };

            struct RegStruct2 {
                field: u32;
                field2: u32;
            };

            #[entry];
            fn main -> u32 {
                RegStruct::{ field: 3 }.field +
                RegStruct2::{ field: 1, field2: 3 }.field2 -
                6u32
            }
        }

        simple "vec-test" {
            use {
                "water/vec";
            };

            #[entry];
            fn main() -> uint {
                let mut v = Vec::[uint]::new();
                v.push(0);
                v.push(1);
                v.push(2);

                let mut vv = Vec::[Vec::[uint]]::new();
                vv.push(v);
                vv.get_mut_ptr(0).push(3);

                0
            }
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

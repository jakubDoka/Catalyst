use std::{mem, path::Path};

use diags::*;
use borrowc::*;
use mir_t::*;
use packaging::*;
use resources::*;
use parsing::*;

use storage::*;
use testing::*;
use types::*;
use typec::*;
use type_creator::type_creator;

#[derive(Default)]
struct TestState {
    interner: Interner,
    types: Types,
    workspace: Workspace,
    resources: Resources,
    package_graph: PackageGraph,
    typec_ctx: TypecCtx,
    typec_transfere: TypecTransfere<'static>,
    mir_ctx: BorrowcCtx,
    arena: Arena,
    functions: String,
    mir: Mir,
    module: ModuleMir,
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
        let ext = TypecExternalCtx {
            types: &mut self.types,
            interner: &mut self.interner,
            workspace: &mut self.workspace,
            resources: &self.resources,
            transfere: &mut active,
            folder: &mut ConstFolderImpl {},
        };
        let meta = TypecMeta::new(&self.resources, module);

        TypecParser::new(&self.arena, &mut self.typec_ctx, ext, meta).execute(items);

        let mir_module = self.mir.modules.next();
        borrowc::compile_functions(
            module,
            mir_module,
            active.checked_funcs(),
            &mut MirCompilationCtx {
                module_ent: &mut self.module,
                reused: &mut self.mir_ctx,
                mir: &mut self.mir,
                types: &mut self.types,
                interner: &mut self.interner,
                workspace: &mut self.workspace,
                arena: &self.arena,
                resources: &self.resources,
            },
        );

        if !self.resources.is_external(module) {
            MirDisplayCtx {
                module: &self.module,
                interner: &self.interner,
                types: &self.types,
                resources: &self.resources,
                mir: &self.mir,
            }
            .display_funcs(
                active.checked_funcs().iter().map(|&(f, ..)| f),
                &mut self.functions,
            )
            .unwrap();
        }

        self.typec_transfere = active.erase();
        self.module.clear();
        self.typec_ctx.cast_checks().for_each(drop);
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

struct ConstFolderImpl {}
impl ConstFolder for ConstFolderImpl {
    fn fold(&mut self, _: Ty, _: TirNode, _: ConstFolderContext) -> FolderValue {
        unimplemented!()
    }
}

ctl_errors! {
    #[info => "borrowc repr of functions:\n{repr}"]
    error MirRepr {
        repr ref: String
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

#![feature(fs_try_exists)]
#![feature(thread_id_value)]
#![feature(default_free_fn)]
#![feature(array_zip)]
#![feature(unboxed_closures)]
#![feature(fn_traits)]

use std::{fs, path::Path, process::Command, vec};

use middleware::{Middleware, MiddlewareArgs, MiddlewareOutput};
use target_lexicon::Triple;

use diags::*;
use gen::*;
use packaging_t::*;

use testing::*;

#[derive(Default)]
struct TestState {
    middleware: Middleware,
}

impl TestState {
    fn finally(&mut self, out: MiddlewareOutput) {
        let s = &mut self.middleware;

        if let Some(ir) = out.ir {
            s.workspace.push(snippet! {
                info: ("generated ir:\n{}", ir);
            })
        }

        if s.workspace.has_errors() {
            return;
        }

        let thread_id = std::thread::current().id().as_u64().to_string();
        let exe_path = format!("o-{}.exe", thread_id);
        let obj_path = format!("o-{}.obj", thread_id);

        fs::write(&obj_path, out.binary).unwrap();

        let host = Triple::host().to_string();

        let compiler = cc::Build::new()
            .opt_level(0)
            .target(&host)
            .host(&host)
            .cargo_metadata(false)
            .get_compiler();

        let args = if compiler.is_like_msvc() {
            vec![
                "ucrt.lib".into(),
                format!("-link /ENTRY:{} /SUBSYSTEM:CONSOLE", gen::ENTRY_POINT_NAME,),
            ]
        } else if compiler.is_like_clang() {
            todo!()
        } else if compiler.is_like_gnu() {
            todo!()
        } else {
            unimplemented!("unknown compiler");
        };

        compiler
            .to_command()
            .arg(&obj_path)
            .args(args)
            .status()
            .unwrap();

        let path = Path::new(&exe_path).canonicalize().unwrap();

        let output = Command::new(path)
            .current_dir(std::env::current_dir().unwrap())
            .output()
            .unwrap();

        fs::remove_file(obj_path).unwrap();
        fs::remove_file(exe_path).unwrap();

        s.workspace.push(snippet! {
            info: ("status: {:x?}", output.status.code().unwrap());
            info: ("stdout:\n{}", String::from_utf8_lossy(&output.stdout));
            info: ("stderr:\n{}", String::from_utf8_lossy(&output.stderr));
        });
    }
}

impl Testable for TestState {
    fn exec(mut self, name: &str) -> (Workspace, Resources) {
        let jit_isa = Isa::host(true).unwrap();
        let isa = Isa::host(false).unwrap();
        let args = MiddlewareArgs {
            path: Path::new(name).into(),
            jit_isa,
            isa,
            incremental_path: None,
            max_cores: None,
            dump_ir: true,
        };
        if let Some(binary) = self.middleware.update(&args) {
            self.finally(binary);
        }
        (self.middleware.workspace, self.middleware.resources)
    }

    fn set_packages(&mut self, packages: Resources) {
        self.middleware.resources = packages;
    }
}

fn main() {
    gen_test! {
        TestState,
        false,
        simple "functions" {
            #[entry];
            fn main -> uint => pass(0);
            fn pass(a: uint) -> uint { return a };
            fn pass_with_implicit_return(a: uint) -> uint { a };
        }

        simple "recursion" {
            #[entry];
            fn main -> uint => 0;

            fn infinity(a: uint) => infinity(a);
        }

        simple "operators" {
            #[entry];
            fn main -> uint => 1 + 2 * 2 - 4 / 2 - 3;
        }

        // simple "compile-time" {
        //     fn sub(a: uint, b: uint) -> uint => a - b;

        //     #[entry];
        //     fn main -> uint => const sub(1, 1);
        // }

        // simple "external" {
        //     fn "default" putchar(c: char) -> u32 extern;

        //     #[entry];
        //     fn main -> uint {
        //         const putchar('a'); // compile time print
        //         putchar('\n');
        //         0
        //     };
        // }

        simple "generic" {
            fn [T] pass(value: T) -> T => value;

            #[entry];
            fn main -> u32 => pass(0uint);
        }

        simple "struct-constructor" {
            struct OnStack {
                a: uint;
                b: uint
            };

            struct InRegister {
                a: u32;
                b: u32
            };

            struct [T, E] Generic {
                a: T;
                b: E
            };

            #[entry];
            fn main -> uint {
                Generic::{
                    a: OnStack::{ a: 1; b: 2 };
                    b: InRegister::{ a: 3; b: 1 }
                };
                0
            };
        }
        simple "auto-ref-deref" {
            impl uint {
                fn reference(s: ^^^^^^^^^^^^Self) -> ^^^^^^^^^^^^Self => s;
                fn dereference(s: Self) -> Self => s;
            };

            #[entry];
            fn main -> uint => 0.reference().dereference();
        }

        simple "additional-param-garbage" {
            fn [T] pass(value: T) -> T => value;

            struct B;

            #[entry];
            fn main -> uint => pass::[uint, B](0uint, 'h');
        }

        simple "spec-test" {
            spec Flood {
                fn new -> uint;
            };

            struct Fool;

            impl Flood for Fool {
                fn new -> uint => 0;
            };

            fn [T: Flood] make_flood() -> uint => T::new();

            #[entry];
            fn main -> uint => make_flood::[Fool]();
        }

        simple "generic-spec" {
            spec [T] GenericSpec {
                fn take(t: T) -> Self;
            };

            impl GenericSpec[uint] for uint {
                fn take(t: uint) -> Self => t;
            };

            fn [B, T: GenericSpec[B]] take(t: B) -> T => T::take(t);

            #[entry];
            fn main() -> uint => take(0);
        }

        simple "struct access" {
            struct Foo {
                a: uint;
                b: uint;
            };

            #[entry];
            fn main -> uint => Foo::{ a: 1; b: 0 }.b;
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
                RegStruct::{ field: 0 }.field +
                RegStruct2::{ field: 1; field2: 0 }.field2
            }
        }

        simple "match" {
            struct Matched {
                a: uint;
                b: uint
            };

            #[entry];
            fn main() -> uint => match Matched::{ a: 0; b: 1 } {
                ::{ a: 1; b: 0 } => 1;
                ::{ a: 0; b: 1 } => 0;
                ::{ a; b: 0 } => a;
                ::{ a; b } => a + b;
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
                a => Returned::{ a; b: 0 };
            }.a;
        }

        simple "recursive-fib" {
            #[entry];
            fn main -> uint => fib(10) - 55;

            fn fib(x: uint) -> uint => match x {
                0 => 0;
                1 => 1;
                a => fib(a - 1) + fib(a - 2);
            };
        }

        simple "enum" {
            enum [T] Option {
                Some: T;
                None;
            };

            #[entry];
            fn main() -> uint => match Option::Some~0 {
                ::Some~4 => 5;
                ::Some~1 => 2;
                ::None => 3;
                ::Some~a => a;
            }
        }

        simple "enum-stress" {
            enum [T] Option {
                None;
                Some: T;
            };

            #[entry];
            fn main() -> uint => match Option::Some~Option::Some~Option::Some~0 {
                ::Some~::None => 5;
                ::Some~::Some~::None => 2;
                ::None => return 3;
                ::Some~a => match a {
                    ::Some~::Some~a => a;
                    ::Some~::None => 6;
                    ::None => 1;
                };
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

        simple "cast" {
            #[entry];
            fn main() -> uint => cast(0);
        }

        simple "cast-mismatch" {
            #[entry];
            fn main() -> u32 => cast(0);

            fn [F, T] my_cast(value: F) -> T => cast(value);
        }

        simple "swap-macro" {
            use {
                "water/option";
                "water/ptr";
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

            enum SwapState {
                Two: TwoTokens;
                Last: LastToken;
                Empty;
            };

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

            break;

            #[entry];
            swap! swap! fn -> main uint => 0;
            //fn swap! -> main uint => 0;
            //fn main -> uint => 0;
        }

        // simple "drop-gen" {
        //     use {
        //         "water/marker";
        //     };

        //     fn [T] drop(value: T) {};

        //     fn "default" putchar(c: char) -> u32 extern;

        //     struct A {
        //         ch: char;
        //     };

        //     impl A {
        //         fn new(ch: char) -> Self => ::{ ch };
        //         fn set_char(s: ^mut Self, ch: char) => s.ch = ch;
        //     };

        //     impl Drop for A {
        //         fn drop(v: ^mut Self) {
        //             putchar(v.ch);
        //             putchar(' ');
        //         };
        //     };

        //     fn drop_unused() {
        //         A::new('a');
        //     };

        //     fn drop_referenced() {
        //         A::new('a').set_char('b');
        //     };

        //     fn drop_variable() {
        //         let a = A::new('a');
        //         a.set_char('c');
        //     };

        //     fn drop_refed_variable() {
        //         let a = ^A::new('a');
        //         a.set_char('d');
        //     };

        //     fn move_in_drop() {
        //         let a = A::new('e');
        //         a = A::new('f');
        //     };

        //     fn drop_cond() {
        //         let a = A::new('g');
        //         if true => drop(a);
        //     };

        //     #[entry];
        //     fn main() -> uint {
        //         drop_unused();
        //         drop_referenced();
        //         drop_variable();
        //         drop_refed_variable();
        //         move_in_drop();
        //         drop_cond();
        //         0
        //     };
        // }
    }
}

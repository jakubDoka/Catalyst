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
    mir_ctx: MirBuilderCtx,
    functions: String,
    mir: Mir,
}

impl Scheduler for TestState {
    fn resources(&mut self) -> packaging::PackageLoader {
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
        let mut type_checked_funcs = vec![];
        ty_checker!(self, module).execute(
            items,
            &mut self.typec_ctx,
            self.ast_transfer.activate(),
            &mut type_checked_funcs,
        );

        mir_checker!(self, module)
            .funcs(&mut self.mir_ctx, &mut type_checked_funcs)
            .display_funcs(&self.mir_ctx.just_compiled, &mut self.functions)
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
    }
}

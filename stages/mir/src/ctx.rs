use lexing_t::Span;

use storage::*;
use typec_t::*;

use mir_t::*;

#[derive(Clone, Copy)]
pub struct VarMir {
    pub value: VRef<ValueMir>,
}

#[derive(Default)]
pub struct MirCtx {
    pub depth: u32,
    pub no_moves: bool,
    pub func: FuncMirInner,
    pub dd: DebugData,
    pub vars: Vec<VarMir>,
    pub generics: Vec<FragSlice<Spec>>,
    pub args: Vec<VRef<ValueMir>>,
    pub insts: Vec<(InstMir, Span)>,
    pub used_types: Map<Ty, VRef<MirTy>>,
    pub just_compiled: Vec<FragRef<Func>>,
    pub generic_types: Vec<VRef<MirTy>>,
    pub value_depths: ShadowMap<ValueMir, u32>,
}

/*
struct bump_alloc::primitives::VRef<mir_t::mir::ValueMir> mir::ctx::MirCtx::value(union enum2$<typec_t::ty::Ty>, struct typec_t::typec::Typec *) (c:\src\rust\catalyst\stages\mir\src\ctx.rs:34)
struct bump_alloc::primitives::VRef<mir_t::mir::ValueMir> mir::state_gen::MirChecker::value(union enum2$<typec_t::ty::Ty>) (c:\src\rust\catalyst\stages\mir\src\builder.rs:650)
static struct mir_t::mir::FuncMirInner mir::state_gen::MirChecker::func(struct bump_alloc::primitives::FragRef<typec_t::func::Func>, struct typec_t::tir::TirNode) (c:\src\rust\catalyst\stages\mir\src\builder.rs:40)
struct mir::state_gen::MirChecker * mir::state_gen::MirChecker::funcs(struct bump_alloc::bump_vec::BumpVec<tuple$<bump_alloc::primitives::FragRef<typec_t::func::Func>,typec_t::tir::TirNode> > *) (c:\src\rust\catalyst\stages\mir\src\builder.rs:17)
void mir_test::impl$0::parse_segment(struct mir_test::TestState *, struct bump_alloc::primitives::VRef<packaging_t::packaging::Module>, struct parsing::parser::items::GroupedItemsAst) (c:\src\rust\catalyst\tests\mir-test\src\main.rs:63)
static void packaging::scheduler::Scheduler::execute<mir_test::TestState>(struct mir_test::TestState *, struct ref$<std::path::Path>) (c:\src\rust\catalyst\stages\packaging\src\scheduler.rs:73)
static struct tuple$<diags::items::Workspace,packaging_t::packaging::Resources> testing::items::impl$0::exec<mir_test::TestState>(struct mir_test::TestState, struct str) (c:\src\rust\catalyst\utils\testing\src\lib.rs:128)
mir_test::main::{{closure}}::{{closure}} (c:\src\rust\catalyst\utils\testing\src\lib.rs:19)
core::ops::function::FnOnce::call_once (@core::ops::function::FnOnce::call_once:15)
void testing::items::test_case::closure$0(struct testing::items::test_case::closure_env$0 *) (c:\src\rust\catalyst\utils\testing\src\lib.rs:160)
void testing::items::test_case(struct str, union enum2$<core::option::Option<ref$<std::thread::scoped::Scope> > >,  *) (c:\src\rust\catalyst\utils\testing\src\lib.rs:174)
mir_test::main::{{closure}} (c:\src\rust\catalyst\utils\testing\src\lib.rs:16)
void std::thread::scoped::scope::closure$0<mir_test::main::closure_env$0,tuple$<> >(struct std::thread::scoped::scope::closure_env$0<mir_test::main::closure_env$0,tuple$<> >) (@std::thread::scoped::scope::{{closure}}:9)
void core::panic::unwind_safe::impl$23::call_once<tuple$<>,std::thread::scoped::scope::closure_env$0<mir_test::main::closure_env$0,tuple$<> > >(struct core::panic::unwind_safe::AssertUnwindSafe<std::thread::scoped::scope::closure_env$0<mir_test::main::closure_env$0,tuple$<> > >) (@<core::panic::unwind_safe::AssertUnwindSafe<F> as core::ops::function::FnOnce<()>>::call_once:6)
static void std::panicking::try::do_call<core::panic::unwind_safe::AssertUnwindSafe<std::thread::scoped::scope::closure_env$0<mir_test::main::closure_env$0,tuple$<> > >,tuple$<> >(unsigned char *) (@7ff65e0ba2d8..7ff65e0ba339:3)
14000A3A3 (@7ff65e0ba3a3..7ff65e0ba411:3)
union enum2$<core::result::Result<tuple$<>,alloc::boxed::Box<dyn$<core::any::Any,core::marker::Send>,alloc::alloc::Global> > > std::panicking::try<tuple$<>,core::panic::unwind_safe::AssertUnwindSafe<std::thread::scoped::scope::closure_env$0<mir_test::main::closure_env$0,tuple$<> > > >(struct core::panic::unwind_safe::AssertUnwindSafe<std::thread::scoped::scope::closure_env$0<mir_test::main::closure_env$0,tuple$<> > >) (@std::panicking::try:15)
union enum2$<core::result::Result<tuple$<>,alloc::boxed::Box<dyn$<core::any::Any,core::marker::Send>,alloc::alloc::Global> > > std::panic::catch_unwind<core::panic::unwind_safe::AssertUnwindSafe<std::thread::scoped::scope::closure_env$0<mir_test::main::closure_env$0,tuple$<> > >,tuple$<> >(struct core::panic::unwind_safe::AssertUnwindSafe<std::thread::scoped::scope::closure_env$0<mir_test::main::closure_env$0,tuple$<> > >) (@std::panic::catch_unwind:6)
std::thread::scoped::scope (@std::thread::scoped::scope:33)
static void mir_test::main() (c:\src\rust\catalyst\tests\mir-test\src\main.rs:79)
*/

impl MirCtx {
    pub fn value(&mut self, ty: Ty, typec: &Typec) -> VRef<ValueMir> {
        if ty == Ty::UNIT {
            return ValueMir::UNIT;
        } else if ty == Ty::TERMINAL {
            return ValueMir::TERMINAL;
        }

        let ty = self.project_ty(ty, typec);
        let val = self.func.values.push(ValueMir { ty });
        self.value_depths[val] = self.depth;
        val
    }

    pub fn create_block(&mut self) -> VRef<BlockMir> {
        self.func.blocks.push(BlockMir::default())
    }

    pub fn get_var(&self, var: VRef<VarHeaderTir>) -> VarMir {
        self.vars[var.index()]
    }

    pub fn project_ty_slice(&mut self, ty_slice: &[Ty], typec: &Typec) -> VRefSlice<MirTy> {
        self.func.ty_params.extend(ty_slice.iter().map(|&ty| {
            Self::project_ty_low(
                ty,
                &mut self.used_types,
                &mut self.func.types,
                &mut self.generic_types,
                typec,
            )
        }))
    }

    pub fn project_ty(&mut self, ty: Ty, typec: &Typec) -> VRef<MirTy> {
        Self::project_ty_low(
            ty,
            &mut self.used_types,
            &mut self.func.types,
            &mut self.generic_types,
            typec,
        )
    }

    pub fn project_ty_low(
        ty: Ty,
        used_types: &mut Map<Ty, VRef<MirTy>>,
        dependant_types: &mut PushMap<MirTy>,
        generic_types: &mut Vec<VRef<MirTy>>,
        typec: &Typec,
    ) -> VRef<MirTy> {
        if let Some(&ty) = used_types.get(&ty) {
            return ty;
        }

        let mir_ty = dependant_types.push(MirTy { ty });
        used_types.insert(ty, mir_ty);

        if typec.contains_params(ty) {
            generic_types.push(mir_ty);
        }

        mir_ty
    }

    pub fn close_block(&mut self, id: VRef<BlockMir>, control_flow: ControlFlowMir) {
        let block = BlockMir {
            args: self.func.value_args.extend(self.args.drain(..)),
            insts: self
                .func
                .insts
                .extend(self.insts.iter().map(|&(inst, ..)| inst)),
            control_flow,

            ..self.func.blocks[id] // inherit ref_count
        };

        self.func
            .insts
            .indexed(block.insts)
            .zip(self.insts.drain(..).map(|(.., span)| span))
            .for_each(|((key, ..), span)| self.dd.instr_spans[key] = span);

        self.func.blocks[id] = block;
    }

    pub fn clear(&mut self) -> FuncMirInner {
        self.vars.clear();
        self.dd.clear();
        self.used_types.clear();
        self.generics.clear();

        let mut cln = self.func.clone();
        cln.generics = cln.ty_params.extend(self.generic_types.drain(..));
        self.func.clear();
        cln
    }

    pub fn value_ty(&self, value: VRef<ValueMir>) -> Ty {
        self.func.value_ty(value)
    }
}

#[must_use]
pub struct MirVarFrame {
    pub base: usize,
}

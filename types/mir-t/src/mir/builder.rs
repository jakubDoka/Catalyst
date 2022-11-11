use std::{
    mem,
    ops::{Deref, DerefMut},
};

use lexing_t::Span;

use storage::*;
use typec_t::*;

use crate::*;

pub struct MirBuilder<'a> {
    pub current_block: Option<VRef<BlockMir>>,
    pub arena: &'a Arena,
    pub ctx: &'a mut MirBuilderCtx,
}

impl<'a> MirBuilder<'a> {
    pub fn new(entry_block: VRef<BlockMir>, arena: &'a Arena, ctx: &'a mut MirBuilderCtx) -> Self {
        Self {
            current_block: Some(entry_block),
            arena,
            ctx,
        }
    }

    pub fn field(
        &mut self,
        header: VRef<ValueMir>,
        index: u32,
        dest: VRef<ValueMir>,
        span: Span,
    ) -> Option<()> {
        self.inst(InstMir::Field(header, index, dest), span)?;
        self.ctx.moves.owners[dest] = Owner::Indirect(index, header);
        Some(())
    }

    pub fn value_ty(&self, value: VRef<ValueMir>) -> Ty {
        self.ctx.func.value_ty(value)
    }

    // pub fn drop_value(&mut self, value: VRef<ValueMir>, span: Span, typec: &mut Typec, interner: &mut Interner) -> Result<(), DropError> {
    //     let root = match self.ctx.moves.owners[value] {
    //         Owner::Temporary(graph) => graph,
    //         Owner::Nested(..) => unreachable!(),
    //         Owner::Var(var) => self.ctx.vars[var].graph,
    //     };
    //     let drop_fn = typec[SpecBase::DROP].methods.index(0);
    //     self.drop_value_recur(value, root, &mut DropState { drop_fn, span, typec, interner })
    // }

    // fn drop_value_recur(&mut self, value: VRef<ValueMir>, root: OptVRef<MoveGraph>, ds: &mut DropState) -> Result<(), DropError> {
    //     let root = root.map(|r| self.ctx.moves.graphs[r]);
    //     if let Some(MoveGraph::Gone(..) | MoveGraph::GoneSplit(..)) = root {
    //         return Ok(());
    //     }

    //     let ty = self.ctx.func.types[self.ctx.func.values[value].ty].ty;
    //     let is_drop = ty.is_drop(&self.ctx.generics, ds.typec, ds.interner);

    //     if let (Some(true) | None, Some(MoveGraph::Partial(children))) = (is_drop, root) {
    //         return Err(DropError::Partial(children));
    //     }

    //     match is_drop {
    //         Some(true) => {
    //             let ptr_ty = Ty::Pointer(ds.typec.pointer_to(Mutability::Mutable, ty, ds.interner));
    //             let ptr = self.value(ptr_ty, ds.typec);
    //             self.inst(InstMir::Ref(value, ptr), ds.span);
    //             let caller = CallMir {
    //                 callable: CallableMir::SpecFunc(ds.drop_fn),
    //                 params: self.ctx.func.ty_params.extend([self.ctx.func.values[value].ty]),
    //                 args: self.ctx.func.value_args.extend([ptr]),
    //             };
    //             let caller = self.ctx.func.calls.push(caller);
    //             self.inst(InstMir::Call(caller, None), ds.span)
    //         },
    //         Some(false) => None,
    //         None => self.inst(InstMir::MayDrop(value), ds.span),
    //     };

    //     let children = match root {
    //         Some(MoveGraph::Partial(children) | MoveGraph::PresentSplit(children)) => Some(children),
    //         _ => None,
    //     };

    //     match ty {
    //         Ty::Struct(s) => {
    //             for (i, field) in ds.typec[s].fields.keys().enumerate() {
    //                 let field = ds.typec[field];
    //                 let node = children.map(|c| c.index(i));
    //                 let field_value = self.value(field.ty, ds.typec);
    //                 self.inst(InstMir::Field(value, i as u32, field_value), ds.span);
    //                 self.drop_value_recur(field_value, node, ds)?;
    //             }
    //         },
    //         Ty::Enum(e) => {

    //         },
    //         Ty::Instance(_) => todo!(),
    //         Ty::Pointer(..) |
    //         Ty::Param(..) |
    //         Ty::Builtin(..) => (),
    //     }

    //     Ok(())
    // }

    pub fn inst(&mut self, kind: InstMir, span: Span) -> Option<()> {
        self.current_block?;
        self.ctx.insts.push((kind, span));
        Some(())
    }

    pub fn value(&mut self, ty: Ty, typec: &Typec) -> VRef<ValueMir> {
        if ty == Ty::UNIT {
            return ValueMir::UNIT;
        }
        if ty == Ty::TERMINAL {
            return ValueMir::TERMINAL;
        }

        let ty = self.ctx.project_ty(ty, typec);
        self.ctx.func.values.push(ValueMir { ty })
    }

    pub fn create_var(&mut self, value: VRef<ValueMir>) {
        self.ctx.vars.push(VarMir { value });
    }

    pub fn close_block(&mut self, span: Span, control_flow: ControlFlowMir) -> bool {
        let Some(current_block) = self.current_block else {
            return true;
        };

        self.increment_block_refcount(control_flow);

        self.ctx.close_block(current_block, control_flow);
        self.ctx.dd.block_closers[current_block] = span;

        false
    }

    fn increment_block_refcount(&mut self, control_flow: ControlFlowMir) {
        match control_flow {
            ControlFlowMir::Terminal | ControlFlowMir::Return(..) => {}
            ControlFlowMir::Split(.., a, b) => {
                self.ctx.func.blocks[a].ref_count += 1;
                self.ctx.func.blocks[b].ref_count += 1;
            }
            ControlFlowMir::Goto(a, ..) => {
                self.ctx.func.blocks[a].ref_count += 1;
            }
        }
    }

    pub fn select_block(&mut self, block: VRef<BlockMir>) -> bool {
        mem::replace(&mut self.current_block, Some(block)).is_some()
    }
}

// struct DropState<'ctx> {
//     drop_fn: FragRef<SpecFunc>,
//     span: Span,
//     typec: &'ctx mut Typec,
//     interner: &'ctx mut Interner,
// }

#[derive(Clone, Copy)]
pub struct VarMir {
    pub value: VRef<ValueMir>,
}

impl CtxFrameItem for VarMir {
    type Ctx = MirBuilderCtx;
    ctx_frame_seq_getters! { |ctx| ctx.vars }
}

// #[derive(Debug)]
// pub enum DropError {
//     Partial(VSlice<MoveGraph>),
// }

#[derive(Debug)]
pub enum MoveError {
    Graph(MoveGraph),
    Pointer(Span),
}

#[derive(Clone, Copy, Debug)]
pub enum MoveGraph {
    Present,
    Gone(Span),
    PresentSplit(VSlice<MoveGraph>),
    GoneSplit(VSlice<MoveGraph>),
    Partial(VSlice<MoveGraph>),
}

#[derive(Clone, Copy, Debug)]
pub enum Owner {
    Direct(OptVRef<MoveGraph>),
    Indirect(u32, VRef<ValueMir>),
}

impl Default for Owner {
    fn default() -> Self {
        Self::Direct(None)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Move {
    pub graph: VRef<MoveGraph>,
    pub old: MoveGraph,
}

#[derive(Default)]
pub struct MirMoveCtx {
    pub terminating_branches: Vec<bool>,
    pub cached_moves: Frames<Move>,
    pub owners: ShadowMap<ValueMir, Owner>,
    pub graphs: PushMap<MoveGraph>,
    pub current_branch: Map<VRef<MoveGraph>, MoveGraph>,
    pub brach_move_slices: PushMap<Move>,
    pub enums: Vec<EnumMove>,
}

impl MirMoveCtx {
    pub fn clear(&mut self) {
        self.terminating_branches.clear();
        self.cached_moves.clear();
        self.owners.clear();
        self.graphs.clear();
        self.current_branch.clear();
        self.brach_move_slices.clear();
    }
}

#[must_use]
pub struct MoveFrame(usize);

#[derive(Default)]
pub struct MirBuilderCtx {
    pub untracked_moves: bool,
    pub func: FuncMirInner,
    pub dd: DebugData,
    pub vars: Vec<VarMir>,
    pub generics: Vec<FragSlice<Spec>>,
    pub args: Vec<VRef<ValueMir>>,
    pub insts: Vec<(InstMir, Span)>,
    pub used_types: Map<Ty, VRef<MirTy>>,
    pub just_compiled: Vec<FragRef<Func>>,
    pub generic_types: Vec<VRef<MirTy>>,
    pub moves: MirMoveCtx,
}

impl MirBuilderCtx {
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
        self.moves.clear();

        let mut cln = self.func.clone();
        cln.generics = cln.ty_params.extend(self.generic_types.drain(..));
        self.func.clear();
        cln
    }

    pub fn value_ty(&self, value: VRef<ValueMir>) -> Ty {
        self.func.value_ty(value)
    }

    pub fn move_in(&mut self, value: VRef<ValueMir>, typec: &mut Typec, interner: &mut Interner) {
        if self.untracked_moves {
            return;
        }

        let ty = self.value_ty(value);

        if ty.is_copy(&self.generics, typec, interner) {
            return;
        }

        let mut current = value;
        let mut path = bumpvec![];
        let root = loop {
            match self.moves.owners[current] {
                Owner::Direct(graph) => break graph,
                Owner::Indirect(id, header) => {
                    path.push((id, self.func.value_ty(header)));
                    current = header;
                }
            }
        };

        let Some(root) = root else {
            return;
        };

        let mut graph_path = bumpvec![];
        let mut current = root;
        for (id, ty) in path.into_iter().rev() {
            graph_path.push(current);
            current = match self.moves.graphs[current] {
                MoveGraph::Present | MoveGraph::PresentSplit(..) => return,
                MoveGraph::Gone(span) => {
                    match self.expand_graph_node(ty, id, Some(span), root, typec) {
                        Some(Some(graph)) => graph,
                        Some(None) => graph_path.pop().unwrap(),
                        None => todo!(),
                    }
                }
                MoveGraph::GoneSplit(children) => {
                    self.moves.graphs[current] = MoveGraph::Partial(children);
                    children.index(id as usize)
                }
                MoveGraph::Partial(children, ..) => children.index(id as usize),
            }
        }

        self.change_graph(current, MoveGraph::Present);

        for node in graph_path.into_iter().rev() {
            match self.moves.graphs[node] {
                MoveGraph::Present
                | MoveGraph::PresentSplit(..)
                | MoveGraph::Gone(..)
                | MoveGraph::GoneSplit(..) => unreachable!(),
                MoveGraph::Partial(children) => {
                    let collapse = self.moves.graphs[children]
                        .iter()
                        .all(|&g| matches!(g, MoveGraph::Present | MoveGraph::PresentSplit(..)));
                    if collapse {
                        self.change_graph(node, MoveGraph::PresentSplit(children));
                    } else {
                        break;
                    }
                }
            }
        }
    }

    pub fn enum_move_out(
        &mut self,
        value: VRef<ValueMir>,
        inner_value: VRef<ValueMir>,
        span: Span,
        typec: &mut Typec,
        interner: &mut Interner,
    ) -> Result<(), MoveError> {
        self.move_out(value, span, typec, interner)?;
        self.moves.enums.push(EnumMove { value, inner_value });
        Ok(())
    }

    pub fn move_out(
        &mut self,
        value: VRef<ValueMir>,
        span: Span,
        typec: &mut Typec,
        interner: &mut Interner,
    ) -> Result<(), MoveError> {
        if self.untracked_moves {
            return Ok(());
        }

        let ty = self.value_ty(value);

        if ty.is_copy(&self.generics, typec, interner) {
            return Ok(());
        }
        let mut current = &mut self.moves.owners[value];
        let mut path = bumpvec![];
        let mut current = *loop {
            match current {
                Owner::Direct(graph) => break graph,
                &mut Owner::Indirect(id, header, ..) => {
                    path.push((id, self.func.value_ty(header)));
                    current = &mut self.moves.owners[header];
                }
            }
        }
        .get_or_insert_with(|| self.moves.graphs.push(MoveGraph::Present));

        let mut graph_path = bumpvec![];
        for (id, ty) in path.into_iter().rev() {
            graph_path.push(current);
            current = match self.moves.graphs[current] {
                MoveGraph::Present => match self
                    .expand_graph_node(ty, id, None, current, typec)
                    .ok_or(MoveError::Pointer(span))?
                {
                    Some(current) => current,
                    None => graph_path.pop().unwrap(),
                },
                MoveGraph::PresentSplit(children) => {
                    self.moves.graphs[current] = MoveGraph::Partial(children);
                    children.index(id as usize)
                }
                g @ MoveGraph::Gone(..) => return Err(MoveError::Graph(g)),
                MoveGraph::GoneSplit(children) | MoveGraph::Partial(children) => {
                    children.index(id as usize)
                }
            }
        }

        let g = self.change_graph(current, MoveGraph::Gone(span));
        let (MoveGraph::Present | MoveGraph::PresentSplit(..)) = g else {
            return Err(MoveError::Graph(g));
        };

        for node in graph_path.into_iter().rev() {
            match self.moves.graphs[node] {
                MoveGraph::Present
                | MoveGraph::PresentSplit(..)
                | MoveGraph::Gone(..)
                | MoveGraph::GoneSplit(..) => unreachable!(),
                MoveGraph::Partial(children) => {
                    let collapse = self.moves.graphs[children]
                        .iter()
                        .all(|&g| matches!(g, MoveGraph::Gone(..) | MoveGraph::GoneSplit(..)));
                    if collapse {
                        self.change_graph(node, MoveGraph::GoneSplit(children));
                    } else {
                        break;
                    }
                }
            }
        }

        Ok(())
    }

    fn change_graph(&mut self, addr: VRef<MoveGraph>, new: MoveGraph) -> MoveGraph {
        let old = mem::replace(&mut self.moves.graphs[addr], new);
        self.moves.current_branch.insert(addr, old);
        old
    }

    fn expand_graph_node(
        &mut self,
        ty: Ty,
        id: u32,
        gone: Option<Span>,
        root: VRef<MoveGraph>,
        typec: &Typec,
    ) -> Option<Option<VRef<MoveGraph>>> {
        let traverse_struct = |sel: &mut Self, s: FragRef<Struct>| {
            let fill = if let Some(gone) = gone {
                MoveGraph::Gone(gone)
            } else {
                MoveGraph::Present
            };
            let children = (0..typec[s].fields.len()).map(|_| fill);
            let slice = sel.moves.graphs.extend(children);
            sel.change_graph(root, MoveGraph::Partial(slice));
            Some(Some(slice.index(id as usize)))
        };

        match ty {
            Ty::Struct(s) => traverse_struct(self, s),
            Ty::Enum(..) => Some(None),
            Ty::Instance(inst) => match typec[inst].base {
                GenericTy::Struct(s) => traverse_struct(self, s),
                GenericTy::Enum(..) => Some(None),
            },
            Ty::Pointer(..) => None,
            Ty::Param(..) | Ty::Builtin(..) => unreachable!(),
        }
    }

    pub fn start_move_frame(&mut self) -> MoveFrame {
        self.moves.cached_moves.extend(
            self.moves
                .current_branch
                .drain()
                .map(|(graph, old)| Move { graph, old }),
        );
        self.moves.cached_moves.mark();
        MoveFrame(self.moves.cached_moves.len())
    }

    pub fn discard_move_branch(&mut self) {
        self.moves
            .current_branch
            .drain()
            .for_each(|(graph, old)| self.moves.graphs[graph] = old);
        self.moves.cached_moves.mark();
        self.moves.terminating_branches.push(true);
    }

    pub fn save_move_branch(&mut self) {
        self.moves
            .cached_moves
            .extend(self.moves.current_branch.drain().map(|(graph, old)| Move {
                graph,
                old: mem::replace(&mut self.moves.graphs[graph], old),
            }));
        self.moves.cached_moves.mark();
        self.moves.terminating_branches.push(false);
    }

    pub fn end_move_frame(&mut self, frame: MoveFrame) {
        let mut excess = self.moves.cached_moves.len() - frame.0;
        (0..excess).for_each(|_| self.moves.cached_moves.join_frames());
        excess -= self
            .moves
            .terminating_branches
            .drain(frame.0..)
            .map(|b| b as usize)
            .sum::<usize>();
        let mut count_map = ShadowMap::new();
        for r#move in self.moves.cached_moves.pop().collect::<BumpVec<_>>() {
            match (r#move.old, self.moves.graphs[r#move.graph]) {
                (
                    MoveGraph::Present | MoveGraph::PresentSplit(..),
                    MoveGraph::Gone(..) | MoveGraph::Partial(..) | MoveGraph::GoneSplit(..),
                ) => {
                    if count_map[r#move.graph] == -1 {
                        continue;
                    }
                    count_map[r#move.graph] += 1;
                    if count_map[r#move.graph] == excess as isize {
                        self.change_graph(r#move.graph, r#move.old);
                    }
                }
                (
                    MoveGraph::Gone(..) | MoveGraph::Partial(..) | MoveGraph::GoneSplit(..),
                    MoveGraph::Present | MoveGraph::PresentSplit(..),
                ) => {
                    if count_map[r#move.graph] >= 0 {
                        self.change_graph(r#move.graph, r#move.old);
                        count_map[r#move.graph] = -1;
                    }
                }
                _ => (),
            }
        }

        self.moves.current_branch.extend(
            self.moves
                .cached_moves
                .pop()
                .map(|Move { graph, old }| (graph, old)),
        );
    }
}

pub struct EnumMove {
    pub value: VRef<ValueMir>,
    pub inner_value: VRef<ValueMir>,
}

impl CtxFrameItem for EnumMove {
    type Ctx = MirBuilderCtx;

    type Args<'a> = (&'a mut Typec, &'a mut Interner);

    ctx_frame_seq_getters! { |ctx| ctx.moves.enums }

    fn process_impl(
        ctx: &mut Self::Ctx,
        (typec, interner): Self::Args<'_>,
        selfs: impl Iterator<Item = Self>,
    ) {
        for EnumMove { value, inner_value } in selfs {
            if let Owner::Direct(graph) = ctx.moves.owners[inner_value] {
                match graph.map(|g| ctx.moves.graphs[g]) {
                    None | Some(MoveGraph::Present | MoveGraph::PresentSplit(..)) => {
                        ctx.move_in(value, typec, interner);
                    }
                    _ => (),
                }
            }
        }
    }
}

#[derive(Clone)]
pub struct DependantTypes(PushMap<MirTy>);

impl DependantTypes {
    pub fn clear(&mut self) {
        self.0.truncate(2);
    }
}

impl Default for DependantTypes {
    fn default() -> Self {
        Self({
            let mut values = PushMap::new();
            values.push(MirTy { ty: Ty::UNIT });
            values.push(MirTy { ty: Ty::TERMINAL });
            values
        })
    }
}

impl Deref for DependantTypes {
    type Target = PushMap<MirTy>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for DependantTypes {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/*
    struct bump_alloc::primitives::VRef<mir_t::mir::ValueMir> mir_t::mir::builder::MirBuilder::value(union enum2$<typec_t::ty::Ty>, struct typec_t::typec::Typec *) (c:\src\rust\catalyst\types\mir-t\src\mir\builder.rs:127)
static void mir::state_gen::MirChecker::bind_pattern_vars(struct typec_t::tir::PatTir, struct bump_alloc::primitives::VRef<mir_t::mir::ValueMir>, struct mir_t::mir::builder::MirBuilder *) (c:\src\rust\catalyst\stages\mir\src\builder.rs:271)
static void mir::state_gen::MirChecker::bind_pattern_vars(struct typec_t::tir::PatTir, struct bump_alloc::primitives::VRef<mir_t::mir::ValueMir>, struct mir_t::mir::builder::MirBuilder *) (c:\src\rust\catalyst\stages\mir\src\builder.rs:284)
static void mir::state_gen::MirChecker::match_arm(struct typec_t::tir::MatchArmTir, union enum2$<core::option::Option<bump_alloc::primitives::VRef<mir_t::mir::BlockMir> > > *, struct bump_alloc::primitives::VRef<mir_t::mir::ValueMir>, struct bump_alloc::primitives::VRef<mir_t::mir::ValueMir>, bool, struct mir_t::mir::builder::MirBuilder *) (c:\src\rust\catalyst\stages\mir\src\builder.rs:215)
static union enum2$<core::option::Option<bump_alloc::primitives::VRef<mir_t::mir::ValueMir> > > mir::state_gen::MirChecker::match(struct typec_t::tir::MatchTir, struct lexing_t::span::Span, struct bump_alloc::primitives::VRef<mir_t::mir::ValueMir>, bool, struct mir_t::mir::builder::MirBuilder *) (c:\src\rust\catalyst\stages\mir\src\builder.rs:189)
union enum2$<core::option::Option<bump_alloc::primitives::VRef<mir_t::mir::ValueMir> > > mir::state_gen::MirChecker::node(struct typec_t::tir::TirNode, union enum2$<core::option::Option<bump_alloc::primitives::VRef<mir_t::mir::ValueMir> > >, bool, struct mir_t::mir::builder::MirBuilder *) (c:\src\rust\catalyst\stages\mir\src\builder.rs:78)
static union enum2$<core::option::Option<bump_alloc::primitives::VRef<mir_t::mir::ValueMir> > > mir::state_gen::MirChecker::block(struct slice$<typec_t::tir::TirNode>, struct bump_alloc::primitives::VRef<mir_t::mir::ValueMir>, bool, struct mir_t::mir::builder::MirBuilder *) (c:\src\rust\catalyst\stages\mir\src\builder.rs:560)
union enum2$<core::option::Option<bump_alloc::primitives::VRef<mir_t::mir::ValueMir> > > mir::state_gen::MirChecker::node(struct typec_t::tir::TirNode, union enum2$<core::option::Option<bump_alloc::primitives::VRef<mir_t::mir::ValueMir> > >, bool, struct mir_t::mir::builder::MirBuilder *) (c:\src\rust\catalyst\stages\mir\src\builder.rs:66)
union enum2$<core::option::Option<enum2$<core::option::Option<bump_alloc::primitives::VRef<mir_t::mir::ValueMir> > > > > mir::builder::impl$0::return::closure$0(struct mir::builder::impl$0::return::closure_env$0, struct typec_t::tir::TirNode *) (c:\src\rust\catalyst\stages\mir\src\builder.rs:675)
union enum2$<core::option::Option<enum2$<core::option::Option<bump_alloc::primitives::VRef<mir_t::mir::ValueMir> > > > > enum2$<core::option::Option<ref$<typec_t::tir::TirNode> > >::and_then<ref$<typec_t::tir::TirNode>,enum2$<core::option::Option<bump_alloc::primitives::VRef<mir_t::mir::ValueMir> > >,mir::builder::impl$0::return::closure_env$0>(union enum2$<core::option::Option<ref$<typec_t::tir::TirNode> > >, struct mir::builder::impl$0::return::closure_env$0) (@core::option::Option<T>::and_then:26)
static union enum2$<core::option::Option<bump_alloc::primitives::VRef<mir_t::mir::ValueMir> > > mir::state_gen::MirChecker::return(union enum2$<core::option::Option<ref$<typec_t::tir::TirNode> > >, struct lexing_t::span::Span, struct mir_t::mir::builder::MirBuilder *) (c:\src\rust\catalyst\stages\mir\src\builder.rs:673)
union enum2$<core::option::Option<bump_alloc::primitives::VRef<mir_t::mir::ValueMir> > > mir::state_gen::MirChecker::node(struct typec_t::tir::TirNode, union enum2$<core::option::Option<bump_alloc::primitives::VRef<mir_t::mir::ValueMir> > >, bool, struct mir_t::mir::builder::MirBuilder *) (c:\src\rust\catalyst\stages\mir\src\builder.rs:72)
static struct mir_t::mir::FuncMirInner mir::state_gen::MirChecker::func(struct bump_alloc::primitives::FragRef<typec_t::func::Func>, struct typec_t::tir::TirNode, struct bump_alloc::arena::Arena *, struct mir_t::mir::builder::MirBuilderCtx *) (c:\src\rust\catalyst\stages\mir\src\builder.rs:52)
struct mir::state_gen::MirChecker * mir::state_gen::MirChecker::funcs(struct bump_alloc::arena::Arena *, struct mir_t::mir::builder::MirBuilderCtx *, struct bump_alloc::bump_vec::BumpVec<tuple$<bump_alloc::primitives::FragRef<typec_t::func::Func>,typec_t::tir::TirNode> > *) (c:\src\rust\catalyst\stages\mir\src\builder.rs:22)
void mir_test::impl$0::parse_segment(struct mir_test::TestState *, struct bump_alloc::primitives::VRef<packaging_t::packaging::Module>, struct parsing::parser::items::GroupedItemsAst) (c:\src\rust\catalyst\tests\mir-test\src\main.rs:62)
static void packaging::scheduler::Scheduler::execute<mir_test::TestState>(struct mir_test::TestState *, struct ref$<std::path::Path>) (c:\src\rust\catalyst\stages\packaging\src\scheduler.rs:73)
static struct tuple$<diags::items::Workspace,packaging_t::packaging::Resources> testing::items::impl$0::exec<mir_test::TestState>(struct mir_test::TestState, struct str) (c:\src\rust\catalyst\utils\testing\src\lib.rs:128)
mir_test::main::{{closure}}::{{closure}} (c:\src\rust\catalyst\utils\testing\src\lib.rs:19)
core::ops::function::FnOnce::call_once (@core::ops::function::FnOnce::call_once:15)
void testing::items::test_case::closure$0(struct testing::items::test_case::closure_env$0 *) (c:\src\rust\catalyst\utils\testing\src\lib.rs:160)*/

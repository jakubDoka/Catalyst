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
        self.ctx.moves.owners[dest] = Owner::Nested(index, header);
        Some(())
    }

    pub fn value_ty(&self, value: VRef<ValueMir>) -> Ty {
        self.ctx.func.types[self.ctx.func.values[value].ty].ty
    }

    pub fn r#move_in(&mut self, value: VRef<ValueMir>, typec: &mut Typec, interner: &mut Interner) {
        if self.ctx.untracked_moves {
            return;
        }

        let ty = self.value_ty(value);

        if ty.is_copy(&self.ctx.generics, typec, interner) {
            return;
        }

        let mut root_value = value;
        let mut path = bumpvec![];
        let root = loop {
            match self.ctx.moves.owners[root_value] {
                Owner::Temporary(graph) => break graph,
                Owner::Nested(id, header) => {
                    path.push((id, self.ctx.func.types[self.ctx.func.values[header].ty].ty));
                    root_value = header;
                }
                Owner::Var(var) => break self.ctx.vars[var].graph,
            }
        };

        let Some(root) = root else {
            return;
        };

        let mut graph_path = bumpvec![];
        let mut current = root;
        for (id, ty) in path.into_iter().rev() {
            graph_path.push(current);
            current = match self.ctx.moves.graphs[current] {
                MoveGraph::Present | MoveGraph::PresentSplit(..) => return,
                MoveGraph::Gone(span) => {
                    match self.expand_graph_node(ty, id, Some(span), root, typec) {
                        Some(graph) => graph,
                        None => todo!(),
                    }
                }
                MoveGraph::GoneSplit(children) => {
                    self.ctx.moves.graphs[current] = MoveGraph::Partial(children);
                    children.index(id as usize)
                }
                MoveGraph::Partial(children, ..) => children.index(id as usize),
            }
        }

        self.change_graph(current, MoveGraph::Present);

        for node in graph_path.into_iter().rev() {
            match self.ctx.moves.graphs[node] {
                MoveGraph::Present
                | MoveGraph::PresentSplit(..)
                | MoveGraph::Gone(..)
                | MoveGraph::GoneSplit(..) => unreachable!(),
                MoveGraph::Partial(children) => {
                    let collapse = self.ctx.moves.graphs[children]
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

    pub fn r#move_out(
        &mut self,
        value: VRef<ValueMir>,
        span: Span,
        typec: &mut Typec,
        interner: &mut Interner,
    ) -> Result<(), MoveError> {
        if self.ctx.untracked_moves {
            return Ok(());
        }

        let ty = self.value_ty(value);

        if ty.is_copy(&self.ctx.generics, typec, interner) {
            return Ok(());
        }
        let mut current = &mut self.ctx.moves.owners[value];
        let mut path = bumpvec![];
        let mut current = *loop {
            match current {
                Owner::Temporary(graph) => break graph,
                &mut Owner::Nested(id, header, ..) => {
                    path.push((id, self.ctx.func.types[self.ctx.func.values[header].ty].ty));
                    current = &mut self.ctx.moves.owners[header];
                }
                &mut Owner::Var(var) => break &mut self.ctx.vars[var].graph,
            }
        }
        .get_or_insert_with(|| self.ctx.moves.graphs.push(MoveGraph::Present));

        let mut graph_path = bumpvec![];
        for (id, ty) in path.into_iter().rev() {
            graph_path.push(current);
            current = match self.ctx.moves.graphs[current] {
                MoveGraph::Present => self
                    .expand_graph_node(ty, id, None, current, typec)
                    .ok_or(MoveError::Pointer(span))?,
                MoveGraph::PresentSplit(children) => {
                    self.ctx.moves.graphs[current] = MoveGraph::Partial(children);
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
            match self.ctx.moves.graphs[node] {
                MoveGraph::Present
                | MoveGraph::PresentSplit(..)
                | MoveGraph::Gone(..)
                | MoveGraph::GoneSplit(..) => unreachable!(),
                MoveGraph::Partial(children) => {
                    let collapse = self.ctx.moves.graphs[children]
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
        let old = mem::replace(&mut self.ctx.moves.graphs[addr], new);
        self.ctx.moves.current_branch.insert(addr, old);
        old
    }

    fn expand_graph_node(
        &mut self,
        ty: Ty,
        id: u32,
        gone: Option<Span>,
        root: VRef<MoveGraph>,
        typec: &Typec,
    ) -> Option<VRef<MoveGraph>> {
        let fill = if let Some(gone) = gone {
            MoveGraph::Gone(gone)
        } else {
            MoveGraph::Present
        };

        let traverse_struct = |sel: &mut Self, s: FragRef<Struct>| {
            let children = (0..typec[s].fields.len()).map(|_| fill);
            let slice = sel.ctx.moves.graphs.extend(children);
            sel.change_graph(root, MoveGraph::Partial(slice));
            Some(slice.index(id as usize))
        };

        let traverse_enum = |sel: &mut Self, e: FragRef<Enum>| {
            let children = (0..typec[e].variants.len()).map(|_| fill);
            let slice = sel.ctx.moves.graphs.extend(children);
            sel.change_graph(root, MoveGraph::Partial(slice));
            Some(slice.index(id as usize))
        };

        match ty {
            Ty::Struct(s) => traverse_struct(self, s),
            Ty::Enum(e) => traverse_enum(self, e),
            Ty::Instance(inst) => match typec[inst].base {
                GenericTy::Struct(s) => traverse_struct(self, s),
                GenericTy::Enum(e) => traverse_enum(self, e),
            },
            Ty::Pointer(..) => None,
            Ty::Param(..) | Ty::Builtin(..) => unreachable!(),
        }
    }

    pub fn start_move_frame(&mut self) -> MoveFrame {
        self.ctx.moves.cached_moves.extend(
            self.ctx
                .moves
                .current_branch
                .drain()
                .map(|(graph, old)| Move { graph, old }),
        );
        self.ctx.moves.cached_moves.mark();
        MoveFrame(self.ctx.moves.cached_moves.len())
    }

    pub fn discard_move_branch(&mut self) {
        self.ctx
            .moves
            .current_branch
            .drain()
            .for_each(|(graph, old)| self.ctx.moves.graphs[graph] = old);
        self.ctx.moves.cached_moves.mark();
        self.ctx.moves.terminating_branches.push(true);
    }

    pub fn save_move_branch(&mut self) {
        self.ctx
            .moves
            .cached_moves
            .extend(
                self.ctx
                    .moves
                    .current_branch
                    .drain()
                    .map(|(graph, old)| Move {
                        graph,
                        old: mem::replace(&mut self.ctx.moves.graphs[graph], old),
                    }),
            );
        self.ctx.moves.cached_moves.mark();
        self.ctx.moves.terminating_branches.push(false);
    }

    pub fn end_move_frame(&mut self, frame: MoveFrame) {
        let mut excess = self.ctx.moves.cached_moves.len() - frame.0;
        (0..excess).for_each(|_| self.ctx.moves.cached_moves.join_frames());
        excess -= self
            .ctx
            .moves
            .terminating_branches
            .drain(frame.0..)
            .map(|b| b as usize)
            .sum::<usize>();
        let mut count_map = ShadowMap::new();
        for r#move in self.ctx.moves.cached_moves.pop().collect::<BumpVec<_>>() {
            match (r#move.old, self.ctx.moves.graphs[r#move.graph]) {
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

        self.ctx.moves.current_branch.extend(
            self.ctx
                .moves
                .cached_moves
                .pop()
                .map(|Move { graph, old }| (graph, old)),
        );
    }

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
        let var = self.ctx.vars.push(VarMir { value, graph: None });
        self.ctx.moves.owners[value] = Owner::Var(var);
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

#[derive(Clone, Copy)]
pub struct VarMir {
    pub value: VRef<ValueMir>,
    pub graph: OptVRef<MoveGraph>,
}

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
    Temporary(OptVRef<MoveGraph>),
    Nested(u32, VRef<ValueMir>),
    Var(VRef<VarMir>),
}

impl Default for Owner {
    fn default() -> Self {
        Self::Temporary(None)
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
}

impl MirMoveCtx {
    pub fn clear(&mut self) {
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
    pub vars: PushMap<VarMir>,
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
        self.vars[unsafe { var.cast() }]
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

    pub fn start_frame(&self) -> MirFrame {
        MirFrame(self.vars.len())
    }

    pub fn end_frame(&mut self, frame: MirFrame) {
        self.vars.truncate(frame.0);
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

#[must_use]
pub struct MirFrame(usize);

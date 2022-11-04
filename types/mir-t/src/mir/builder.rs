use std::{
    collections::hash_map::Entry,
    default::default,
    mem,
    ops::{Deref, DerefMut},
};

use lexing_t::Span;

use storage::*;
use typec_t::*;

use crate::*;

macro_rules! present_graph {
    () => {
        MoveGraph::Present | MoveGraph::Partial(.., 0)
    };
}

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
                    path.push((id, self.ctx.func.types[self.ctx.func.values[value].ty].ty));
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
            graph_path.push((id, current));
            match self.ctx.moves.graphs[current] {
                MoveGraph::Present => return,
                MoveGraph::Gone(span) => {
                    current = match self.expand_graph_node(ty, id, Some(span), root, typec) {
                        Some(graph) => graph,
                        None => return,
                    }
                }
                MoveGraph::Partial(children, ..) => current = children.index(id as usize),
            }
        }

        let mut gone = bumpvec![];
        let mut frontier = bumpvec![root];
        while let Some(node) = frontier.pop() {
            match self.ctx.moves.graphs[node] {
                present_graph!() => (),
                MoveGraph::Gone(..) => gone.push(node),
                MoveGraph::Partial(children, ref mut count) => {
                    if children.len() as u32 == *count {
                        gone.push(node);
                    }

                    *count = 0;
                    frontier.extend(children.keys());
                }
            }
        }

        for (.., node) in graph_path.into_iter().rev() {
            match self.ctx.moves.graphs[node] {
                present_graph!() => unreachable!(),
                MoveGraph::Gone(..) => unreachable!(),
                MoveGraph::Partial(.., ref mut count) => {
                    *count -= 1;
                    if *count != 0 {
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
        let mut root = *loop {
            match current {
                Owner::Temporary(graph) => break graph,
                &mut Owner::Nested(id, header, ..) => {
                    path.push((id, self.ctx.func.types[self.ctx.func.values[value].ty].ty));
                    current = &mut self.ctx.moves.owners[header];
                }
                &mut Owner::Var(var) => break &mut self.ctx.vars[var].graph,
            }
        }
        .get_or_insert_with(|| self.ctx.moves.graphs.push(MoveGraph::Present));

        for (id, ty) in path.into_iter().rev() {
            root = match self.ctx.moves.graphs[root] {
                present_graph!() => self
                    .expand_graph_node(ty, id, None, root, typec)
                    .ok_or(MoveError::Pointer(span))?,
                g @ MoveGraph::Gone(..) => return Err(MoveError::Graph(g)),
                MoveGraph::Partial(children, ref mut count) => {
                    *count += 1;
                    if children.len() as u32 == *count {}
                    children.index(id as usize)
                }
            }
        }

        let g = mem::replace(&mut self.ctx.moves.graphs[root], MoveGraph::Gone(span));
        let present_graph!() = g else {
            return Err(MoveError::Graph(g));
        };

        self.ctx.moves.current_branch.insert(Move {
            graph: root,
            direction: MoveDirection::Out,
        });

        Ok(())
    }

    pub fn expand_graph_node(
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
        let mut traverse_struct = |s: FragRef<Struct>| {
            let children = (0..typec[s].fields.len()).map(|_| fill);
            let slice = self.ctx.moves.graphs.extend(children);
            let missing = if gone.is_some() { slice.len() - 1 } else { 1 };
            self.ctx.moves.graphs[root] = MoveGraph::Partial(slice, missing as u32);
            Some(slice.index(id as usize))
        };

        match ty {
            Ty::Struct(s) => traverse_struct(s),
            Ty::Enum(_) => todo!(),
            Ty::Instance(inst) => match typec[inst].base {
                GenericTy::Struct(s) => traverse_struct(s),
                GenericTy::Enum(_) => todo!(),
            },
            Ty::Pointer(..) => None,
            Ty::Param(..) | Ty::Builtin(..) => unreachable!(),
        }
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

pub enum MoveError {
    Graph(MoveGraph),
    Pointer(Span),
}

#[derive(Clone, Copy)]
pub enum MoveGraph {
    Present,
    Gone(Span),
    Partial(VSlice<MoveGraph>, u32),
}

#[derive(Clone, Copy)]
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

pub enum MoveFrame {
    Split(VSlice<VSlice<VRef<ValueMir>>>),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum MoveDirection {
    In,
    Out,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Move {
    pub graph: VRef<MoveGraph>,
    pub direction: MoveDirection,
}

#[derive(Default)]
pub struct MirMoveCtx {
    pub cached_moves: Frames<Move>,
    pub owners: ShadowMap<ValueMir, Owner>,
    pub graphs: PushMap<MoveGraph>,
    pub frames: Vec<MoveFrame>,
    pub branch_moves: PushMap<VSlice<Move>>,
    pub current_branch: Set<Move>,
    pub brach_move_slices: PushMap<Move>,
}

impl MirMoveCtx {
    pub fn clear(&mut self) {
        self.owners.clear();
        self.graphs.clear();
        self.frames.clear();
        self.branch_moves.clear();
        self.current_branch.clear();
        self.brach_move_slices.clear();
    }
}

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

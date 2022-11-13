use std::{
    default::default,
    iter, mem,
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

    pub fn pointer_to(
        &mut self,
        value: VRef<ValueMir>,
        mutability: Mutability,
        span: Span,
        typec: &mut Typec,
        interner: &mut Interner,
    ) -> VRef<ValueMir> {
        let ty = self.value_ty(value);
        let ptr_ty = typec.pointer_to(mutability, ty, interner).into();
        let dest = self.value(ptr_ty, typec);
        self.ctx.func.set_referenced(value);
        self.inst(InstMir::Ref(value, dest), span);
        dest
    }

    pub fn drop_value(
        &mut self,
        value: VRef<ValueMir>,
        span: Span,
        typec: &mut Typec,
        interner: &mut Interner,
    ) {
        let ty = self.value_ty(value);
        match ty.is_drop(&self.ctx.generics, typec, interner) {
            Some(Some(drop_impl)) => {
                let drop_fn = typec[typec[drop_impl].methods][0];
                let mut params = bumpvec![None; typec.pack_func_param_specs(drop_fn).count()];
                typec
                    .compatible(&mut params, ty, typec[drop_impl].key.ty)
                    .unwrap();
                let params = params.into_iter().collect::<Option<BumpVec<_>>>().unwrap();
                let value = self.pointer_to(value, Mutability::Mutable, span, typec, interner);
                let call = CallMir {
                    callable: CallableMir::Func(drop_fn),
                    params: self.ctx.project_ty_slice(&params, typec),
                    args: self.ctx.func.value_args.extend([value]),
                };
                let call = self.ctx.func.calls.push(call);
                self.inst(InstMir::Call(call, None), span);
            }
            Some(None) => (),
            None => {
                self.inst(InstMir::MayDrop(value), span);
                return;
            }
        }

        match ty.base(typec).as_generic() {
            Some(GenericTy::Struct(s)) => {
                let field_len = typec[s].fields.len();
                for i in 0..field_len {
                    let field = self.value(ty.component_ty(i, typec, interner).unwrap(), typec);
                    self.inst(InstMir::Field(value, i as u32, field), span);
                    self.drop_value(field, span, typec, interner);
                }
            }
            Some(GenericTy::Enum(e)) => {
                let variant_len = typec[e].variants.len();
                let Some(last_variant) = variant_len.checked_sub(1) else {
                    return;
                };
                let mut ret_block = None;
                for i in 0..last_variant {
                    let flag_ty = typec.enum_flag_ty(e).unwrap().into();
                    let enum_flag = self.value(flag_ty, typec);
                    self.inst(InstMir::Field(value, 0, enum_flag), span);
                    let variant = self.value(ty.component_ty(i, typec, interner).unwrap(), typec);
                    self.inst(InstMir::Field(value, 1, variant), span);
                    let lit = self.value(flag_ty, typec);
                    self.inst(InstMir::Int(i as i64, lit), span);
                    let cond = self.value(Ty::BOOL, typec);
                    let call = CallMir {
                        callable: CallableMir::Func(flag_ty.int_eq().unwrap()),
                        params: default(),
                        args: self.ctx.func.value_args.extend([enum_flag, lit]),
                    };
                    let call = self.ctx.func.calls.push(call);
                    self.inst(InstMir::Call(call, Some(cond)), span);
                    let cond_block = self.ctx.create_block();
                    let next_block = self.ctx.create_block();
                    self.close_block(span, ControlFlowMir::Split(cond, cond_block, next_block));
                    self.select_block(cond_block);
                    self.drop_value(variant, span, typec, interner);
                    let &mut return_block =
                        ret_block.get_or_insert_with(|| self.ctx.create_block());
                    self.close_block(span, ControlFlowMir::Goto(return_block, None));
                    self.select_block(next_block);
                }
                let variant = self.value(
                    ty.component_ty(last_variant, typec, interner).unwrap(),
                    typec,
                );
                self.inst(InstMir::Field(value, 1, variant), span);
                self.drop_value(variant, span, typec, interner);
                if let Some(return_block) = ret_block {
                    self.close_block(span, ControlFlowMir::Goto(return_block, None));
                    self.select_block(return_block);
                }
            }
            None => (),
        }
    }

    pub fn move_in(
        &mut self,
        value: VRef<ValueMir>,
        span: Span,
        typec: &mut Typec,
        interner: &mut Interner,
    ) -> Result<(), MoveError> {
        if self.ctx.untracked_moves {
            return Ok(());
        }

        let ty = self.ctx.value_ty(value);

        if ty.is_copy(&self.ctx.generics, typec, interner) {
            return Ok(());
        }

        let mut current = value;
        let mut path = bumpvec![];
        let root = loop {
            match self.ctx.moves.owners[current] {
                Owner::Direct(graph) => break graph,
                Owner::Indirect(id, header) => {
                    path.push((id, self.ctx.func.value_ty(header)));
                    current = header;
                }
            }
        };

        let Some(root) = root else {
            self.drop_value(value, span, typec, interner);
            return Ok(());
        };

        let mut graph_path = bumpvec![];
        let mut current = root;
        for (id, ty) in path.into_iter().rev() {
            graph_path.push(current);
            current = match self.ctx.moves.graphs[current] {
                MoveGraph::Present | MoveGraph::PresentSplit(..) => {
                    self.drop_value(value, span, typec, interner);
                    return Ok(());
                }
                MoveGraph::Gone(span) => {
                    match self.ctx.expand_graph_node(ty, id, Some(span), root, typec) {
                        Some(Some(graph)) => graph,
                        Some(None) => graph_path.pop().unwrap(),
                        None => {
                            graph_path.clear();
                            break;
                        }
                    }
                }
                MoveGraph::GoneSplit(children) => {
                    self.ctx.change_graph(current, MoveGraph::Partial(children));
                    children.index(id as usize)
                }
                MoveGraph::Partial(children, ..) => children.index(id as usize),
            }
        }

        let mut frontier = bumpvec![(current, value)];
        while let Some((node, value)) = frontier.pop() {
            match self.ctx.moves.graphs[node] {
                MoveGraph::Present | MoveGraph::PresentSplit(..) => {
                    self.drop_value(value, default(), typec, interner);
                }
                MoveGraph::Gone(..) => {
                    self.ctx.change_graph(node, MoveGraph::Present);
                }
                MoveGraph::GoneSplit(children) | MoveGraph::Partial(children) => {
                    self.assert_not_drop(value, node, typec, interner)?;
                    self.ctx
                        .change_graph(node, MoveGraph::PresentSplit(children));
                    if let MoveGraph::Partial(..) = self.ctx.moves.graphs[node] {
                        frontier.extend(children.keys().enumerate().map(|(i, k)| {
                            (
                                k,
                                match self.ctx.moves.graphs[k] {
                                    MoveGraph::Present | MoveGraph::PresentSplit(..) => {
                                        let header_ty = self.ctx.value_ty(value);
                                        let field = self.ctx.value(
                                            header_ty.component_ty(i, typec, interner).unwrap(),
                                            typec,
                                        );
                                        self.inst(
                                            InstMir::Field(value, i as u32, field),
                                            default(),
                                        );
                                        field
                                    }
                                    _ => ValueMir::UNIT,
                                },
                            )
                        }));
                    } else {
                        frontier.extend(children.keys().map(|k| (k, ValueMir::UNIT)));
                    }
                }
            }
        }

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
                        self.ctx
                            .change_graph(node, MoveGraph::PresentSplit(children));
                    } else {
                        break;
                    }
                }
            }
        }

        Ok(())
    }

    pub fn start_enum_frame(&self) -> EnumFrame {
        EnumFrame {
            base: self.ctx.moves.enums.len(),
        }
    }

    pub fn end_enum_frame(
        &mut self,
        frame: EnumFrame,
        span: Span,
        typec: &mut Typec,
        interner: &mut Interner,
    ) {
        let mut enum_moves = mem::take(&mut self.ctx.moves.enums);
        for EnumMove { value, inner_value } in enum_moves.drain(frame.base..) {
            if let Owner::Direct(graph) = self.ctx.moves.owners[inner_value] {
                match graph.map(|g| self.ctx.moves.graphs[g]) {
                    None | Some(MoveGraph::Present | MoveGraph::PresentSplit(..)) => {
                        self.move_in(value, span, typec, interner).unwrap();
                    }
                    _ => (),
                }
            }
        }
        self.ctx.moves.enums = enum_moves;
    }

    pub fn assert_not_drop(
        &mut self,
        value: VRef<ValueMir>,
        graph: VRef<MoveGraph>,
        typec: &mut Typec,
        interner: &mut Interner,
    ) -> Result<(), MoveError> {
        if let Some(Some(..)) = self
            .value_ty(value)
            .is_drop(&self.ctx.generics, typec, interner)
        {
            return Err(MoveError::DropGraph(self.ctx.moves.graphs[graph]));
        }

        Ok(())
    }

    pub fn inst(&mut self, kind: InstMir, span: Span) -> Option<()> {
        self.current_block?;
        self.ctx.insts.push((kind, span));
        Some(())
    }

    pub fn value(&mut self, ty: Ty, typec: &Typec) -> VRef<ValueMir> {
        self.ctx.value(ty, typec)
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

#[derive(Clone, Copy)]
pub struct VarMir {
    pub value: VRef<ValueMir>,
}

impl CtxFrameItem for VarMir {
    type Ctx = MirBuilderCtx;
    ctx_frame_seq_getters! { |ctx| ctx.vars }
}

#[derive(Debug)]
pub enum MoveError {
    Graph(MoveGraph),
    DropGraph(MoveGraph),
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
    pub branches: Vec<OptVRef<BlockMir>>,
    pub cached_moves: Frames<Move>,
    pub owners: ShadowMap<ValueMir, Owner>,
    pub graphs: PushMap<MoveGraph>,
    pub current_branch: Map<VRef<MoveGraph>, MoveGraph>,
    pub brach_move_slices: PushMap<Move>,
    pub enums: Vec<EnumMove>,
    meta: ShadowMap<MoveGraph, MoveMeta>,
    drop_masks: PushMap<bool>,
    values_to_drop: Vec<(VRef<ValueMir>, VRef<MoveGraph>)>,
}

impl MirMoveCtx {
    pub fn clear(&mut self) {
        self.branches.clear();
        self.cached_moves.clear();
        self.owners.clear();
        self.graphs.clear();
        self.current_branch.clear();
        self.brach_move_slices.clear();
    }

    fn prepare_for_branch_drops(&mut self) {
        self.meta.clear();
        self.drop_masks.clear();
        self.values_to_drop.clear();
    }
}

#[must_use]
pub struct MoveFrame {
    base: usize,
    branch_base: usize,
}

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
    pub fn value(&mut self, ty: Ty, typec: &Typec) -> VRef<ValueMir> {
        if ty == Ty::UNIT {
            return ValueMir::UNIT;
        }
        if ty == Ty::TERMINAL {
            return ValueMir::TERMINAL;
        }

        let ty = self.project_ty(ty, typec);
        self.func.values.push(ValueMir { ty })
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
        self.moves.clear();

        let mut cln = self.func.clone();
        cln.generics = cln.ty_params.extend(self.generic_types.drain(..));
        self.func.clear();
        cln
    }

    pub fn value_ty(&self, value: VRef<ValueMir>) -> Ty {
        self.func.value_ty(value)
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
                    self.change_graph(current, MoveGraph::Partial(children));
                    children.index(id as usize)
                }
                g @ MoveGraph::Gone(..) => return Err(MoveError::Graph(g)),
                MoveGraph::GoneSplit(children) | MoveGraph::Partial(children) => {
                    children.index(id as usize)
                }
            }
        }

        let mut frontier = bumpvec![current];
        while let Some(node) = frontier.pop() {
            match self.moves.graphs[node] {
                MoveGraph::Present => {
                    self.change_graph(node, MoveGraph::Gone(span));
                }
                MoveGraph::PresentSplit(children) => {
                    self.change_graph(node, MoveGraph::GoneSplit(children));
                    frontier.extend(children.keys());
                }
                MoveGraph::Gone(..) | MoveGraph::GoneSplit(..) | MoveGraph::Partial(..) => {
                    return Err(MoveError::Graph(self.moves.graphs[node]))
                }
            }
        }

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
        MoveFrame {
            base: self.moves.cached_moves.len(),
            branch_base: self.moves.branches.len(),
        }
    }

    pub fn discard_move_branch(&mut self) {
        self.moves
            .current_branch
            .drain()
            .for_each(|(graph, old)| self.moves.graphs[graph] = old);
        self.moves.cached_moves.mark();
        self.moves.branches.push(None);
    }

    pub fn save_move_branch(&mut self, current_block: VRef<BlockMir>) {
        self.moves
            .cached_moves
            .extend(self.moves.current_branch.drain().map(|(graph, old)| Move {
                graph,
                old: mem::replace(&mut self.moves.graphs[graph], old),
            }));
        self.moves.cached_moves.mark();
        self.moves.branches.push(Some(current_block));
    }

    pub fn end_move_frame(&mut self, frame: MoveFrame) {
        let branch_count = self.moves.cached_moves.len() - frame.base;
        // remove the wrapping mark
        drop(self.moves.cached_moves.pop());
        let mut moves = bumpvec![];
        for i in 0..branch_count {
            moves.extend(self.moves.cached_moves.pop().map(move |elem| (i, elem)));
        }
        let active_branch_count = branch_count
            - self.moves.branches[frame.branch_base..]
                .iter()
                .filter(|b| b.is_some())
                .count();

        self.moves.prepare_for_branch_drops();
        for (branch, r#move) in moves {
            let mut meta = &mut self.moves.meta[r#move.graph];
            match (r#move.old, self.moves.graphs[r#move.graph]) {
                (
                    MoveGraph::Present | MoveGraph::PresentSplit(..),
                    MoveGraph::Gone(..) | MoveGraph::Partial(..) | MoveGraph::GoneSplit(..),
                ) => {
                    if meta.counter == -1 {
                        continue;
                    }
                    meta.counter += 1;
                    if meta.counter == active_branch_count as isize {
                        self.change_graph(r#move.graph, r#move.old);
                    }
                }
                (
                    MoveGraph::Gone(..) | MoveGraph::Partial(..) | MoveGraph::GoneSplit(..),
                    MoveGraph::Present | MoveGraph::PresentSplit(..),
                ) => {
                    if meta.counter >= 0 {
                        meta.mask = self
                            .moves
                            .drop_masks
                            .extend(iter::repeat(false).take(branch_count));

                        meta.counter = -1;
                        self.change_graph(r#move.graph, r#move.old);
                    } else {
                        self.moves.drop_masks[meta.mask][branch] = true;
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

#[derive(Default, Clone)]
pub struct MoveMeta {
    counter: isize,
    mask: VSlice<bool>,
}

pub struct EnumMove {
    pub value: VRef<ValueMir>,
    pub inner_value: VRef<ValueMir>,
}

#[must_use]
pub struct EnumFrame {
    base: usize,
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

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
        self.ctx.owners[dest] = OwnerMir::Nested(index, header);
        Some(())
    }

    pub fn value_ty(&self, value: VRef<ValueMir>) -> Ty {
        self.ctx.func.types[self.ctx.func.values[value].ty].ty
    }

    pub fn r#move(
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

        let mut current = &mut self.ctx.owners[value];
        let mut path = bumpvec![];
        let mut root = loop {
            match current {
                OwnerMir::Temporary(graph) => {
                    break *graph
                        .get_or_insert_with(|| self.ctx.move_slices.push(MoveGraphMir::Present))
                }
                &mut OwnerMir::Nested(id, header) => {
                    path.push((id, self.ctx.func.types[self.ctx.func.values[value].ty].ty));
                    current = &mut self.ctx.owners[header];
                }
                &mut OwnerMir::Var(var) => break self.ctx.vars[var].graph,
            }
        };

        let traverse_struct =
            |sel: &mut Self, s: FragRef<Struct>, id: u32, root: VRef<MoveGraphMir>| {
                let children = (0..typec[s].fields.len()).map(|_| MoveGraphMir::Present);
                let slice = sel.ctx.move_slices.extend(children);
                sel.ctx.move_slices[root] = MoveGraphMir::Partial(slice);
                slice.index(id as usize)
            };

        for (id, ty) in path {
            root = match self.ctx.move_slices[root] {
                MoveGraphMir::Present => match ty {
                    Ty::Struct(s) => traverse_struct(self, s, id, root),
                    Ty::Enum(_) => todo!(),
                    Ty::Instance(inst) => match typec[inst].base {
                        GenericTy::Struct(s) => traverse_struct(self, s, id, root),
                        GenericTy::Enum(_) => todo!(),
                    },
                    Ty::Pointer(..) => return Err(MoveError::Pointer(span)),
                    Ty::Param(..) | Ty::Builtin(..) => unreachable!(),
                },
                g @ MoveGraphMir::Gone(..) => return Err(MoveError::Graph(g)),
                MoveGraphMir::Partial(children) => children.index(id as usize),
            }
        }

        let g = mem::replace(&mut self.ctx.move_slices[root], MoveGraphMir::Gone(span));
        let MoveGraphMir::Present = g else {
            return Err(MoveError::Graph(g));
        };

        Ok(())
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
        let graph = self.ctx.move_slices.push(MoveGraphMir::Present);
        let var = self.ctx.vars.push(VarMir { value, graph });
        self.ctx.owners[value] = OwnerMir::Var(var);
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
    pub graph: VRef<MoveGraphMir>,
}

pub enum MoveError {
    Graph(MoveGraphMir),
    Pointer(Span),
}

#[derive(Clone, Copy)]
pub enum MoveGraphMir {
    Present,
    Gone(Span),
    Partial(VSlice<MoveGraphMir>),
}

#[derive(Clone, Copy)]
pub enum OwnerMir {
    Temporary(OptVRef<MoveGraphMir>),
    Nested(u32, VRef<ValueMir>),
    Var(VRef<VarMir>),
}

impl Default for OwnerMir {
    fn default() -> Self {
        Self::Temporary(None)
    }
}

pub struct CachedMove {
    pub graph: VRef<MoveGraphMir>,
}

pub enum MoveFrame {
    Split(VSlice<VSlice<CachedMove>>),
}

#[derive(Default)]
pub struct MirBuilderCtx {
    pub untracked_moves: bool,
    pub func: FuncMirInner,
    pub dd: DebugData,
    pub vars: PushMap<VarMir>,
    pub generics: Vec<FragSlice<Spec>>,
    pub move_slices: PushMap<MoveGraphMir>,
    pub current_branch: Vec<CachedMove>,
    pub move_frames: Vec<MoveFrame>,
    pub branch_moves: PushMap<VSlice<CachedMove>>,
    pub brach_move_slices: PushMap<CachedMove>,
    pub owners: ShadowMap<ValueMir, OwnerMir>,
    pub args: Vec<VRef<ValueMir>>,
    pub insts: Vec<(InstMir, Span)>,
    pub used_types: Map<Ty, VRef<MirTy>>,
    pub just_compiled: Vec<FragRef<Func>>,
    pub generic_types: Vec<VRef<MirTy>>,
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
        self.move_slices.clear();
        self.owners.clear();
        self.generics.clear();
        self.brach_move_slices.clear();
        self.branch_moves.clear();

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

use std::{iter, mem, default::default};

use diags::*;
use lexing_t::*;
use mir_t::*;
use storage::*;
use typec_t::*;

use crate::*;

impl MirChecker<'_, '_> {
    pub fn enum_move_out(
        &mut self,
        value: VRef<ValueMir>,
        inner_value: VRef<ValueMir>,
        span: Span,
    ) {
        todo!();
    }

    pub fn move_out(&mut self, value: VRef<ValueMir>, span: Span) {
        let mut missing = bumpvec![];
        let graph = self.mir_move_ctx.mapping[value.index()];
        match self.mir_move_ctx.graphs[graph].kind {
            MoveGraphKind::Present(PresentKind::BehindDrop) => todo!(),
            MoveGraphKind::Present(PresentKind::BehindPointer) => todo!(),
            _ => (),
        }
        self.take_graph(graph, span, &mut missing);
        self.handle_move_error("cannot move out from incomplete value", span, missing)
    }

    fn handle_frozen_move(&mut self, span: Span, graph: VRef<MoveGraph>) {
        let path = {
            let mut current = Some(graph);
            let mut path = bumpvec![];
            while let Some(graph) = current {
                path.push(self.mir_move_ctx.graphs[graph].index);
                current = self.mir_move_ctx.graphs[graph].parent;
            }
            path
        };

        let conflicting_ty = {
            let mut current = self.value_ty(self.mir_move_ctx.graphs[graph].owner);
            for index in path {
                current = current.component_ty(index as usize, self.typec, self.interner).unwrap();
            }
            current
        };

        match conflicting_ty {
            Ty::Struct(_) => todo!(),
            Ty::Enum(_) => todo!(),
            Ty::Instance(_) => todo!(),
            Ty::Pointer(_) => todo!(),
            Ty::Param(_) => todo!(),
            Ty::Builtin(_) => todo!(),
        }
    }

    pub fn move_in(&mut self, dest: VRef<ValueMir>, span: Span) {
        let graph = self.mir_move_ctx.mapping[dest.index()];
        self.fill_graph(graph, span);
    }

    fn handle_move_error(
        &mut self,
        message: &str,
        loc: Span,
        mut missing: BumpVec<Span>,
    ) {
        if missing.is_empty() {
            return;
        }

        missing.sort_unstable();
        missing.dedup();

        self.workspace.push(Snippet {
            title: annotation!(err: ("{}", message)),
            footer: vec![],
            slices: vec![Some(Slice {
                span: missing
                    .iter()
                    .copied()
                    .reduce(|a, b| a.joined(b))
                    .map_or(loc, |fin| loc.joined(fin)),
                origin: self.source,
                annotations: missing
                    .into_iter()
                    .map(|span| source_annotation!(info[span]: "move out of value"))
                    .chain(iter::once(source_annotation!(err[loc]: "occurred here")))
                    .collect(),
                fold: true,
            })],
            origin: default(),
        });
    }

    pub fn make_owner(&mut self, value: VRef<ValueMir>) {
        let ty = self.value_ty(value);
        let dest = self.mir_move_ctx.graphs.push(MoveGraph {
            index: 0,
            owner: value,
            parent: None,
            drop_meta: DropMeta::No,
            kind: MoveGraphKind::Present(PresentKind::Normal),
        });
        self.make_owner_recur(ty, value, dest);
        self.mir_move_ctx.mapping.push(dest);
    }

    fn make_owner_recur(&mut self, ty: Ty, owner: VRef<ValueMir>, dest: VRef<MoveGraph>) {
        self.mir_move_ctx.graphs[dest].kind = match ty {
            Ty::Pointer(..) | Ty::Builtin(..) => MoveGraphKind::Present(PresentKind::Copy),
            ty if ty.is_copy(&self.mir_ctx.generics, self.typec, self.interner) => {
                MoveGraphKind::Present(PresentKind::Copy)
            }
            ty if let Some(res) = ty.is_drop(&self.mir_ctx.generics, self.typec, self.interner).transpose() => {
                self.mir_move_ctx.graphs[dest].drop_meta = match res {
                    Some(r#impl) => DropMeta::Yes(r#impl),
                    None => DropMeta::Maybe,
                };
                MoveGraphKind::Present(PresentKind::Drop)
            }
            Ty::Struct(s) => self.make_struct_owner(s, ty, owner, dest),
            Ty::Instance(i) => {
                let Instance { base, .. } = self.typec[i];
                match base {
                    GenericTy::Struct(s) => self.make_struct_owner(s, ty, owner, dest),
                    GenericTy::Enum(..) => MoveGraphKind::Present(PresentKind::Normal),
                }
            }
            Ty::Enum(..) | Ty::Param(..) => MoveGraphKind::Present(PresentKind::Normal),
        };
    }

    fn make_struct_owner(
        &mut self,
        s: FragRef<Struct>,
        ty: Ty,
        owner: VRef<ValueMir>,
        dest: VRef<MoveGraph>,
    ) -> MoveGraphKind {
        let len = self.typec[s].fields.len();
        let children = self.mir_move_ctx.graphs.extend(
            iter::repeat(MoveGraph {
                index: 0,
                owner,
                parent: Some(dest),
                drop_meta: DropMeta::No,
                kind: MoveGraphKind::Present(PresentKind::Normal),
            })
            .take(len),
        );

        for (i, key) in children.keys().enumerate() {
            let ty = ty.component_ty(i, self.typec, self.interner).unwrap();
            self.mir_move_ctx.graphs[key].index = i as u32;
            self.make_owner_recur(ty, owner, key);
        }

        MoveGraphKind::Split(children)
    }

    fn fill_graph(&mut self, graph: VRef<MoveGraph>, span: Span) {
        self.drop_graph(graph, span);
        self.fill_graph_children(graph);
    }

    fn drop_graph(&mut self, graph: VRef<MoveGraph>, span: Span) {
        let check_point = self.check_point();
        let root_value = self.access_graph(graph, span);
        let dropped_something = self.drop_graph_low(root_value, graph, span);
        if !dropped_something {
            self.rollback(check_point);
        }
    }

    fn drop_graph_low(&mut self, value: VRef<ValueMir>, graph: VRef<MoveGraph>, span: Span) -> bool {
        match self.mir_move_ctx.graphs[graph].kind {
            MoveGraphKind::Present(PresentKind::Copy) => false,
            MoveGraphKind::Present(..) => self.translate_drop_meta(graph, value, span),
            MoveGraphKind::Gone(span) => false,
            MoveGraphKind::Split(children) => {
                let mut dropped = self.translate_drop_meta(graph, value, span);
                for (i, child) in children.keys().enumerate() {
                    let check_point = self.check_point();
                    let ty = self.value_ty(value).component_ty(i, self.typec, self.interner).unwrap();
                    let Some(child_value) = self.value(ty) else { continue };
                    self.inst(InstMir::Field(value, i as u32, child_value), span);
                    let dropped_field = self.drop_graph_low(child_value, child, span);
                    if !dropped_field {
                        self.rollback(check_point);
                    }
                    dropped |= dropped_field;
                }
                dropped
            },
        }
    }

    fn translate_drop_meta(&mut self, graph: VRef<MoveGraph>, value: VRef<ValueMir>, span: Span) -> bool {
        match self.mir_move_ctx.graphs[graph].drop_meta {
            DropMeta::No => false,
            DropMeta::Maybe => {
                self.inst(InstMir::MayDrop(value), span);
                true
            }
            DropMeta::Yes(r#impl) => {
                self.inst(InstMir::Drop(value, r#impl), span);
                true
            }
        }
    }

    fn access_graph(&mut self, graph: VRef<MoveGraph>, span: Span) -> VRef<ValueMir> {
        let value_path = {
            let mut current = Some(graph);
            let mut path = bumpvec![];
            while let Some(current_graph) = current {
                let graph_ent = self.mir_move_ctx.graphs[current_graph];
                path.push(graph_ent.index);
                current = graph_ent.parent;
            }
            path
        };

        let mut current = self.mir_move_ctx.graphs[graph].owner;
        for index in value_path.into_iter().rev() {
            let current_ty = self.value_ty(current);
            let next_ty = current_ty.component_ty(index as usize, self.typec, self.interner).unwrap();
            let Some(next) = self.value(next_ty) else { continue };
            self.inst(InstMir::Field(current, index, next), span);
            current = next;
        }
        current
    }

    fn fill_graph_children(&mut self, graph: VRef<MoveGraph>) {
        let mut frontier = bumpvec![graph];
        while let Some(graph) = frontier.pop() {
            match &mut self.mir_move_ctx.graphs[graph].kind {
                MoveGraphKind::Present(..) => (),
                &mut MoveGraphKind::Gone(span) => {
                    if let Some(SwappedMove::Present) = self
                        .mir_move_ctx
                        .swaps
                        .insert(graph, SwappedMove::Gone(span))
                    {
                        self.mir_move_ctx.swaps.remove(graph);
                    };
                }
                MoveGraphKind::Split(children) => {
                    frontier.extend(children.keys());
                }
            }
        }
    }

    fn take_graph(&mut self, graph: VRef<MoveGraph>, span: Span, missing: &mut BumpVec<Span>) {
        let mut frontier = bumpvec![graph];
        while let Some(graph) = frontier.pop() {
            match &mut self.mir_move_ctx.graphs[graph].kind {
                MoveGraphKind::Present(PresentKind::Copy) => (),
                s @ MoveGraphKind::Present(..) => {
                    if let Some(SwappedMove::Gone(..)) =
                        self.mir_move_ctx.swaps.insert(graph, SwappedMove::Present)
                    {
                        self.mir_move_ctx.swaps.remove(graph);
                    };
                    *s = MoveGraphKind::Gone(span);
                }
                MoveGraphKind::Split(children) => {
                    frontier.extend(children.keys());
                }
                &mut MoveGraphKind::Gone(span) => missing.push(span),
            }
        }
    }

    fn check_point(&mut self) -> MirCheckPoint {
        MirCheckPoint {
            current: self.current_block,
            current_insts: self.mir_ctx.insts.len(),
            func_check_point: self.mir_ctx.func.check_point(),
        }
    }

    fn rollback(&mut self, check_point: MirCheckPoint) {
        if self.current_block != check_point.current {
            self.current_block = check_point.current;
            self.mir_ctx.insts.clear();
            if let Some(block) = check_point.current {
                let insts = mem::take(&mut self.mir_ctx.func.blocks[block].insts);
                let spanned_insts = insts.keys().map(|inst| self.mir_ctx.dd.instr_spans[inst]);
                self.mir_ctx.insts.extend(
                    self.mir_ctx.func.insts[insts]
                        .iter()
                        .take(check_point.current_insts)
                        .copied()
                        .zip(spanned_insts),
                );
            }
        } else {
            self.mir_ctx.insts.truncate(check_point.current_insts);
        }
        self.mir_ctx.func.rollback(check_point.func_check_point);
    }
}

struct MirCheckPoint {
    current: OptVRef<BlockMir>,
    current_insts: usize,
    func_check_point: MirFuncCheckPoint,
}

#[derive(Default)]
pub struct MirMoveCtx {
    graphs: PushMap<MoveGraph>,
    mapping: Vec<VRef<MoveGraph>>,

    swaps: SparseMap<MoveGraph, SwappedMove>,
    cached_swaps: Frames<(VRef<MoveGraph>, SwappedMove)>,
}

impl MirMoveCtx {
    pub fn clear(&mut self) {
        self.graphs.clear();
    }

    pub fn save_move_branch(&mut self) {
        self.cached_swaps
            .extend(self.swaps.iter().map(|(graph, &op)| (graph, op)));
        self.swaps.clear();
    }
}

#[derive(Clone, Copy)]
struct MoveGraph {
    index: u32,
    drop_meta: DropMeta,
    parent: OptVRef<MoveGraph>,
    owner: VRef<ValueMir>,
    kind: MoveGraphKind,
}

#[derive(Clone, Copy)]
enum DropMeta {
    Yes(FragRef<Impl>),
    No,
    Maybe,
}

#[derive(Clone, Copy)]
enum SwappedMove {
    Present,
    Gone(Span),
}

#[derive(Clone, Copy)]
enum MoveGraphKind {
    Present(PresentKind),
    Gone(Span),
    Split(VSlice<MoveGraph>),
}

#[derive(Clone, Copy)]
enum PresentKind {
    Copy,
    Pointer,
    BehindPointer,
    Normal,
    Drop,
    BehindDrop,
}

use std::{
    collections::BTreeMap,
    default::default,
    hash::Hash,
    iter,
    mem::{self, MaybeUninit},
    ops::Range,
};

use diags::*;
use lexing_t::*;
use mir_t::*;
use storage::*;
use typec_t::*;

use crate::*;

pub type BranchBlock = Result<VRef<BlockMir>, bool>;

/*
    # Validating Move State
    We cannot move out of value twice or move partially moved value. We need to
    drop all values that are not moved. We cannot move out of pointer, we cannot
    move out of droppable value. Any value marked copy is not tracked.

    ## Algorithm
    if we generated temporary value:
        - drop it unless moved already
    elif we performed invalid move:
        - report error
    elif we moved value:
        - check the range of the move
        if value is moved of value is partially moved:
            - report error
            - clear the range of the move
        - insert path to move map
    elif we restored value:
        - drop previous value
        - remove move range from index
    elif we shadowed a variable at the same level:
        - drop previous value
    elif we closed a scope:
        - drop all variables
    else:
        - do nothing


    # Conditionals
    If we have exclusive branches, we need to drop things conditionally and
    correctly restore ownership from move ins. Terminating branch should not affect
    outer move state.

    ## Algorithm
    for each branch:
        - validate move state
        - if not terminating:
            cache branch moves
    for each disjoint move out:
        - consider outer value moved out by branch
        - attach drops to all branches that don't move out the value
    if for all branches ode moves in a value:
        - consider outer value moved in by branch

    # Loops
    Moved out values at the end of the loop block are considered double moves.
    Break or continue should also trigger drops up to loop block.

    ## Body Algorithm
    - validate move state in loop body
    for each move out at the end of body:
        - report double move

*/

type MovePath = SmallVec<[MovePathSegment; 4]>;

const _: () = assert!(mem::size_of::<MovePath>() == mem::size_of::<Vec<MovePathSegment>>());

impl MirChecker<'_, '_> {
    pub fn gen_field(
        &mut self,
        value: VRef<ValueMir>,
        dest: OptVRef<ValueMir>,
        field: u32,
        span: Span,
    ) -> VRef<ValueMir> {
        let ty = self
            .value_ty(value)
            .component_ty(field as usize, self.typec, self.interner)
            .unwrap();
        let f_value = dest.unwrap_or_else(|| self.value(ty));
        self.inst(InstMir::Field(value, field, f_value), span);
        self.mir_move_ctx.owners[f_value] = Owner::Indirect(IndirectOwner {
            parent: value,
            index: MovePathSegment::field(field),
            ..self.mir_move_ctx.owners[value].unwrap_or_default()
        });
        f_value
    }

    pub fn connect_deref_owner(&mut self, value: VRef<ValueMir>, deref: VRef<ValueMir>) {
        self.mir_move_ctx.owners[deref] = Owner::Indirect(IndirectOwner {
            parent: value,
            index: MovePathSegment::default(),
            behind_pointer: true,
            inside_droppable: false,
        });
    }

    pub fn store_in_var(&mut self, value: VRef<ValueMir>) {
        if let Owner::Direct { ref mut in_var } = self.mir_move_ctx.owners[value] {
            *in_var = true;
            self.mir_ctx.to_drop.push(value);
        }
    }

    pub fn move_out(&mut self, value: VRef<ValueMir>, span: Span) {
        if self.mir_ctx.no_moves {
            return;
        }

        if self
            .value_ty(value)
            .is_copy(&self.mir_ctx.generics, self.typec, self.interner)
        {
            return;
        }

        let (key, .., err) = self.integrity_check(value);
        if let Some(err) = err {
            match err {
                IntegrityError::InvalidMove(owner) => self.invalid_move(owner, span),
                IntegrityError::Moved(r#move) => self.double_move(r#move.span, span),
                IntegrityError::PartiallyMoved(moves) => {
                    self.handle_partial_move("move out of", moves, span)
                }
            };
            return;
        }

        self.mir_move_ctx.lookup.insert(key.clone(), Move { span });
        self.mir_move_ctx.history.push(MoveRecord {
            key,
            r#move: Move { span },
            direction: MoveDirection::Out,
        });
    }

    pub fn handle_referencing(&mut self, value: VRef<ValueMir>, span: Span) {
        if self.mir_ctx.no_moves {
            return;
        }

        let (key, in_var, err) = self.integrity_check(value);
        if let Some(err) = err {
            match err {
                IntegrityError::InvalidMove(owner) if owner.behind_pointer => return,
                IntegrityError::InvalidMove(..) => None,
                IntegrityError::Moved(r#move) => self.referencing_moved(r#move, span),
                IntegrityError::PartiallyMoved(moves) => {
                    self.handle_partial_move("reference", moves, span)
                }
            };
        };

        if !in_var {
            self.store_in_var(value);
        }

        self.mir_ctx.func.set_referenced(key.root);
    }

    fn integrity_check(
        &mut self,
        value: VRef<ValueMir>,
    ) -> (MoveKey, bool, Option<IntegrityError>) {
        let (key, in_var, owner) = self.get_move_key(value);
        if let Some(owner) = owner {
            return (key, in_var, Some(IntegrityError::InvalidMove(owner)));
        }

        let mut traverse_key = key.clone();
        for _ in 0..traverse_key.path.len() + 1 {
            if let Some(&r#move) = self.mir_move_ctx.lookup.get(&traverse_key) {
                return (key, in_var, Some(IntegrityError::Moved(r#move)));
            }
            traverse_key.path.pop();
        }

        {
            let mut inner = remove_range(
                &mut self.mir_move_ctx.lookup,
                key.range(&mut self.mir_move_ctx.range_temp),
            )
            .map(|(.., m)| m)
            .peekable();

            if inner.peek().is_some() {
                let inner = inner.collect();
                return (key, in_var, Some(IntegrityError::PartiallyMoved(inner)));
            }
        }

        (key, in_var, None)
    }

    pub fn move_in(&mut self, dest: VRef<ValueMir>, span: Span) {
        if self.mir_ctx.no_moves {
            return;
        }

        if self
            .value_ty(dest)
            .is_copy(&self.mir_ctx.generics, self.typec, self.interner)
        {
            return;
        }

        let (mut key, .., None) = self.get_move_key(dest) else {
            self.drop_low(dest, &mut None, span);
            return;
        };

        if self.drop_low(dest, &mut Some(&mut key), span) {
            let r#move = self.mir_move_ctx.lookup.remove(&key).unwrap();
            self.mir_move_ctx.history.push(MoveRecord {
                key,
                r#move,
                direction: MoveDirection::In,
            });
            return;
        };

        self.mir_move_ctx.history.extend(
            remove_range(
                &mut self.mir_move_ctx.lookup,
                key.range(&mut self.mir_move_ctx.range_temp),
            )
            .map(|(key, r#move)| MoveRecord {
                key,
                r#move,
                direction: MoveDirection::In,
            }),
        );
    }

    pub fn drop(&mut self, value: VRef<ValueMir>, span: Span) {
        if self.mir_ctx.no_moves {
            return;
        }

        let (mut key, .., None) = self.get_move_key(value) else {
            self.drop_low(value, &mut None, span);
            return;
        };

        self.drop_low(value, &mut Some(&mut key), span);
    }

    fn drop_low(
        &mut self,
        value: VRef<ValueMir>,
        key: &mut Option<&mut MoveKey>,
        span: Span,
    ) -> bool {
        if let Some(ref mut key) = key && self.mir_move_ctx.lookup.contains_key(key) {
            return true;
        }

        let dropper = |s: &mut Self| {
            s.mir_ctx.func.set_referenced(value);
            let drop = s.mir_ctx.func.drops.push(DropMir { value });
            s.inst(InstMir::Drop(drop), span);
        };

        match self.mir_ctx.value_ty(value) {
            Ty::Pointer(..) | Ty::Builtin(..) => (),
            p if !self.typec.may_need_drop(p, self.interner) => (),
            p if p.is_copy(&self.mir_ctx.generics, self.typec, self.interner) => (),
            p if p
                .is_drop(&self.mir_ctx.generics, self.typec, self.interner)
                .transpose()
                .is_some() =>
            {
                dropper(self)
            }
            // Param is always considered drop if its not copy
            // Both conditions are covered by guards above
            Ty::Param(..) => unreachable!(),
            Ty::Struct(s) => self.partial_struct_drop(s, &[][..], value, key, span),
            // previous branches imply, enum needs drop and we know enums are newer
            // partially moved, thus we can drop the whole thing
            Ty::Enum(..) => dropper(self),
            Ty::Instance(i) => {
                let Instance { base, args } = self.typec[i];
                match base {
                    GenericTy::Struct(s) => self.partial_struct_drop(s, args, value, key, span),
                    GenericTy::Enum(..) => dropper(self),
                }
            }
        }

        false
    }

    fn partial_struct_drop(
        &mut self,
        s: FragRef<Struct>,
        params: impl TypecCtxSlice<Ty>,
        value: VRef<ValueMir>,
        key: &mut Option<&mut MoveKey>,
        span: Span,
    ) {
        for (i, field) in self.typec[self.typec[s].fields]
            .to_bumpvec()
            .into_iter()
            .enumerate()
        {
            let ty = self.typec.instantiate(field.ty, params, self.interner);
            if !self.typec.may_need_drop(ty, self.interner) {
                continue;
            }
            let field_value = self.value(ty);
            self.inst(InstMir::Field(value, i as u32, field_value), span);
            if let Some(key) = key {
                key.path.push(MovePathSegment::field(i as u32));
            }
            self.drop_low(field_value, key, span);
            if let Some(key) = key {
                key.path.pop();
            }
        }
    }

    fn get_move_key(&self, value: VRef<ValueMir>) -> (MoveKey, bool, Option<IndirectOwner>) {
        let mut path = smallvec![];
        let mut root = value;
        let mut barrier = None;
        let in_var = loop {
            let owner = match self.mir_move_ctx.owners[root] {
                Owner::Indirect(owner) => owner,
                Owner::Direct { in_var } => break in_var,
            };
            if owner.behind_pointer || owner.inside_droppable {
                barrier.get_or_insert(owner);
            }
            root = owner.parent;
            path.push(owner.index);
        };
        path.reverse();
        (MoveKey { path, root }, in_var, barrier)
    }

    pub fn start_branching(&mut self) {
        if self.mir_ctx.no_moves {
            return;
        }
        self.mir_move_ctx.history.mark();
        self.mir_ctx.depth += 1;
    }

    pub fn save_branch(&mut self) {
        if self.mir_ctx.no_moves {
            return;
        }
        self.revert_current_branch();
        self.mir_move_ctx
            .concurrent_history
            .extend(self.mir_move_ctx.history.pop());
        self.mir_move_ctx.history.mark();
        self.mir_move_ctx.concurrent_history.mark();
    }

    pub fn discard_branch(&mut self) {
        if self.mir_ctx.no_moves {
            return;
        }
        self.revert_current_branch();
        self.mir_move_ctx.history.clear_top();
        self.mir_move_ctx.concurrent_history.mark();
    }

    pub fn end_branching(&mut self, blocks: &[BranchBlock], span: Span) {
        if self.mir_ctx.no_moves {
            return;
        }

        self.mir_ctx.depth -= 1;

        self.mir_move_ctx.concurrent_history.join_frames();
        let mut branches = (0..blocks.len())
            .map(|_| {
                self.mir_move_ctx
                    .concurrent_history
                    .pop()
                    .collect::<BumpVec<_>>()
            })
            .collect::<BumpVec<_>>();
        branches.reverse();

        for branch in branches.iter_mut() {
            self.simplify_branch_history(branch);
        }

        let mut outs = self.count_and_cover_branch_moves(blocks, &branches);

        outs.sort_by_key(|record| record.key.path.len());
        self.simplify_branch_history(&mut outs);

        let mut accessed = BTreeMap::<MoveKey, VRef<ValueMir>>::new();
        for (branch_block, branch) in blocks
            .iter()
            .zip(branches.iter())
            .filter_map(|(&block, branch)| Some((block.ok()?, branch)))
        {
            self.apply_branch(branch);
            accessed.clear();
            let drop_block = self.mir_ctx.create_block();
            self.select_block(drop_block);
            for record in outs.iter() {
                let mut key = MoveKey {
                    root: record.key.root,
                    path: default(),
                };

                let mut value = key.root;
                for &segment in record.key.path.iter() {
                    key.path.push(segment);
                    if let Some(&accessed) = accessed.get(&key) {
                        value = accessed;
                    } else {
                        let ty = self
                            .value_ty(value)
                            .component_ty(segment.as_index(), self.typec, self.interner)
                            .unwrap();
                        let next_value = self.value(ty);
                        self.inst(
                            InstMir::Field(value, segment.as_index() as u32, next_value),
                            span,
                        );
                        value = next_value;
                    }
                }
                self.drop_low(value, &mut Some(&mut key), span);
            }

            if self.current_block == Some(drop_block) && self.mir_ctx.insts.is_empty() {
                self.mir_ctx.func.blocks.pop();
            } else {
                self.mir_ctx.close_block(
                    self.current_block.unwrap(),
                    self.mir_ctx.func.blocks[branch_block].control_flow,
                );
                self.mir_ctx.dd.block_closers[drop_block] = span;

                let new_branch_control_flow = ControlFlowMir::Goto(drop_block, ValueMir::UNIT);
                self.increment_block_refcount(new_branch_control_flow);
                self.mir_ctx.func.blocks[branch_block].control_flow = new_branch_control_flow;
            }

            self.revert_branch(branch);
        }

        self.mir_move_ctx.history.extend(outs.iter().cloned());
        self.mir_move_ctx
            .lookup
            .extend(outs.into_iter().map(|record| (record.key, record.r#move)));

        self.promote_branch_move_ins(blocks);
    }

    fn promote_branch_move_ins(&mut self, blocks: &[BranchBlock]) {
        let move_in_escape_count = blocks
            .iter()
            .filter(|&&block| block.map_or_else(|err| err, |_| true))
            .count();
        for (key, count) in self.mir_move_ctx.count_temp.drain_filter(|_, _| true) {
            if count != move_in_escape_count {
                continue;
            }

            if let Some(r#move) = self.mir_move_ctx.lookup.remove(&key) {
                self.mir_move_ctx.history.push(MoveRecord {
                    key,
                    r#move,
                    direction: MoveDirection::In,
                });
                continue;
            }

            self.mir_move_ctx.history.extend(
                remove_range(
                    &mut self.mir_move_ctx.lookup,
                    key.range(&mut self.mir_move_ctx.range_temp),
                )
                .map(|(key, r#move)| MoveRecord {
                    key,
                    r#move,
                    direction: MoveDirection::In,
                }),
            );
        }
    }

    fn count_and_cover_branch_moves(
        &mut self,
        blocks: &[BranchBlock],
        branches: &BumpVec<BumpVec<MoveRecord>>,
    ) -> BumpVec<MoveRecord> {
        self.mir_move_ctx.count_temp.clear();
        // self.mir_move_ctx.cover_temp.clear();

        let mut outs = bumpvec![cap branches.iter().map(|b| b.len()).sum::<usize>()];
        for records in branches
            .iter()
            .zip(blocks)
            .filter_map(|(branch, block)| block.ok().map(|_| branch))
        {
            for record in records.iter() {
                match record.direction {
                    MoveDirection::Out => {
                        outs.push(record.clone());
                        // self.mir_move_ctx
                        //     .cover_temp
                        //     .entry(record.key)
                        //     .and_modify(|marked| marked.insert(branch_index))
                        //     .or_insert_with(|| BranchMoveMask::from_index(branch_index));
                    }
                    MoveDirection::In => {
                        self.mir_move_ctx
                            .count_temp
                            .range_mut(record.key.range(&mut self.mir_move_ctx.range_temp))
                            .for_each(|(_, count)| *count += 1);
                        self.mir_move_ctx
                            .count_temp
                            .entry(record.key.clone())
                            .and_modify(|count| *count += 1)
                            .or_insert(1);
                    }
                }
            }
        }

        outs
    }

    fn simplify_branch_history(&mut self, branch: &mut BumpVec<MoveRecord>) {
        self.mir_move_ctx.simplify_temp.clear();
        self.mir_move_ctx
            .simplify_temp
            .extend(branch.iter().filter_map(|r| {
                (self.mir_ctx.value_depths[r.key.root] <= self.mir_ctx.depth)
                    .then(|| (r.key.clone(), ()))
            }));

        branch.reverse();
        branch.retain(|record| {
            if self
                .mir_move_ctx
                .simplify_temp
                .remove(&record.key)
                .is_none()
            {
                false
            } else {
                drop(remove_range(
                    &mut self.mir_move_ctx.simplify_temp,
                    record.key.range(&mut self.mir_move_ctx.range_temp),
                ));
                true
            }
        });
        branch.reverse();
    }

    fn remove_range_from_lookup(&mut self, range_key: &MoveKey) {
        remove_range(
            &mut self.mir_move_ctx.lookup,
            range_key.range(&mut self.mir_move_ctx.range_temp),
        )
        .for_each(drop);
    }

    fn revert_current_branch(&mut self) {
        for record in self.mir_move_ctx.history.top() {
            match record.direction {
                MoveDirection::In => self
                    .mir_move_ctx
                    .lookup
                    .insert(record.key.clone(), record.r#move),
                MoveDirection::Out => self.mir_move_ctx.lookup.remove(&record.key),
            };
        }
    }

    fn revert_branch(&mut self, branch: &[MoveRecord]) {
        for record in branch {
            match record.direction {
                MoveDirection::In => self
                    .mir_move_ctx
                    .lookup
                    .insert(record.key.clone(), record.r#move),
                MoveDirection::Out => self.mir_move_ctx.lookup.remove(&record.key),
            };
        }
    }

    fn apply_branch(&mut self, branch: &[MoveRecord]) {
        for record in branch {
            match record.direction {
                MoveDirection::Out => self
                    .mir_move_ctx
                    .lookup
                    .insert(record.key.clone(), record.r#move),
                MoveDirection::In => {
                    self.remove_range_from_lookup(&record.key);
                    self.mir_move_ctx.lookup.remove(&record.key)
                }
            };
        }
    }

    fn handle_partial_move(
        &mut self,
        message: &str,
        moves: BumpVec<Move>,
        span: Span,
    ) -> Option<!> {
        self.workspace.push(CtlSnippet {
            title: ctl_error_annotation!(err: ("cannot {} partially moved value", message)),
            footer: vec![],
            slices: vec![Some(Slice {
                span: moves
                    .iter()
                    .map(|m| m.span)
                    .reduce(|a, b| a.joined(b))
                    .map_or(span, |fin| span.joined(fin)),
                origin: self.source,
                annotations: moves
                    .into_iter()
                    .map(|r#move| ctl_error_source_annotation!(info[r#move.span]: "move out of value"))
                    .chain(iter::once(ctl_error_source_annotation!(err[span]: "occurred here")))
                    .collect(),
                fold: true,
            })],
            origin: default(),
        });

        None
    }

    pub fn start_loop(&mut self, start: VRef<BlockMir>, dest: VRef<ValueMir>) {
        self.start_loop_branching();
        let frame = self.start_scope_frame();
        self.mir_ctx.loops.push(LoopMir {
            start,
            dest,
            frame,
            end: None,
            depth: self.mir_ctx.depth,
        });
    }

    fn start_loop_branching(&mut self) {
        if self.mir_ctx.no_moves {
            return;
        }

        self.mir_move_ctx.history.mark();
        self.mir_ctx.depth += 1;
    }

    pub fn end_loop(&mut self, span: Span, terminated: bool) -> OptVRef<BlockMir> {
        let r#loop = self
            .mir_ctx
            .loops
            .pop()
            .expect("loop end did not pair up with loop start");
        if !self.mir_ctx.no_moves {
            if !terminated {
                self.check_loop_moves(span, r#loop.depth);
            }
            self.mir_move_ctx.history.join_frames();
            self.mir_ctx.depth -= 1;
        }
        self.end_scope_frame(r#loop.frame, span);
        r#loop.end
    }

    pub fn check_loop_moves(&mut self, span: Span, loop_depth: u32) {
        let mut history = self
            .mir_move_ctx
            .history
            .from_nth(loop_depth as usize)
            .to_bumpvec();
        self.simplify_branch_history(&mut history);

        for record in history.iter() {
            if let MoveDirection::Out = record.direction
                && self.mir_ctx.value_depths[record.key.root] < loop_depth {
                self.loop_double_move(record.r#move, span);
            }
        }
    }

    gen_error_fns! {
        push loop_double_move(self, moved: Move, span: Span) {
            err: "cannot move out of the value, it could have been moved out in a previous iteration of the loop";
            (moved.span.joined(span), self.source) {
                info[moved.span]: "move occurred here";
                err[span]: "in this loop";
            }
        }

        push referencing_moved(self, moved: Move, span: Span) {
            err: "cannot reference moved value";
            (moved.span.joined(span), self.source) {
                info[moved.span]: "first move here";
                err[span]: "referenced later here";
            }
        }

        push no_address(self, span: Span) {
            err: "cannot take pointer of temporary value";
            help: "storing value in variable should help";
            (span, self.source) {
                err[span]: "this value";
            }
        }

        push invalid_move(self, owner: IndirectOwner, span: Span) {
            err: "cannot move out the value";
            info: ("notice that {}", [
                "value is located within datatype that implements 'Drop'",
                "value is behind indirection and does not implement 'Copy'",
            ][owner.behind_pointer as usize]);
            (span, self.source) {
                err[span]: "invalid move occurred here";
            }
        }
    }
}

fn remove_range<'a, K: Ord + Clone, V>(
    map: &'a mut BTreeMap<K, V>,
    range: Range<&K>,
) -> impl Iterator<Item = (K, V)> + 'a {
    let to_remove = map
        .range(range)
        // SAFETY: manual drop ensures that we don't double free
        .map(|(k, _)| unsafe { MaybeUninit::new(std::ptr::read(k)) })
        .collect::<BumpVec<_>>();
    to_remove.into_iter().map(|key| {
        // SAFETY: we know contents of `to_remove` are valid
        let entry = map.remove_entry(unsafe { key.assume_init_ref() });
        // SAFETY: range guarantees that entry is not None
        unsafe { entry.unwrap_unchecked() }
    })
}

enum IntegrityError {
    Moved(Move),
    PartiallyMoved(BumpVec<Move>),
    InvalidMove(IndirectOwner),
}

#[derive(Default)]
pub struct MirMoveCtx {
    lookup: BTreeMap<MoveKey, Move>,
    history: Frames<MoveRecord>,
    concurrent_history: Frames<MoveRecord>,
    owners: ShadowMap<ValueMir, Owner>,

    simplify_temp: BTreeMap<MoveKey, ()>,
    count_temp: BTreeMap<MoveKey, usize>,
    // cover_temp: BTreeMap<MoveKey, BranchMoveMask>,
    range_temp: Range<MoveKey>,
}

impl MirMoveCtx {
    pub fn clear(&mut self) {
        self.lookup.clear();
        self.history.clear();
        self.owners.clear();
    }
}

#[derive(Copy, Clone, Default)]
pub struct IndirectOwner {
    parent: VRef<ValueMir>,
    index: MovePathSegment,
    behind_pointer: bool,
    inside_droppable: bool,
}

#[derive(Copy, Clone)]
pub enum Owner {
    Indirect(IndirectOwner),
    Direct { in_var: bool },
}

impl Owner {
    fn unwrap_or_default(&self) -> IndirectOwner {
        match self {
            Owner::Indirect(owner) => *owner,
            Owner::Direct { .. } => IndirectOwner::default(),
        }
    }
}

impl Default for Owner {
    fn default() -> Self {
        Self::Direct { in_var: false }
    }
}

#[derive(Clone, Debug)]
struct MoveRecord {
    key: MoveKey,
    r#move: Move,
    direction: MoveDirection,
}

#[derive(Copy, Clone, Debug)]
enum MoveDirection {
    In,
    Out,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Default)]
struct MoveKey {
    root: VRef<ValueMir>,
    path: MovePath,
}

impl MoveKey {
    fn range<'a>(&self, dest: &'a mut Range<MoveKey>) -> Range<&'a MoveKey> {
        self.clone_into(&mut dest.start);
        dest.start.path.push(MovePathSegment::START);
        self.clone_into(&mut dest.end);
        dest.end.path.push(MovePathSegment::END);

        &dest.start..&dest.end
    }
}

impl Clone for MoveKey {
    fn clone(&self) -> Self {
        Self {
            root: self.root,
            path: self.path.clone(),
        }
    }

    fn clone_from(&mut self, source: &Self) {
        self.root = source.root;
        self.path.clone_from(&source.path);
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Move {
    span: Span,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct MovePathSegment(u32);

impl MovePathSegment {
    const VARIANT_MASK: u32 = 1 << 31;
    const START: MovePathSegment = MovePathSegment(u32::MIN);
    const END: MovePathSegment = MovePathSegment(u32::MAX);

    // fn variant(index: u32) -> Self {
    //     Self(index | Self::VARIANT_MASK)
    // }

    fn field(index: u32) -> Self {
        Self(index)
    }

    // fn as_field(self) -> Result<u32, u32> {
    //     if self.0 & Self::VARIANT_MASK == 0 {
    //         Ok(self.0)
    //     } else {
    //         Err(self.0 & !Self::VARIANT_MASK)
    //     }
    // }

    fn as_index(self) -> usize {
        (self.0 & !Self::VARIANT_MASK) as usize
    }
}

// #[test]
// fn range_vs_single() {
//     let bt: BTreeSet<_> = (0..10000u16).collect();

//     let now = Instant::now();
//     let mut c = 0;
//     for i in 0..10000 {
//         c += bt.contains(&i) as usize;
//     }
//     println!("{} {:?}", c, now.elapsed());

//     let now = Instant::now();
//     let mut c = 0;
//     for i in 0..10000 - 100 {
//         c += bt.range(i..i + 100).next().is_some() as usize;
//     }
//     println!("{} {:?}", c, now.elapsed());
// }

// #[test]
// fn single_vs_two_pass() {
//     let mut bt: BTreeMap<_, ()> = (0..10000u16).map(|i| ([i].to_vec(), ())).collect();

//     let mut start = vec![0];
//     let mut end = vec![0];
//     let now = Instant::now();
//     for i in (0..10000).step_by(100) {
//         start[0] = i;
//         end[0] = i + 100;
//         remove_range_two_pass(&mut bt, &start..&end);
//     }
//     println!("{} {:?}", bt.len(), now.elapsed());

//     let mut bt: BTreeMap<_, ()> = (0..10000u16).map(|i| ([i].to_vec(), ())).collect();

//     let now = Instant::now();
//     for i in (0..10000).step_by(100) {
//         start[0] = i;
//         end[0] = i + 100;
//         remove_range(&mut bt, &mut start..&mut end);
//     }
//     println!("{} {:?}", bt.len(), now.elapsed());
// }

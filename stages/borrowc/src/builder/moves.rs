use std::{collections::BTreeMap, default::default, mem, ops::Range};

use super::{control_flow::BranchBlock, *};
use crate::ctx::LoopMir;

type MovePath = SmallVec<[MovePathSegment; 4]>;
const _: () = assert!(mem::size_of::<MovePath>() == mem::size_of::<Vec<MovePathSegment>>());

impl<'i, 'm> MirBuilder<'i, 'm> {
    pub(super) fn gen_field(
        &mut self,
        value: VRef<ValueMir>,
        dest: Dest,
        field: u32,
        field_ty: Ty,
        span: Span,
    ) -> VRef<ValueMir> {
        let f_value = dest.move_to_or(|| self.create_value(field_ty));
        self.inst(InstMir::Field(value, field, f_value), span);

        let replacement = Owner::Indirect(IndirectOwner {
            parent: value,
            index: MovePathSegment::field(field),
            ..self.reused.moves.owners[value].indirect_or_default(&self.func)
        });
        let old = mem::replace(&mut self.reused.moves.owners[f_value], replacement);
        if !dest.is_view() {
            self.move_out(f_value, span);
            self.reused.moves.owners[f_value] = old;
        }

        f_value
    }

    pub(super) fn move_in(&mut self, dest: VRef<ValueMir>, span: Span) {
        if self.meta.no_moves {
            return;
        }

        if !self.func.value_ty(dest).may_need_drop(self.ext.types) {
            return;
        }

        let (mut key, .., None) = self.get_move_key(dest) else {
             self.drop_low(dest, &mut None, span);
             return;
         };

        if self.drop_low(dest, &mut Some(&mut key), span) {
            let r#move = self.reused.moves.lookup.remove(&key).unwrap();
            self.reused.moves.history.push(MoveRecord {
                key,
                r#move,
                direction: MoveDirection::In,
            });
            return;
        };

        self.reused.moves.history.extend(
            remove_range(
                &mut self.reused.moves.lookup,
                key.range(&mut self.reused.moves.range_temp),
            )
            .map(|(key, r#move)| MoveRecord {
                key,
                r#move,
                direction: MoveDirection::In,
            }),
        );
    }

    pub(super) fn move_out(&mut self, value: VRef<ValueMir>, span: Span) {
        if self.meta.no_moves {
            return;
        }

        if !self.func.value_ty(value).may_need_drop(self.ext.types) {
            return;
        }

        let (key, .., err) = self.integrity_check(value);
        if let Some(err) = err {
            let loc = self.meta.source_loc(span);
            let msg = "move out of";
            match err {
                IntegrityError::InvalidMove(owner) => {
                    self.ext.workspace.push(InvalidMove { owner, loc })
                }
                IntegrityError::Moved(r#move) => MoveOfMoved {
                    something: msg,
                    r#move,
                    loc,
                }
                .add(self.ext.workspace),
                IntegrityError::PartiallyMoved(moves) => MoveOfPartiallyMoved {
                    something: msg,
                    moves,
                    loc,
                }
                .add(self.ext.workspace),
            };
            return;
        }

        self.reused.moves.lookup.insert(key.clone(), Move { span });
        self.reused.moves.history.push(MoveRecord {
            key,
            r#move: Move { span },
            direction: MoveDirection::Out,
        });
    }

    pub(super) fn drop(&mut self, value: VRef<ValueMir>, span: Span) {
        if self.meta.no_moves {
            return;
        }

        let (mut key, .., None) = self.get_move_key(value) else {
            self.drop_low(value, &mut None, span);
            return;
        };

        self.drop_low(value, &mut Some(&mut key), span);
    }

    pub(super) fn end_scope_frame(&mut self, frame: DropFrame, span: Span) {
        for value in self.reused.drop_from(frame) {
            self.drop(value, span);
        }
    }

    pub(super) fn control_flow_drop(&mut self, base: &DropFrame, span: Span) {
        for value in self.reused.view_drop_from(base) {
            self.drop(value, span)
        }
    }

    pub(super) fn create_value(&mut self, ty: Ty) -> VRef<ValueMir> {
        let val = self.func.create_value(ty, self.reused, self.ext.types);
        self.reused.moves.value_depths[val] = self.depth;
        val
    }

    pub(super) fn handle_referencing(&mut self, value: VRef<ValueMir>, span: Span) {
        if self.meta.no_moves {
            return;
        }

        let (key, in_var, err) = self.integrity_check(value);
        if let Some(err) = err {
            let loc = self.meta.source_loc(span);
            let something = "reference to";
            match err {
                IntegrityError::InvalidMove(owner) if owner.behind_pointer => return,
                IntegrityError::InvalidMove(..) => None,
                IntegrityError::Moved(r#move) => MoveOfMoved {
                    something,
                    r#move,
                    loc,
                }
                .add(self.ext.workspace),
                IntegrityError::PartiallyMoved(moves) => MoveOfPartiallyMoved {
                    something,
                    moves,
                    loc,
                }
                .add(self.ext.workspace),
            };
        };

        if !in_var && self.reused.moves.value_depths[value] == self.depth {
            self.reused.store_in_var(value);
        }

        self.func.module.values[key.root].mark_referenced();
    }

    pub(super) fn connect_deref_owner(&mut self, value: VRef<ValueMir>, deref: VRef<ValueMir>) {
        self.reused.moves.owners[deref] = Owner::Indirect(IndirectOwner {
            parent: value,
            index: MovePathSegment::default(),
            behind_pointer: true,
            inside_droppable: false,
        });
    }

    fn drop_low(
        &mut self,
        value: VRef<ValueMir>,
        key: &mut Option<&mut MoveKey>,
        span: Span,
    ) -> bool {
        if let Some(ref mut key) = key && self.reused.moves.lookup.contains_key(key) {
            return true;
        }

        let dropper = |s: &mut Self| {
            //s.func.module.values[value].mark_referenced();
            let drop = s.func.module.drops.push(DropMir { value });
            s.inst(InstMir::Drop(drop), span);
        };

        match self.func.value_ty(value) {
            Ty::Pointer(..) | Ty::Builtin(..) => (),
            t if !t.may_need_drop(self.ext.types) => (),
            t if self
                .ext
                .creator()
                .is_drop(t, self.func.generics)
                .transpose()
                .is_some() =>
            {
                dropper(self)
            }
            // Param is always considered drop if its not copy
            // Both conditions are covered by guards above
            Ty::Param(..) => unreachable!(),
            Ty::Base(base) => match base {
                BaseTy::Struct(s) => self.partial_struct_drop(s, &[][..], value, key, span),
                BaseTy::Enum(..) => dropper(self),
            },
            Ty::Instance(i) => {
                let Instance { base, args } = self.ext.types[i];
                match base {
                    BaseTy::Struct(s) => self.partial_struct_drop(s, args, value, key, span),
                    BaseTy::Enum(..) => dropper(self),
                }
            }
            Ty::Array(_) => todo!(),
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
        for (i, field) in self.ext.types[self.ext.types[s].fields]
            .to_bumpvec()
            .into_iter()
            .enumerate()
        {
            let ty = self.ext.creator().instantiate(field.ty, params);
            if !ty.may_need_drop(self.ext.types) {
                continue;
            }
            let field_value = self.create_value(ty);
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
            if let Some(&r#move) = self.reused.moves.lookup.get(&traverse_key) {
                return (key, in_var, Some(IntegrityError::Moved(r#move)));
            }
            traverse_key.path.pop();
        }

        {
            let mut inner = remove_range(
                &mut self.reused.moves.lookup,
                key.range(&mut self.reused.moves.range_temp),
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

    fn get_move_key(&self, value: VRef<ValueMir>) -> (MoveKey, bool, Option<IndirectOwner>) {
        let mut path = smallvec![];
        let mut root = value;
        let mut barrier = None;
        let in_var = loop {
            let owner = match self.reused.moves.owners[root] {
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
        if self.meta.no_moves {
            return;
        }
        self.reused.moves.history.mark();
        self.depth += 1;
    }

    pub fn save_branch(&mut self) {
        if self.meta.no_moves {
            return;
        }
        self.revert_current_branch();
        self.reused
            .moves
            .concurrent_history
            .extend(self.reused.moves.history.pop());
        self.reused.moves.history.mark();
        self.reused.moves.concurrent_history.mark();
    }

    pub fn discard_branch(&mut self) {
        if self.meta.no_moves {
            return;
        }
        self.revert_current_branch();
        self.reused.moves.history.clear_top();
        self.reused.moves.concurrent_history.mark();
    }

    pub fn end_branching(&mut self, blocks: &[BranchBlock], span: Span) {
        if self.meta.no_moves {
            return;
        }

        self.depth -= 1;

        self.reused.moves.concurrent_history.join_frames();
        let mut branches = (0..blocks.len())
            .map(|_| {
                self.reused
                    .moves
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
            .filter_map(|(&block, branch)| Some((block?, branch)))
        {
            self.apply_branch(branch);

            let drop_block = self.func.create_block();
            self.select_block(drop_block);

            self.drop_invalidated_values(&mut accessed, &outs, span);

            if self.block == Some(drop_block) && self.reused.no_insts() {
                self.func.module.blocks.pop();
            } else {
                let cf = self.func.module.blocks[branch_block].control_flow;
                self.func
                    .close_block(self.block.take().unwrap(), span, cf, self.reused);
                // TODO: debug info

                let new_branch_control_flow = ControlFlowMir::Goto {
                    dest: drop_block,
                    ret: None,
                };
                self.func.increment_block_refcount(new_branch_control_flow);
                self.func.module.blocks[branch_block].control_flow = new_branch_control_flow;
            }

            self.revert_branch(branch);
        }

        self.reused.moves.history.extend(outs.iter().cloned());
        self.reused
            .moves
            .lookup
            .extend(outs.into_iter().map(|record| (record.key, record.r#move)));

        self.promote_branch_move_ins(blocks);
        self.reused.moves.count_temp.clear();
    }

    fn drop_invalidated_values(
        &mut self,
        accessed: &mut BTreeMap<MoveKey, VRef<ValueMir>>,
        outs: &[MoveRecord],
        span: Span,
    ) {
        accessed.clear();
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
                    continue;
                }

                let ty = self
                    .ext
                    .creator()
                    .component_ty(self.func.value_ty(value), segment.as_index())
                    .unwrap();
                let next_value = self.create_value(ty);
                self.inst(
                    InstMir::Field(value, segment.as_index() as u32, next_value),
                    span,
                );
                value = next_value;
            }
            self.drop_low(value, &mut Some(&mut key), span);
        }
    }

    fn promote_branch_move_ins(&mut self, _blocks: &[BranchBlock]) {
        //let move_in_escape_count = blocks.iter().filter(|&&block| block.is_some()).count();
        for (key, count) in self.reused.moves.count_temp.drain_filter(|_, _| true) {
            if count as usize == 0 {
                continue;
            }

            if let Some(r#move) = self.reused.moves.lookup.remove(&key) {
                self.reused.moves.history.push(MoveRecord {
                    key,
                    r#move,
                    direction: MoveDirection::In,
                });
                continue;
            }

            self.reused.moves.history.extend(
                remove_range(
                    &mut self.reused.moves.lookup,
                    key.range(&mut self.reused.moves.range_temp),
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
        let mut outs = bumpvec![cap branches.iter().map(|b| b.len()).sum::<usize>()];
        for records in branches
            .iter()
            .zip(blocks)
            .filter_map(|(branch, block)| block.map(|_| branch))
        {
            for record in records.iter() {
                match record.direction {
                    MoveDirection::Out => {
                        outs.push(record.clone());
                    }
                    MoveDirection::In => {
                        self.reused
                            .moves
                            .count_temp
                            .range_mut(record.key.range(&mut self.reused.moves.range_temp))
                            .for_each(|(_, count)| *count += 1);
                        self.reused
                            .moves
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
        self.reused
            .moves
            .simplify_temp
            .extend(branch.iter().filter_map(|r| {
                (self.reused.moves.value_depths[r.key.root] <= self.depth)
                    .then(|| (r.key.clone(), ()))
            }));

        branch.reverse();
        branch.retain(|record| {
            if self
                .reused
                .moves
                .simplify_temp
                .remove(&record.key)
                .is_none()
            {
                false
            } else {
                drop(remove_range(
                    &mut self.reused.moves.simplify_temp,
                    record.key.range(&mut self.reused.moves.range_temp),
                ));
                true
            }
        });

        self.reused.moves.simplify_temp.clear();

        branch.reverse();
    }

    fn remove_range_from_lookup(&mut self, range_key: &MoveKey) {
        remove_range(
            &mut self.reused.moves.lookup,
            range_key.range(&mut self.reused.moves.range_temp),
        )
        .for_each(drop);
    }

    fn revert_current_branch(&mut self) {
        Self::revert_branch_low(
            self.reused.moves.history.top(),
            &mut self.reused.moves.lookup,
        );
    }

    fn revert_branch(&mut self, branch: &[MoveRecord]) {
        Self::revert_branch_low(branch, &mut self.reused.moves.lookup);
    }

    fn revert_branch_low(branch: &[MoveRecord], lookup: &mut BTreeMap<MoveKey, Move>) {
        for record in branch {
            match record.direction {
                MoveDirection::In => lookup.insert(record.key.clone(), record.r#move),
                MoveDirection::Out => lookup.remove(&record.key),
            };
        }
    }

    fn apply_branch(&mut self, branch: &[MoveRecord]) {
        for record in branch {
            match record.direction {
                MoveDirection::Out => self
                    .reused
                    .moves
                    .lookup
                    .insert(record.key.clone(), record.r#move),
                MoveDirection::In => {
                    self.remove_range_from_lookup(&record.key);
                    self.reused.moves.lookup.remove(&record.key)
                }
            };
        }
    }

    pub fn start_loop(&mut self, start: VRef<BlockMir>, dest: VRef<ValueMir>) {
        self.start_loop_branching();
        let frame = self.start_scope_frame();
        self.reused.push_loop(LoopMir {
            start,
            dest,
            frame,
            end: None,
            depth: self.depth,
        });
    }

    fn start_loop_branching(&mut self) {
        if self.meta.no_moves {
            return;
        }

        self.reused.moves.history.mark();
        self.depth += 1;
    }

    pub fn end_loop(&mut self, loop_span: Span, terminated: bool) -> OptVRef<BlockMir> {
        let r#loop = self.reused.pop_loop();
        if !self.meta.no_moves {
            if !terminated {
                self.check_loop_moves(loop_span, r#loop.depth);
            }
            self.reused.moves.history.join_frames();
            self.depth -= 1;
        }
        self.end_scope_frame(r#loop.frame, loop_span);
        r#loop.end
    }

    pub fn check_loop_moves(&mut self, loop_span: Span, loop_depth: u32) {
        let mut history = self
            .reused
            .moves
            .history
            .from_nth(loop_depth as usize)
            .to_bumpvec();
        self.simplify_branch_history(&mut history);

        for record in history.iter() {
            if let MoveDirection::Out = record.direction
                && self.reused.moves.value_depths[record.key.root] < loop_depth {
                MovedInLoop {
                    loc: self.meta.source_loc(record.r#move.span),
                    loop_span,
                }.add(self.ext.workspace);
            }
        }
    }
}

#[derive(Default)]
pub struct MoveCtx {
    lookup: BTreeMap<MoveKey, Move>,
    history: Frames<MoveRecord>,
    concurrent_history: Frames<MoveRecord>,
    owners: ShadowMap<ValueMir, Owner>,
    // we use map because of remove_range
    simplify_temp: BTreeMap<MoveKey, ()>,
    count_temp: BTreeMap<MoveKey, u32>,
    range_temp: Option<Range<MoveKey>>,
    value_depths: ShadowMap<ValueMir, u32>,
}

impl MoveCtx {
    pub(crate) fn mark_var(&mut self, value: VRef<ValueMir>) -> bool {
        if let Owner::Direct { ref mut in_var } = self.owners[value] {
            *in_var = true;
            true
        } else {
            false
        }
    }

    pub(crate) fn clear(&mut self) {
        self.lookup.clear();
        self.history.clear();
        assert!(self.concurrent_history.is_empty());
        self.owners.clear();
        assert!(self.simplify_temp.is_empty());
        assert!(self.count_temp.is_empty());
    }
}

#[derive(Copy, Clone)]
pub(crate) struct IndirectOwner {
    parent: VRef<ValueMir>,
    index: MovePathSegment,
    behind_pointer: bool,
    inside_droppable: bool,
}

#[derive(Copy, Clone)]
pub(crate) enum Owner {
    Indirect(IndirectOwner),
    Direct { in_var: bool },
}

impl Owner {
    fn indirect_or_default(self, func: &FuncBorrowcCtx) -> IndirectOwner {
        match self {
            Owner::Indirect(owner) => owner,
            Owner::Direct { .. } => IndirectOwner {
                parent: func.unit,
                index: default(),
                behind_pointer: false,
                inside_droppable: false,
            },
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
struct MoveKey {
    root: VRef<ValueMir>,
    path: MovePath,
}

impl MoveKey {
    fn range<'a>(&self, dest: &'a mut Option<Range<MoveKey>>) -> Range<&'a MoveKey> {
        let dest = dest.get_or_insert_with(|| {
            MoveKey {
                root: self.root,
                path: default(),
            }..MoveKey {
                root: self.root,
                path: default(),
            }
        });

        dest.start.path.clear();
        dest.start.path.extend(self.path.iter().cloned());
        dest.start.path.push(MovePathSegment::START);

        dest.end.path.clear();
        dest.end.path.extend(self.path.iter().cloned());
        dest.end.path.push(MovePathSegment::END);

        &dest.start..&dest.end
    }
}

#[derive(Clone, Copy, Debug)]
struct Move {
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

fn remove_range<'a, K: Ord + Clone, V>(
    map: &'a mut BTreeMap<K, V>,
    range: Range<&K>,
) -> impl Iterator<Item = (K, V)> + 'a {
    let to_remove = map
        .range(range)
        // SAFETY: manual drop ensures that we don't double free
        .map(|(k, _)| unsafe { std::mem::ManuallyDrop::new(std::ptr::read(k)) })
        .collect::<BumpVec<_>>();
    to_remove.into_iter().map(|key| {
        // SAFETY: we know contents of `to_remove` are valid
        let entry = map.remove_entry(&key);
        // SAFETY: range guarantees that entry is not None
        unsafe { entry.unwrap_unchecked() }
    })
}

enum IntegrityError {
    Moved(Move),
    PartiallyMoved(Vec<Move>),
    InvalidMove(IndirectOwner),
}

ctl_errors! {
    #[err => "{something} already moved value"]
    #[note => NO_MOVE_NOTE]
    error MoveOfMoved: fatal {
        #[info loc.origin, r#move.span, "previous move of value"]
        #[err loc]
        something: &'static str,
        r#move: Move,
        loc: SourceLoc,
    }

    #[err => "value is possibly moved more then once"]
    #[note => NO_MOVE_NOTE]
    #[note => "nonlocal value remains moved even after loop jumps back"]
    #[help => "'break' after the move or move the value beck before next iteration"]
    error MovedInLoop: fatal {
        #[info loc.origin, loop_span, "the loop"]
        #[err loc]
        loc: SourceLoc,
        loop_span: Span,
    }

    #[err => [
        "move of value locate inside type that implements 'Drop'",
        "move from behind pointer",
    ][owner.behind_pointer as usize]]
    #[note => NO_MOVE_NOTE]
    #[note => COPY_NOTE]
    error InvalidMove: fatal {
        #[err loc]
        owner: IndirectOwner,
        loc: SourceLoc,
    }
}

const NO_MOVE_NOTE: &str = "you can disable move semantics with '#[no_moves]' function attribute";
const COPY_NOTE: &str = "value does not implement 'Copy' spec";

struct MoveOfPartiallyMoved {
    something: &'static str,
    moves: Vec<Move>,
    loc: SourceLoc,
}

impl CtlError for MoveOfPartiallyMoved {
    fn is_fatal(&self) -> bool {
        true
    }

    fn fill_snippet(&self, snippet: &mut CtlSnippet) {
        let &MoveOfPartiallyMoved {
            something,
            ref moves,
            loc,
        } = self;
        snippet.title = ctl_error_annotation!(err => "{something} partially moved value");
        snippet
            .footer
            .extend([ctl_error_annotation!(note => NO_MOVE_NOTE)]);
        moves
            .iter()
            .filter_map(|r#move| {
                ctl_error_source_annotation!(
             info loc.origin, r#move.span, "move occurs earlier here")
            })
            .collect_into(&mut snippet.source_annotations);
        snippet
            .source_annotations
            .extend(ctl_error_source_annotation!(err loc));
    }
}

use ownership_types::*;
use storage::*;
use typec_types::*;

use crate::OwnershipSolver;

impl OwnershipSolver<'_> {
    pub fn solve(&mut self, func: Func) -> errors::Result {
        let FuncMeta { body, args, .. } = self.funcs[func.meta()];

        if body.is_reserved_value() {
            return Ok(());
        }

        self.o_ctx.clear();

        for &arg in self.tir_data.cons.get(args) {
            self.declare(arg)?;
        }

        self.traverse(body)?;

        self.emit_stack_drops(body)?;

        Ok(())
    }

    fn traverse(&mut self, tir: Tir) -> errors::Result<ID> {
        self.traverse_low(tir, true)
    }

    fn traverse_low(&mut self, tir: Tir, create_scope: bool) -> errors::Result<ID> {
        let id = self.o_ctx.seen[tir];
        if id.is_reserved_value() {
            let id = self.traverse_unchecked(tir, create_scope)?;
            self.o_ctx.seen[tir] = id;
            Ok(id)
        } else {
            Ok(id)
        }
    }

    fn traverse_unchecked(&mut self, root: Tir, create_scope: bool) -> errors::Result<ID> {
        let TirEnt { kind, flags, .. } = self.tir_data.ents[root];

        match kind {
            // TODO control flow
            TirKind::Match(..) | TirKind::MatchBlock(..) => self.declare(root),

            TirKind::If(cond, then, otherwise, ..) => {
                self.move_out(cond)?;
                self.emit_branch_drops(&[then, otherwise], root)?;
                self.declare(root)
            }

            TirKind::Loop(block) => {
                self.o_ctx.mark_frame();
                self.o_ctx.start_loop(root);
                self.o_ctx.currently_accessed.mark_frame();
                self.traverse_low(block, false)?;

                self.o_ctx.end_loop();
                self.o_ctx.pop_frame();

                if !self.tir_data.ents[block]
                    .flags
                    .contains(TirFlags::TERMINATING)
                {
                    self.check_loop_moves(root)?;
                } else {
                    self.o_ctx.currently_accessed.merge_top_frames(1);
                    // we want to keep the frame after merge so we push dummy that is
                    // popped few lines later
                    self.o_ctx.currently_accessed.mark_frame();
                }

                self.emit_stack_drops(block)?;
                self.o_ctx.currently_accessed.pop_frame();
                self.declare(root)
            }

            TirKind::GlobalAccess(..) => self.declare_low(Self::base_id(root), root, true, false),
            TirKind::DerefPointer(value) => {
                self.traverse(value)?;
                self.declare_low(Self::base_id(root), root, true, false)
            }

            TirKind::Access(value, ..) => Ok(Self::base_id(value)),

            TirKind::Continue(loop_header, ..) => {
                let loop_level = self.o_ctx.loop_level(loop_header);
                println!("loop_level: {}", loop_level);
                self.check_loop_moves(loop_header)?;
                self.emit_loop_control_drops(root, loop_level)?;

                Ok(ID::reserved_value())
            }

            TirKind::Break(loop_header, value, ..) => {
                if let Some(value) = value.expand() {
                    self.move_out(value)?;
                }

                let loop_level = self.o_ctx.loop_level(loop_header);
                println!("loop_level: {}", loop_level);
                self.emit_loop_control_drops(root, loop_level)?;

                Ok(ID::reserved_value())
            }

            TirKind::Return(value, ..) => {
                if let Some(value) = value.expand() {
                    self.move_out(value)?;
                }

                self.emit_return_drops(root)?;

                Ok(ID::reserved_value())
            }

            TirKind::Block(stmts, ..) => {
                if create_scope {
                    self.o_ctx.mark_frame();
                }

                if let Some((&last, others)) = self.tir_data.cons.get(stmts).split_last() {
                    for &stmt in others {
                        self.traverse(stmt)?;
                    }

                    self.move_out(last)?;
                    // let span = self.tir_data.ents[last].span;
                    // println!("{} {:?}", span.log(self.sources), self.o_ctx.currently_accessed.top_frame());
                }

                if create_scope {
                    if !flags.contains(TirFlags::TERMINATING) {
                        self.emit_stack_drops(root)?;
                    }
                    self.o_ctx.pop_frame();
                }

                self.declare(root)
            }

            TirKind::Variable(value) | TirKind::BitCast(value) => {
                self.move_out(value)?;
                self.declare_low(Self::base_id(value), value, false, true)
            }

            TirKind::Assign(to, from, ..) => {
                if self.types.may_drop(self.tir_data.ents[to].ty) {
                    self.emit_assign_drops(root, to)?;
                }
                self.move_out(from)?;
                self.move_in(to)?;
                Ok(ID::reserved_value())
            }

            TirKind::FieldAccess(header, field) => {
                let base_id = self.traverse(header)?;

                let field = self.ty_comps[field];
                let id = Self::field_id(base_id, field.index);

                if self.o_ctx.scope.get(id) == OwnershipContext::NO_OWNERSHIP {
                    let ownership = self.o_ctx.scope.get(base_id);

                    let ent = OwnershipEnt {
                        tir: root.into(),
                        ty: field.ty,
                        id,
                        ..self.o_ctx.ownerships[ownership]
                    };
                    self.o_ctx.push_item(self.types, ent);
                };

                Ok(id)
            }

            TirKind::Constructor(args)
            | TirKind::Call(.., args)
            | TirKind::IndirectCall(.., args) => {
                if let TirKind::IndirectCall(ptr, ..) = kind {
                    self.move_out(ptr)?;
                }

                for &arg in self.tir_data.cons.get(args) {
                    self.move_out(arg)?;
                }
                self.declare(root)
            }

            TirKind::IntLit(..)
            | TirKind::BoolLit(..)
            | TirKind::CharLit(..)
            | TirKind::FuncPtr(..) => self.declare(root),

            TirKind::TakePtr(value) => {
                self.ensure_not_moved(value)?;
                self.declare(root)
            }

            // already declared
            TirKind::Argument(_) => Ok(Self::base_id(root)),

            TirKind::LoopInProgress(..) => unreachable!(),
            TirKind::Invalid => unimplemented!(),
        }
    }

    fn emit_branch_drops(&mut self, branches: &[Tir], on: Tir) -> errors::Result {
        let branches = branches
            .iter()
            .copied()
            .filter(|&b| !self.tir_data.ents[b].flags.contains(TirFlags::TERMINATING));
        let branch_count = branches.clone().count();

        for (i, branch) in branches.clone().rev().enumerate() {
            self.o_ctx.mark_current_access_frame_low(i as u32);
            self.move_out(branch)?;
        }

        let accessed = self
            .o_ctx
            .currently_accessed
            .iter_from_frame_inv(branch_count)
            .filter(|access| self.o_ctx.ownerships[access.ownership].last_move.is_some())
            .cloned()
            .collect::<Vec<_>>();

        let mut buffer = vec![];
        for branch in branches.clone() {
            for (i, other) in branches.clone().enumerate() {
                if other == branch {
                    continue;
                }

                buffer.extend_from_slice(self.o_ctx.currently_accessed.nth_frame_inv(i));
            }

            buffer.retain(|access| self.o_ctx.ownerships[access.ownership].last_move.is_some());

            for access in &buffer {
                let ent = &mut self.o_ctx.ownerships[access.ownership];
                ent.last_move.take();
            }

            let frontier = buffer.drain(..).map(|access| access.ownership).collect();
            let drops = self.emit_drops(frontier, on)?;
            self.o_ctx.drops[branch] = self.o_ctx.drop_nodes.join(self.o_ctx.drops[branch], drops);
        }

        self.o_ctx.currently_accessed.merge_top_frames(branch_count);

        for &access in &accessed {
            let ent = &mut self.o_ctx.ownerships[access.ownership];
            ent.last_move = access.tir.into();
        }

        let pre_eval = self
            .o_ctx
            .pre_eval_lists
            .push_iter(accessed.into_iter().map(|access| access.tir));
        self.o_ctx.pre_eval[on] = pre_eval;

        Ok(())
    }

    fn check_loop_moves(&mut self, loop_header: Tir) -> errors::Result {
        let id = self.o_ctx.seen[loop_header];
        let ownership = self.o_ctx.scope.get(id);
        let level = self.o_ctx.ownerships[ownership].loop_level;

        for &Access {
            ownership: accessed,
            ..
        } in self
            .o_ctx
            .currently_accessed
            .iter_from_frame(level as usize)
        {
            let ent = &self.o_ctx.ownerships[accessed];
            if let Some(last_move) = ent.last_move.expand() && ent.loop_level <= level {
                let tir = ent.tir.unwrap();
                let because = self.tir_data.ents[tir].span;
                let loc = self.tir_data.ents[last_move].span;
                self.diagnostics.push(OwError::LoopDoubleMove {
                    because,
                    loc,
                });
                return Err(());
            }
        }
        Ok(())
    }

    fn emit_loop_control_drops(&mut self, on: Tir, loop_level: usize) -> errors::Result {
        let frontier = self.o_ctx.scope.all_items_from_frame(loop_level).collect();
        let drops = self.emit_drops(frontier, on)?;
        self.o_ctx.drops[on] = drops;

        Ok(())
    }

    fn emit_return_drops(&mut self, on: Tir) -> errors::Result {
        let drops = self.emit_drops(self.o_ctx.scope.all_items().collect(), on)?;
        self.o_ctx.drops[on] = drops;

        Ok(())
    }

    fn emit_assign_drops(&mut self, on: Tir, to: Tir) -> errors::Result {
        let id = self.traverse(to)?;
        let ownership = self.o_ctx.scope.get(id);
        let drops = self.emit_drops_low(true, vec![ownership], on)?;
        self.o_ctx.drops[on] = drops;
        Ok(())
    }

    fn emit_stack_drops(&mut self, on: Tir) -> errors::Result {
        let drops = self.emit_drops(self.o_ctx.scope.top_items().collect(), on)?;
        self.o_ctx.drops[on] = self.o_ctx.drop_nodes.join(self.o_ctx.drops[on], drops);
        Ok(())
    }

    fn emit_drops(&mut self, frontier: Vec<Ownership>, on: Tir) -> errors::Result<DropNodeList> {
        self.emit_drops_low(false, frontier, on)
    }

    fn emit_drops_low(
        &mut self,
        assign: bool,
        frontier: Vec<Ownership>,
        _on: Tir,
    ) -> errors::Result<DropNodeList> {
        let mut frontier = frontier
            .into_iter()
            .filter_map(|ownership| {
                let ent = &self.o_ctx.ownerships[ownership];
                (ent.last_move.is_none() && (!ent.behind_pointer || assign)).then(|| {
                    (
                        ownership,
                        self.o_ctx.drop_nodes.push_one(DropNodeEnt {
                            meta: Ok(self.o_ctx.ownerships[ownership].tir.unwrap()),
                            children: DropNodeList::reserved_value(),
                        }),
                    )
                })
            })
            .collect::<Vec<_>>();

        let drops = self.o_ctx.drop_nodes.close_frame();

        let mut field_buffer = vec![];
        while let Some((ownership, drop_node)) = frontier.pop() {
            let OwnershipEnt { ty, id, .. } = self.o_ctx.ownerships[ownership];
            let ty = self.types.base_of(ty);
            let is_dropped = self.types[ty].flags.contains(TyFlags::DROP);

            match self.types[ty].kind {
                TyKind::Struct(fields) => {
                    for (k, &field) in self.ty_comps.get_iter(fields) {
                        let id = Self::field_id(id, field.index);
                        let ownership = self.o_ctx.scope.get(id);
                        if ownership != OwnershipContext::NO_OWNERSHIP {
                            if self.o_ctx.ownerships[ownership].last_move.is_none() {
                                field_buffer.push((ownership, k));
                            } else if is_dropped {
                                // let move_tir = self.o_ctx.ownerships[ownership].last_move.unwrap();
                                // let because = self.tir_data.ents[move_tir].span;
                                // let loc = self.tir_data.ents[on].span;
                                // self.diagnostics.push(OwError::PartiallyMovedDrop {
                                //     because,
                                //     loc,
                                // })
                            }
                        }
                    }

                    let drops =
                        self.o_ctx
                            .drop_nodes
                            .push_iter(field_buffer.iter().map(|&(_, field)| DropNodeEnt {
                                meta: Err(field),
                                children: DropNodeList::reserved_value(),
                            }));

                    frontier.extend(
                        field_buffer
                            .drain(..)
                            .zip(self.o_ctx.drop_nodes.slice_keys(drops))
                            .map(|((ownership, ..), drop)| (ownership, drop)),
                    );

                    self.o_ctx.drop_nodes[drop_node].children = drops;
                }

                TyKind::Param(..)
                | TyKind::Bound(..)
                | TyKind::Ptr(..)
                | TyKind::FuncPtr(..)
                | TyKind::Int(..)
                | TyKind::Uint(..)
                | TyKind::Bool
                | TyKind::Enum(..) => (),

                TyKind::Instance(..) | TyKind::Unresolved => unreachable!(),
            }
            drop(0);
        }

        Ok(drops)
    }

    fn move_in(&mut self, target: Tir) -> errors::Result {
        let id = self.traverse(target)?;

        let mut frontier = vec![id];

        while let Some(id) = frontier.pop() {
            let ownership = self.o_ctx.scope.get(id);

            let ent = &mut self.o_ctx.ownerships[ownership];
            let mut ty = ent.ty;
            if ent.last_move.is_none() {
                continue;
            }

            if ent.level < self.o_ctx.scope.level() as u32 {
                let new = OwnershipEnt {
                    last_move: None.into(),
                    variable: false,
                    ..*ent
                };
                self.o_ctx.push_item(self.types, new);
            } else {
                ent.last_move = None.into();
            }

            if let TyKind::Instance(base, ..) = self.types[ty].kind {
                ty = base;
            }

            match self.types[ty].kind {
                TyKind::Struct(fields) => frontier.extend(
                    self.ty_comps
                        .get(fields)
                        .iter()
                        .map(|f| Self::field_id(id, f.index)),
                ),
                _ if self.types[ty].flags.contains(TyFlags::COPY) => {}
                kind => unimplemented!("{kind:?}"),
            }
        }
        Ok(())
    }

    fn move_out(&mut self, target: Tir) -> errors::Result {
        self.move_out_low(target, target)
    }

    fn move_out_low(&mut self, target: Tir, hint: Tir) -> errors::Result {
        let Some((should_move, ownership)) = self.ensure_not_moved(target)? else {
            return Ok(());
        };

        let ent = &mut self.o_ctx.ownerships[ownership];
        ent.last_move = (should_move).then_some(hint).into();
        if ent.level < self.o_ctx.scope.level() as u32 && self.types.may_drop(ent.ty) {
            self.o_ctx.push_current_access(ownership, hint);
        }

        Ok(())
    }

    fn ensure_not_moved(&mut self, target: Tir) -> errors::Result<Option<(bool, Ownership)>> {
        let id = self.traverse(target)?;

        if id.is_reserved_value() {
            return Ok(None);
        }

        let ownership = self.o_ctx.scope.get(id);

        let ent = &mut self.o_ctx.ownerships[ownership];

        let copy = self.types[ent.ty].flags.contains(TyFlags::COPY);
        if !copy {
            if let Some(because) = ent.last_move.expand() {
                let because = self.tir_data.ents[because].span;
                let loc = self.tir_data.ents[target].span;
                self.diagnostics.push(OwError::DoubleMove { because, loc });
                return Err(());
            }

            if ent.behind_pointer {
                let loc = self.tir_data.ents[target].span;
                self.diagnostics
                    .push(OwError::MoveFromBehindPointer { loc });
                return Err(());
            }
        }

        Ok(Some((!copy, ownership)))
    }

    fn declare(&mut self, tir: Tir) -> errors::Result<ID> {
        self.declare_low(Self::base_id(tir), tir, false, false)
    }

    fn declare_low(
        &mut self,
        id: ID,
        tir: Tir,
        behind_pointer: bool,
        variable: bool,
    ) -> errors::Result<ID> {
        let ownership_ent = OwnershipEnt {
            tir: tir.into(),
            ty: self.tir_data.ents[tir].ty,
            behind_pointer,
            variable,
            id,
            level: u32::MAX,
            ..Default::default()
        };
        self.o_ctx.push_item(self.types, ownership_ent);

        Ok(id)
    }

    fn base_id(tir: Tir) -> ID {
        tir.into()
    }

    fn field_id(id: ID, field_index: u32) -> ID {
        id + ID(field_index as u64)
    }
}

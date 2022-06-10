#![feature(if_let_guard)]
#![feature(let_chains)]
#![feature(let_else)]

pub mod error;
pub mod state;

pub use state::OwnershipSolver;

use ownership_types::*;
use storage::*;
use typec_types::*;

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
        let TirEnt { kind, .. } = self.tir_data.ents[root];

        match kind {
            // TODO control flow
            TirKind::Match(..) | TirKind::MatchBlock(..) => self.declare(root),

            TirKind::If(cond, then, otherwise) => {
                self.move_out(cond)?;
                self.create_branches(&[then, otherwise])?;
                self.declare(root)
            }

            TirKind::Loop(block) => {
                let id = self.declare(root)?;
                self.o_ctx.seen[root] = id;
                self.o_ctx.mark_frame();
                self.o_ctx.currently_accessed.mark_frame();
                self.traverse_low(block, false)?;

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
                Ok(id)
            }

            TirKind::GlobalAccess(..) | TirKind::DerefPointer(..) => {
                self.declare_low(Self::base_id(root), root, true)
            }

            TirKind::Access(value, ..) => Ok(Self::base_id(value)),

            TirKind::Continue(loop_header, ..) => {
                let loop_level = self.level_of(loop_header)?;
                self.check_loop_moves(loop_header)?;
                self.emit_loop_control_drops(root, loop_level)?;

                Ok(ID::reserved_value())
            }

            TirKind::Break(loop_header, value, ..) => {
                if let Some(value) = value.expand() {
                    self.move_out(value)?;
                }

                let loop_level = self.level_of(loop_header)?;
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
                    self.emit_stack_drops(root)?;
                    self.o_ctx.pop_frame();
                }

                self.declare(root)
            }

            TirKind::Variable(value) | TirKind::BitCast(value) => {
                self.move_out(value)?;
                self.declare(value)
            }

            TirKind::Assign(to, from, ..) => {
                self.emit_assign_drops(root, to)?;
                self.move_out(from)?;
                self.move_in(to)?;
                Ok(ID::reserved_value())
            }

            TirKind::FieldAccess(header, field) => {
                let base_id = self.traverse(header)?;
                let field = self.ty_comps[field];
                let id = Self::field_id(base_id, field.index);

                if self.o_ctx.scope.get(id).is_none() {
                    let Some(ownership) = self.o_ctx.scope.get(base_id) else {
                        unreachable!()
                    };

                    let ent = OwnershipEnt {
                        tir: root.into(),
                        ty: field.ty,
                        id,
                        ..self.o_ctx.ownerships[ownership]
                    };
                    self.o_ctx.push_item(ent, true);
                };

                Ok(id)
            }

            TirKind::Constructor(args)
            | TirKind::Call(.., args)
            | TirKind::IndirectCall(.., args) => {
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

    fn create_branches(&mut self, branches: &[Tir]) -> errors::Result {
        for (i, &branch) in branches.iter().enumerate() {
            self.o_ctx.mark_current_access_frame_low(i as u32);
            self.traverse(branch)?;
        }

        let mut buffer = vec![];
        for &branch in branches {
            for (i, &other) in branches.iter().rev().enumerate() {
                if other == branch {
                    continue;
                }

                if self.tir_data.ents[branch]
                    .flags
                    .contains(TirFlags::TERMINATING)
                {
                    continue;
                }

                buffer.extend_from_slice(self.o_ctx.currently_accessed.nth_frame_inv(i));
            }

            let drops = self
                .o_ctx
                .drops_nodes
                .alloc(buffer.len(), Default::default());
            let frontier = buffer
                .drain(..)
                .map(|access| access.ownership)
                .zip(self.o_ctx.drops_nodes.slice_keys(drops))
                .collect();
            self.emit_drops(frontier)?;
            self.o_ctx.drops[branch] = self.o_ctx.drops_nodes.join(self.o_ctx.drops[branch], drops);
        }

        for (i, &branch) in branches.iter().rev().enumerate() {
            if !self.tir_data.ents[branch]
                .flags
                .contains(TirFlags::TERMINATING)
            {
                buffer.extend_from_slice(self.o_ctx.currently_accessed.nth_frame_inv(i));
            }
        }

        for _ in branches {
            self.o_ctx.currently_accessed.pop_frame();
        }

        for item in buffer.drain(..) {
            self.o_ctx.currently_accessed.push(item);
        }

        Ok(())
    }

    fn check_loop_moves(&mut self, loop_header: Tir) -> errors::Result {
        let id = self.o_ctx.seen[loop_header];
        let Some(ownership) = self.o_ctx.scope.get(id) else {
            unreachable!();
        };
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

    fn level_of(&self, root: Tir) -> errors::Result<usize> {
        let Some(ownership) = self.o_ctx.scope.get(Self::base_id(root)) else {
            unreachable!();
        };

        Ok(self.o_ctx.ownerships[ownership].level as usize)
    }

    fn emit_loop_control_drops(&mut self, on: Tir, loop_level: usize) -> errors::Result {
        let drops = self.o_ctx.drops_nodes.alloc(
            self.o_ctx.scope.all_items_from_frame(loop_level).count(),
            Default::default(),
        );
        let iter = self.o_ctx.drops_nodes.slice_keys(drops);
        let frontier = self
            .o_ctx
            .scope
            .all_items_from_frame(loop_level)
            .zip(iter)
            .collect();
        self.emit_drops(frontier)?;

        self.o_ctx.drops[on] = drops;

        Ok(())
    }

    fn emit_return_drops(&mut self, on: Tir) -> errors::Result {
        let drops = self
            .o_ctx
            .drops_nodes
            .alloc(self.o_ctx.scope.all_items().count(), Default::default());
        let iter = self.o_ctx.drops_nodes.slice_keys(drops);
        let frontier = self.o_ctx.scope.all_items().zip(iter).collect();
        self.emit_drops(frontier)?;

        self.o_ctx.drops[on] = drops;

        Ok(())
    }

    fn emit_assign_drops(&mut self, on: Tir, to: Tir) -> errors::Result {
        let id = self.traverse(to)?;
        let drops = self.o_ctx.drops_nodes.alloc(1, Default::default());
        let drop = self.o_ctx.drops_nodes.key_of(drops, 0).unwrap();
        let Some(ownership) = self.o_ctx.scope.get(id) else {
            unreachable!();
        };
        let frontier = vec![(ownership, drop)];

        self.emit_drops(frontier)?;
        self.o_ctx.drops[on] = drops;

        Ok(())
    }

    fn emit_stack_drops(&mut self, on: Tir) -> errors::Result {
        let drops = self
            .o_ctx
            .drops_nodes
            .alloc(self.o_ctx.scope.top_items().count(), Default::default());
        let iter = self.o_ctx.drops_nodes.slice_keys(drops);
        let frontier = self.o_ctx.scope.top_items().zip(iter).collect();
        self.emit_drops(frontier)?;

        self.o_ctx.drops[on] = self.o_ctx.drops_nodes.join(self.o_ctx.drops[on], drops);

        Ok(())
    }

    fn emit_drops(&mut self, mut frontier: Vec<(Ownership, DropNode)>) -> errors::Result {
        while let Some((ownership, drop_node)) = frontier.pop() {
            let OwnershipEnt { mut ty, id, .. } = self.o_ctx.ownerships[ownership];
            if let TyKind::Instance(base, ..) = self.types[ty].kind {
                ty = base;
            }

            let drop = self.o_ctx.ownerships[ownership].last_move.is_none();

            match self.types[ty].kind {
                TyKind::Struct(fields) => {
                    let fields = self.ty_comps.get(fields);
                    let drops = self
                        .o_ctx
                        .drops_nodes
                        .alloc(fields.len(), Default::default());
                    for (field, drop) in fields.iter().zip(self.o_ctx.drops_nodes.slice_keys(drops))
                    {
                        let id = Self::field_id(id, field.index);
                        let ownership = self.o_ctx.scope.get(id).unwrap_or_else(|| {
                            self.o_ctx.push(OwnershipEnt {
                                tir: None.into(),
                                ty: field.ty,
                                ..Default::default()
                            })
                        });
                        frontier.push((ownership, drop));
                    }
                    self.o_ctx.drops_nodes[drop_node] = DropNodeEnt {
                        drop,
                        children: drops,
                    };
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
        }

        Ok(())
    }

    fn move_in(&mut self, target: Tir) -> errors::Result {
        let id = self.traverse(target)?;

        let mut frontier = vec![id];

        while let Some(id) = frontier.pop() {
            let Some(ownership) = self.o_ctx.scope.get(id) else {
                continue;
            };

            let ent = &mut self.o_ctx.ownerships[ownership];
            let mut ty = ent.ty;
            if ent.last_move.is_none() {
                continue;
            }

            if ent.level < self.o_ctx.scope.level() as u32 {
                let new = OwnershipEnt {
                    last_move: None.into(),
                    ..*ent
                };
                self.o_ctx.push_item(new, false);
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
        if ent.level < self.o_ctx.scope.level() as u32 {
            self.o_ctx.push_current_access(ownership, hint);
        }

        Ok(())
    }

    fn ensure_not_moved(&mut self, target: Tir) -> errors::Result<Option<(bool, Ownership)>> {
        let id = self.traverse(target)?;

        if id.is_reserved_value() {
            return Ok(None);
        }

        let Some(ownership) = self.o_ctx.scope.get(id) else {
            unreachable!();
        };

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
        self.declare_low(Self::base_id(tir), tir, false)
    }

    fn declare_low(&mut self, id: ID, tir: Tir, behind_pointer: bool) -> errors::Result<ID> {
        let ownership_ent = OwnershipEnt {
            tir: tir.into(),
            ty: self.tir_data.ents[tir].ty,
            behind_pointer,
            id,
            ..Default::default()
        };
        self.o_ctx.push_item(ownership_ent, false);

        Ok(id)
    }

    fn base_id(tir: Tir) -> ID {
        tir.into()
    }

    fn field_id(id: ID, field_index: u32) -> ID {
        id + ID(field_index as u64)
    }
}

#![feature(if_let_guard)]

pub mod state;

pub use state::OwnershipSolver;

use storage::*;
use typec_types::*;

impl OwnershipSolver<'_> {
    pub fn solve(&mut self, func: Func) -> errors::Result {
        self.o_ctx.clear();

        let FuncMeta { body, args, .. } = self.funcs[func.meta()];

        if body.is_reserved_value() {
            return Ok(());
        }
        
        // TODO: eliminate `to_vec` 
        for arg in self.tir_data.cons.get(args).to_vec() {
            self.spawn(arg)?;
        }

        self.solve_low(body)
    }

    pub fn solve_low(&mut self, root: Tir) -> errors::Result {
        match self.tir_data.ents[root].kind {
            TirKind::Match(value, branching) => {
                self.solve_low(value)?;
                self.solve_low(branching)?;
                self.spawn(root)?;
                Ok(())
            }
            TirKind::MatchBlock(_) => self.spawn(root), // TODO
            TirKind::Loop(block) => {
                self.solve_low(block)?;
                self.spawn(root)?;
                Ok(())
            },

            TirKind::Assign(to, value, ..) => {
                self.solve_low(value)?;
                self.move_in(to)?;
                
                let mut drops = vec![];
                self.emit_drops(to, ID::default(), &mut drops)?;
                let drops = self.tir_data.cons.push(&drops);
                self.tir_data.ents[root].kind = TirKind::Assign(to, value, drops);
                
                self.move_out(value)?;
                
                Ok(())
            }

            TirKind::If(cond, then, otherwise) => {
                self.solve_low(cond)?;

                self.o_ctx.start_branch(&[otherwise]);
                self.solve_low(then)?;
                self.o_ctx.end_branch();
                
                self.o_ctx.start_branch(&[then]);
                self.solve_low(otherwise)?;
                self.o_ctx.end_branch();

                self.o_ctx.pop_branches([then, otherwise].len());

                self.spawn(root)?;

                Ok(())
            }

            TirKind::Break(.., value) | TirKind::Return(value) if value.is_some() => {
                self.solve_low(value.unwrap())?;
                self.move_out(value.unwrap())?;
                Ok(())
            }

            TirKind::Variable(value) => {
                self.solve_low(value)?;
                self.move_out(value)?;
                self.spawn(root)?;
                Ok(())
            }

            TirKind::Block(args, ..) => {
                self.o_ctx.start_scope(root);
                // TODO: eliminate `to_vec`
                
                if let Some((&last, others)) = self.tir_data.cons.get(args).to_vec().split_last() {
                    for &arg in others {
                        self.solve_low(arg)?;
                    }

                    self.solve_low(last)?;
                    if self.tir_data.ents[last].ty == self.tir_data.ents[root].ty
                        && self.tir_data.ents[last].ty != self.builtin_types.nothing {
                        self.move_out(last)?;
                    }
                }
                
                {
                    let mut drops = vec![];
                    // TODO: eliminate `to_vec`
                    for drop in self.o_ctx.to_drop.top_frame().to_vec() {
                        self.emit_drops(drop, drop.into(), &mut drops)?;
                    }
                    let drops = self.tir_data.cons.push(&drops);
                    self.tir_data.ents[root].kind = TirKind::Block(args, drops);
                }
                
                self.o_ctx.end_scope();
                self.spawn(root)?;
                Ok(())
            }
            TirKind::Constructor(args)
            | TirKind::Call(.., args)
            | TirKind::IndirectCall(.., args) => {
                // TODO: eliminate `to_vec` 
                for arg in self.tir_data.cons.get(args).to_vec() {
                    self.solve_low(arg)?;
                    self.move_out(arg)?;
                }
                self.spawn(root)?;
                Ok(())
            }
            TirKind::BitCast(value) => {
                self.solve_low(value)?;
                self.move_out(value)?;
                self.spawn(root)?;
                Ok(())
            }
            
            TirKind::IntLit(..)
            | TirKind::BoolLit(..)
            | TirKind::FuncPtr(..)
            | TirKind::CharLit(..) => self.spawn(root),
        
            TirKind::FieldAccess(header, ..) => self.solve_low(header),

            TirKind::Access(..)
            | TirKind::Return(..)
            | TirKind::TakePtr(..)
            | TirKind::DerefPointer(..)
            | TirKind::GlobalAccess(..)
            | TirKind::Break(..) => Ok(()),

            TirKind::Invalid | TirKind::Argument(..) | TirKind::LoopInProgress(..) => {
                unreachable!()
            }
        }
    }

    fn emit_drops(&mut self, root: Tir, id: ID, buffer: &mut Vec<Tir>) -> errors::Result {
        if self.o_ctx.move_lookup.contains(id) {
            return Ok(());
        }
        
        let root_ty = self.tir_data.ents[root].ty;
        if self.types[root_ty].flags.contains(TyFlags::DROP) {
            buffer.push(root);
        }

        Ok(())
    }

    fn spawn(&mut self, value: Tir) -> errors::Result {
        self.o_ctx.spawned.insert(value);
        self.o_ctx.to_drop.push(value);
        
        Ok(())
    }

    fn move_in(&mut self, value: Tir) -> errors::Result {
        Ok(())
    }

    fn move_out(&mut self, value: Tir) -> errors::Result {
        // println!("=== {}", self.tir_data.ents[value].span.log(self.sources));
        let ty = self.tir_data.ents[value].ty;

        // the call also checks if none of the referenced values are already moved
        let id = self.value_id(value, true, false)?;
        if self.types[ty].flags.contains(TyFlags::COPY) {
            return Ok(());
        };

        self.o_ctx.move_lookup.insert(id);

        Ok(())
    }

    fn value_id(&mut self, value: Tir, move_out: bool, can_deref: bool) -> errors::Result<ID> {
        let id = self.value_id_low(value, move_out, can_deref)?;
        if move_out && self.o_ctx.move_lookup.contains(id) {
            todo!("value already moved {}", self.tir_data.ents[value].span.log(self.sources));
        }
        Ok(id)
    }

    fn value_id_low(&mut self, value: Tir, move_out: bool, can_deref: bool) -> errors::Result<ID> {
        if self.o_ctx.spawned.contains(value) {
            return Ok(value.into());
        }

        let TirEnt { kind, ty, .. } = self.tir_data.ents[value];
        let copy = self.types[ty].flags.contains(TyFlags::COPY) || can_deref;
        match kind {
            TirKind::Access(value, var) => {
                self.value_id(var.expand().unwrap_or(value), move_out, can_deref)
            },
            TirKind::FieldAccess(header, field) => {
                let header_id = self.value_id(header, move_out, copy)?;
                let field_id = self.ty_comps[field].index;
                Ok(ID::owned(header_id, ID(field_id as u64)))
            },
            
            TirKind::GlobalAccess(..) if move_out && !copy => todo!("moving value contained in global state"),
            TirKind::DerefPointer(..) if move_out && !copy => todo!("moving value behind pointer"),
            
            TirKind::TakePtr(..) 
            | TirKind::GlobalAccess(..) 
            | TirKind::DerefPointer(..) => Ok(ID::reserved_value()),

            kind => unimplemented!("{:?} {}", kind, self.tir_data.ents[value].span.log(self.sources)),
        }
    }
}

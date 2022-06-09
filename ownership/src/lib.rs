#![feature(if_let_guard)]
#![feature(let_chains)]

pub mod state;

use ownership_types::Validity;
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
            TirKind::MatchBlock(block) => self.solve_low(block), // TODO
            
            TirKind::Loop(block) => {
                self.o_ctx.start_loop();
                self.solve_low(block)?;
                self.spawn(root)?;
                self.o_ctx.end_loop();
                Ok(())
            },

            TirKind::Assign(to, value, ..) => {
                self.solve_low(value)?;
                let mut drops = vec![];
                self.move_in(to, &mut drops)?;
                
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

            TirKind::Break(_, value, ..) | TirKind::Return(value, ..) if value.is_some() => {
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
            | TirKind::GlobalAccess(..)
            | TirKind::Continue(..)
            | TirKind::Return(..)
            | TirKind::Break(..) => Ok(()),

            | TirKind::DerefPointer(value)
            | TirKind::TakePtr(value) => self.solve_low(value),


            TirKind::Invalid | TirKind::Argument(..) | TirKind::LoopInProgress(..) => {
                unreachable!()
            }
        }
    }

    fn emit_drops(&mut self, root: Tir, id: ID, buffer: &mut Vec<Tir>) -> errors::Result {
        if !self.o_ctx.move_lookup.get(id).map(Validity::is_valid).unwrap_or(false) {
            return Ok(());
        }
        
        let root_ty = self.tir_data.ents[root].ty;
        if self.types[root_ty].flags.contains(TyFlags::DROP) {
            buffer.push(root);
        }

        Ok(())
    }

    fn spawn(&mut self, value: Tir) -> errors::Result {
        self.o_ctx.spawn(value);
        self.o_ctx.to_drop.push(value);
        
        Ok(())
    }

    fn move_in(&mut self, value: Tir, buffer: &mut Vec<Tir>) -> errors::Result {
        let id = self.value_id(value, false, true)?.0;
        self.move_in_low(value, id, buffer)
    }

    fn move_in_low(&mut self, value: Tir, id: ID, buffer: &mut Vec<Tir>) -> errors::Result {
        let TirEnt { ty, span, .. }= self.tir_data.ents[value];
        let TyEnt { mut kind, flags, .. } = self.types[ty];
        if let TyKind::Instance(base, ..) = kind {
            kind = self.types[base].kind;
        }
        
        if flags.contains(TyFlags::COPY) {
            return Ok(());
        }
        self.o_ctx.move_lookup.insert(id, Validity::Valid);
        
        match kind {
            TyKind::Struct(fields) => {
                for (fid, &field) in self.ty_comps.get_iter(fields) {
                    let id = ID::owned(id, ID(field.index as u64));
                    
                    let field_access = || {
                        let kind = TirKind::FieldAccess(value, fid);
                        TirEnt::new(kind, ty, span)
                    };

                    match self.o_ctx.move_lookup.insert(id, Validity::Valid) {
                        Some(Validity::Valid) | None => {
                            let field_access = self.tir_data.ents.push(field_access());
                            self.emit_drops(field_access, id, &mut vec![])?;
                        }
                        Some(Validity::Partial) => {
                            let field_access = self.tir_data.ents.push(field_access());
                            self.move_in_low(field_access, id, buffer)?;
                        }
                        Some(Validity::Invalid) => {
                            println!("poop");
                        },
                    }
                }
            },
            TyKind::Enum(_, _) => todo!(),
            TyKind::Instance(_, _) => todo!(),
            kind => unimplemented!("{kind:?}"),
        }

        Ok(())
    }

    fn move_out(&mut self, value: Tir) -> errors::Result {
        // println!("=== {}", self.tir_data.ents[value].span.log(self.sources));
        let ty = self.tir_data.ents[value].ty;

        // the call also checks if none of the referenced values are already moved
        let (id, root) = self.value_id(value, true, false)?;
        if self.types[ty].flags.contains(TyFlags::COPY) {
            return Ok(());
        };

        let root = self.o_ctx.spawned[root].level as usize;
        if !self.o_ctx.terminating_since(root, self.tir_data) {
            todo!("value can possibly be moved twice in this loop {}", self.tir_data.ents[value].span.log(self.sources));
        }

        self.o_ctx.move_lookup.insert(id, Validity::Invalid);

        Ok(())
    }

    fn value_id(&mut self, value: Tir, move_out: bool, can_deref: bool) -> errors::Result<(ID, Tir)> {
        let res = self.value_id_low(value, move_out, can_deref)?;
        if move_out && !self.o_ctx.move_lookup.get(res.0).map(Validity::is_valid).unwrap_or(true) {
            todo!("value already moved {}", self.tir_data.ents[value].span.log(self.sources));
        }
        Ok(res)
    }
 
    fn value_id_low(&mut self, value: Tir, move_out: bool, can_deref: bool) -> errors::Result<(ID, Tir)> {
        if self.o_ctx.is_spawned(value) {
            return Ok((value.into(), value));
        }

        let TirEnt { kind, ty, .. } = self.tir_data.ents[value];
        let copy = self.types[ty].flags.contains(TyFlags::COPY) || can_deref;
        match kind {
            TirKind::Access(value, var) => {
                self.value_id(var.expand().unwrap_or(value), move_out, can_deref)
            },
            TirKind::FieldAccess(header, field) => {
                let (header_id, root) = self.value_id(header, move_out, copy)?;
                let field_id = self.ty_comps[field].index;
                Ok((ID::owned(header_id, ID(field_id as u64)), root))
            },
            
            TirKind::GlobalAccess(..) if move_out && !copy => todo!("moving value contained in global state"),
            TirKind::DerefPointer(..) if move_out && !copy => todo!("moving value behind pointer"),
            
            TirKind::TakePtr(value) | TirKind::DerefPointer(value) => {
                let (_, value) = self.value_id(value, move_out, can_deref)?;
                Ok((ID::reserved_value(), value)) 
            },
            TirKind::GlobalAccess(..) => Ok((ID::reserved_value(), value)),

            kind => unimplemented!("{:?} {}", kind, self.tir_data.ents[value].span.log(self.sources)),
        }
    }
}

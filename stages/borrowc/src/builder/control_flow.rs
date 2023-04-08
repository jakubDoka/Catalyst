use std::default::default;

use crate::{ctx::LoopMir, patterns::*};

use super::*;

pub(super) type BranchBlock = Option<VRef<BlockMir>>;

impl<'i, 'm> MirBuilder<'i, 'm> {
    pub(super) fn r#loop(&mut self, r#loop: LoopTir, dest: VRef<ValueMir>, span: Span) -> NodeRes {
        let start = self.func.create_block();
        self.start_loop(start, dest);

        self.close_block(
            span,
            ControlFlowMir::Goto {
                dest: start,
                ret: None,
            },
        );
        self.select_block(start);

        let terminated = self.node(r#loop.body, Dest::View).is_none();

        if !terminated {
            self.func.mark_cycle(start);
            self.close_block(
                span,
                ControlFlowMir::Goto {
                    dest: start,
                    ret: None,
                },
            );
        }

        if let Some(end) = self.end_loop(span, terminated) {
            self.select_block(end);
            return Some(dest);
        }

        None
    }

    pub(super) fn r#continue(&mut self, loop_id: VRef<LoopHeaderTir>, span: Span) -> NodeRes {
        let LoopMir { start, depth, .. } = self.reused[loop_id];
        self.check_loop_moves(span, depth);
        self.func.mark_cycle(start);
        self.close_block(
            span,
            ControlFlowMir::Goto {
                dest: start,
                ret: None,
            },
        );
        None
    }

    pub(super) fn r#break(&mut self, BreakTir { loop_id, value }: BreakTir, span: Span) -> NodeRes {
        let ret = value
            .map(|v| self.node(v, Dest::MoveTo(self.reused[loop_id].dest)))
            .transpose()?;
        let &mut end = self.reused[loop_id]
            .end
            .get_or_insert_with(|| self.func.create_block_with_pass(ret));

        // SAFETY: We are not storing the frame in any way for possible missuse
        self.control_flow_drop(&unsafe { self.reused[loop_id].frame.clone() }, span);

        self.close_block(span, ControlFlowMir::Goto { dest: end, ret });
        None
    }

    pub(super) fn r#return(
        &mut self,
        value: Option<&TirNode>,
        span: Span,
    ) -> Option<VRef<ValueMir>> {
        let ret_val = value
            .map(|&val| self.node(val, Dest::MoveTo(self.func.ret)))
            .transpose()?;

        self.control_flow_drop(&DropFrame::BASE, span);

        self.close_block(
            span,
            ControlFlowMir::Return(ret_val.unwrap_or(self.func.unit)),
        );
        None
    }

    pub(super) fn call(
        &mut self,
        CallTir {
            func, params, args, ..
        }: CallTir,
        ty: Ty,
        dest: VRef<ValueMir>,
        span: Span,
    ) -> NodeRes {
        let callable = match func {
            CallableTir::Func(func) => CallableMir::Func(func),
            CallableTir::SpecFunc(bound_func) => CallableMir::SpecFunc(bound_func),
            CallableTir::Pointer(..) => todo!(),
        };

        let params = self.create_params(params);

        let args = args
            .iter()
            .map(|&arg| self.node(arg, Dest::Move))
            .collect::<Option<BumpVec<_>>>()?;

        let call = self.func.call_inst(callable, params, args, dest);
        self.inst(call, span);

        if ty == Ty::TERMINAL {
            self.close_block(span, ControlFlowMir::Terminal);
            return None;
        }

        Some(dest)
    }

    pub(super) fn r#if(
        &mut self,
        IfTir { top, elifs, r#else }: IfTir,
        dest: VRef<ValueMir>,
        span: Span,
    ) -> NodeRes {
        self.start_branching();

        let mut reached = bumpvec![cap 1 + elifs.len() + 1];
        let mut dest_block = None;
        let mut branch = top;
        let mut elif_iter = elifs.iter().copied();
        let current = loop {
            let Some(cond_value) = self.node(branch.cond, Dest::View) else {
                todo!("we need to do a cleanup here");
            };

            let then = self.func.create_block();
            let otherwise = self.func.create_block();

            self.close_block(
                branch.cond.span,
                ControlFlowMir::Split {
                    cond: cond_value,
                    then,
                    otherwise,
                },
            );
            self.select_block(then);
            reached.push(self.branch(branch.body, &mut dest_block, dest, None));
            self.select_block(otherwise);

            match elif_iter.next() {
                Some(elif) => branch = elif,
                None => break otherwise,
            }
        };

        if let Some(r#else) = r#else {
            reached.push(self.branch(r#else, &mut dest_block, dest, None));
        } else {
            let &mut dest = dest_block.get_or_insert_with(|| self.func.create_block());
            self.close_block(span, ControlFlowMir::Goto { dest, ret: None });
            self.discard_branch();
            reached.push(Some(current));
        }

        self.end_branching(&reached, span);

        if let Some(dest_block) = dest_block {
            self.select_block(dest_block);
        }

        Some(dest)
    }

    pub(super) fn r#match(
        &mut self,
        MatchTir { value, arms }: MatchTir,
        dest: VRef<ValueMir>,
        span: Span,
    ) -> NodeRes {
        let mir_value = self.node(value, Dest::View)?;

        let branch_patterns = {
            let mut branch_nodes = bumpvec![];
            arms.iter()
                .enumerate()
                .filter_map(|(ord, &MatchArmTir { pat, .. })| {
                    branch_nodes.clear();
                    self.pattern_to_branch(pat, &mut branch_nodes);
                    let (&start, rest) = branch_nodes.split_first()?;
                    Some(Branch {
                        start,
                        nodes: self.ext.arena.alloc_slice(rest),
                        ord,
                    })
                })
                .collect::<BumpVec<_>>()
        };

        let mut reachable = bumpvec![false; branch_patterns.len()];
        let tree = patterns::as_tree(self.ext.arena, &branch_patterns, &mut reachable);
        if tree.has_missing {
            let gaps = patterns::find_gaps(tree)
                .into_iter()
                .map(|seq| patterns::display_pat(&seq, self.func.value_ty(mir_value), &self.ext))
                .intersperse(", ".into())
                .collect::<String>();
            NonExhaustive {
                loc: self.meta.source_loc(span),
                gaps,
            }
            .add(self.ext.workspace);
        }

        self.start_branching();

        let arms = arms
            .iter()
            .zip(reachable)
            .filter_map(|(&arm, reachable)| reachable.then_some(arm))
            .collect::<BumpVec<_>>();
        let mut reached = bumpvec![cap arms.len()];
        let (&last, rest) = arms.split_last()?;
        let mut dest_block = None;
        for &arm in rest.iter() {
            let cond = self
                .pattern_to_cond(arm.pat, mir_value)
                .expect("only last pattern can be non refutable");
            let then = self.func.create_block();
            let otherwise = self.func.create_block();
            self.close_block(
                arm.pat.span,
                ControlFlowMir::Split {
                    cond,
                    then,
                    otherwise,
                },
            );
            self.select_block(then);
            reached.push(self.match_arm(arm, &mut dest_block, mir_value, dest));
            self.select_block(otherwise);
        }
        reached.push(self.match_arm(last, &mut dest_block, mir_value, dest));

        self.end_branching(&reached, span);

        let dest_block = dest_block?;
        self.select_block(dest_block);

        Some(dest)
    }

    fn match_arm(
        &mut self,
        MatchArmTir { pat, body }: MatchArmTir,
        dest_block: &mut OptVRef<BlockMir>,
        value: VRef<ValueMir>,
        dest: VRef<ValueMir>,
    ) -> BranchBlock {
        let frame = self.start_scope_frame();
        self.bind_pattern_vars(pat, value);
        self.branch(body, dest_block, dest, Some(frame))
    }

    fn branch(
        &mut self,
        body: TirNode,
        dest_block: &mut OptVRef<BlockMir>,
        dest: VRef<ValueMir>,
        wrapper: Option<DropFrame>,
    ) -> BranchBlock {
        if let ret @ Some(..) = self.node(body, Dest::MoveTo(dest)) {
            if let Some(wrapper) = wrapper {
                self.end_scope_frame(wrapper, body.span);
            }
            let &mut dest = dest_block.get_or_insert_with(|| self.func.create_block_with_pass(ret));
            let block = self.close_block(body.span, ControlFlowMir::Goto { dest, ret });
            self.save_branch();
            block
        } else {
            self.discard_branch();
            if let Some(wrapper) = wrapper {
                self.discard_scope_frame(wrapper);
            }
            None
        }
    }

    pub(super) fn bind_pattern_vars(
        &mut self,
        PatTir {
            kind,
            has_binding,
            ty,
            span,
            ..
        }: PatTir,
        value: VRef<ValueMir>,
    ) {
        if !has_binding {
            return;
        }

        match kind {
            PatKindTir::Unit(unit) => match unit {
                UnitPatKindTir::Struct { fields } => {
                    for (i, &field) in fields
                        .iter()
                        .enumerate()
                        .filter(|(_, field)| field.has_binding)
                    {
                        let dest = self.create_value(field.ty);
                        self.inst(InstMir::Field(value, i as u32, dest), field.span);
                        self.bind_pattern_vars(field, dest);
                    }
                }
                UnitPatKindTir::Binding(mutable, ..) => {
                    let dest = self.create_value(ty);
                    if mutable {
                        self.func.module.values[dest].mark_mutable();
                    }
                    self.move_out(value, span);
                    self.inst(InstMir::Var(value, dest), span);
                    self.func.create_var_from_value(dest, self.reused);
                }
                UnitPatKindTir::Enum {
                    value: Some(&pat), ..
                } => {
                    let dest = self.create_value(pat.ty);
                    // self.enum_move_out(value, dest, span);
                    self.inst(InstMir::Field(value, 1, dest), span);
                    self.bind_pattern_vars(pat, dest);
                }
                UnitPatKindTir::Int(..)
                | UnitPatKindTir::Wildcard
                | UnitPatKindTir::Enum { .. } => unreachable!(),
            },
            PatKindTir::Or(_) => todo!(),
        }
    }

    fn pattern_to_cond(
        &mut self,
        PatTir {
            kind,
            ty,
            is_refutable,
            span,
            ..
        }: PatTir,
        value: VRef<ValueMir>,
    ) -> OptVRef<ValueMir> {
        if !is_refutable {
            return None;
        }

        match kind {
            PatKindTir::Unit(unit) => match unit {
                UnitPatKindTir::Struct { fields } => {
                    let fields = fields
                        .iter()
                        .enumerate()
                        .filter(|(_, field)| field.is_refutable);

                    let mut ret_value = None;
                    for (i, &field) in fields {
                        let dest = self.create_value(field.ty);
                        self.inst(InstMir::Field(value, i as u32, dest), field.span);
                        let Some(val) = self.pattern_to_cond(field, dest) else {
                             continue;
                        };
                        ret_value = match ret_value {
                            Some(other) => Some(self.call_inst(
                                Func::BOOL_BAND,
                                default(),
                                [val, other],
                                Ty::BOOL,
                                span,
                            )),
                            None => Some(val),
                        }
                    }

                    ret_value
                }
                UnitPatKindTir::Int(int) => {
                    let val = self.create_value(ty);
                    let lit = self.number(int.err(), val, int.unwrap_or(span), InstMir::Int)?;
                    Some(self.call_inst(
                        ty.int_eq().unwrap(),
                        default(),
                        [lit, value],
                        Ty::BOOL,
                        int.unwrap_or(span),
                    ))
                }
                UnitPatKindTir::Binding(..) | UnitPatKindTir::Wildcard => None,
                UnitPatKindTir::Enum {
                    id,
                    ty: enum_ty,
                    value: enum_value,
                } => {
                    let enum_flag_ty = self.ext.types.enum_flag_ty(enum_ty);
                    let enum_flag = (enum_flag_ty != Builtin::Unit).then(|| {
                        let dest = self.create_value(Ty::Builtin(enum_flag_ty));
                        self.inst(InstMir::Field(value, 0, dest), span);
                        let const_flag = self.create_value(Ty::Builtin(enum_flag_ty));
                        self.number(Some(id as i64), const_flag, span, InstMir::Int);
                        self.call_inst(
                            Ty::Builtin(enum_flag_ty).int_eq().unwrap(),
                            default(),
                            [dest, const_flag],
                            Ty::BOOL,
                            span,
                        )
                    });

                    let Some(val) = enum_value.and_then(|&pat| {
                         let enum_value = self.create_value(pat.ty);
                         self.inst(InstMir::Field(value, 1, enum_value), span);
                         self.pattern_to_cond(pat, enum_value)
                    }) else {
                         return enum_flag;
                    };

                    match enum_flag {
                        Some(other) => Some(self.call_inst(
                            Func::BOOL_BAND,
                            default(),
                            [val, other],
                            Ty::BOOL,
                            span,
                        )),
                        None => Some(val),
                    }
                }
            },
            PatKindTir::Or(..) => todo!(),
        }
    }

    fn pattern_to_branch(&mut self, PatTir { kind, .. }: PatTir, nodes: &mut BumpVec<NodeRanges>) {
        match kind {
            PatKindTir::Unit(unit) => match unit {
                UnitPatKindTir::Struct { fields } => {
                    for &pat in fields {
                        self.pattern_to_branch(pat, nodes);
                    }
                }
                UnitPatKindTir::Int(value, ..) => {
                    let int = match value {
                        Ok(span) => self
                            .ext
                            .resources
                            .span_str(self.meta.source, span)
                            .parse()
                            .unwrap(),
                        Err(lit) => lit as u128,
                    };
                    nodes.push(NodeRanges::Scalar(Range::at(int)));
                }
                UnitPatKindTir::Binding(..) | UnitPatKindTir::Wildcard => {
                    nodes.push(NodeRanges::Scalar(Range::full()))
                }
                UnitPatKindTir::Enum {
                    id,
                    ty: enum_ty,
                    value,
                } => {
                    nodes.push(NodeRanges::Scalar(
                        if id as usize == self.ext.types[enum_ty].variants.len() - 1 {
                            Range {
                                start: id as u128,
                                end: UpperBound::Outside,
                            }
                        } else {
                            Range::at(id as u128)
                        },
                    ));
                    if let Some(&value) = value {
                        self.pattern_to_branch(value, nodes);
                    }
                }
            },
            PatKindTir::Or(..) => todo!(),
        }
    }

    fn call_inst(
        &mut self,
        callable: impl Into<CallableMir>,
        params: VRefSlice<TyMir>,
        args: impl IntoIterator<Item = VRef<ValueMir>>,
        ret: Ty,
        span: Span,
    ) -> VRef<ValueMir> {
        let ret = self.create_value(ret);
        let inst = self.func.call_inst(callable.into(), params, args, ret);
        self.inst(inst, span);
        ret
    }

    fn close_block(&mut self, span: Span, flow: ControlFlowMir) -> OptVRef<BlockMir> {
        let current = self.block.take()?;
        self.func.increment_block_refcount(flow);
        self.func.close_block(current, span, flow, self.reused);
        Some(current)
    }
}

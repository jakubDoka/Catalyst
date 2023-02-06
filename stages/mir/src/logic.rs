use std::{default::default, iter, mem};

use diags::*;
use lexing_t::Span;
use mir_t::*;
use packaging_t::span_str;
use storage::*;
use typec_t::*;

use crate::{builder::MirBuilder, moves::BranchBlock, *};

pub type NodeRes = OptVRef<ValueMir>;

impl MirChecker<'_, '_> {
    pub fn consts(
        &mut self,
        ctx: &mut MirCtx,
        module_ent: &mut ModuleMir,
        move_ctx: &mut MirMoveCtx,
        module: FragRef<ModuleMir>,
        input: &mut BumpVec<(FragRef<Const>, TirNode)>,
    ) -> &mut Self {
        for (r#const, tir_const) in input.drain(..) {
            let body = self.r#const(module, r#const, tir_const, module_ent, ctx, move_ctx);
            self.mir.bodies.insert(BodyOwner::Const(r#const), body);
            ctx.just_compiled_consts.push(r#const);
        }

        self
    }

    fn r#const(
        &mut self,
        module: FragRef<ModuleMir>,
        r#const: FragRef<Const>,
        body: TirNode,
        module_ent: &mut ModuleMir,
        mir_ctx: &mut MirCtx,
        move_ctx: &mut MirMoveCtx,
    ) -> FuncMir {
        let Const { ty, .. } = self.typec[r#const];

        let mut builder = MirBuilder::new(ty, false, module_ent, mir_ctx, move_ctx, self.typec);

        let frame = self.start_scope_frame();
        let (block, args) = self.push_args(default(), &mut builder);
        self.node(body, None, false, &mut builder);
        self.discard_scope_frame(frame);

        builder.finish(args, module, block)
    }

    pub fn funcs(
        &mut self,
        ctx: &mut MirCtx,
        module_ent: &mut ModuleMir,
        move_ctx: &mut MirMoveCtx,
        module: FragRef<ModuleMir>,
        input: &mut BumpVec<(FragRef<Func>, TirFunc)>,
    ) -> &mut Self {
        for (func, tir_func) in input.drain(..) {
            let body = self.func(module, func, tir_func.body, ctx, module_ent, move_ctx);
            self.mir.bodies.insert(BodyOwner::Func(func), body);
            ctx.just_compiled_funcs.push(func);
        }

        self
    }

    fn func(
        &mut self,
        module: FragRef<ModuleMir>,
        func: FragRef<Func>,
        body: TirNode,
        ctx: &mut MirCtx,
        module_ent: &mut ModuleMir,
        move_ctx: &mut MirMoveCtx,
    ) -> FuncMir {
        let Func {
            signature, flags, ..
        } = self.typec[func];

        let mut builder = MirBuilder::new(
            signature.ret,
            flags.contains(FuncFlags::NO_MOVES),
            module_ent,
            ctx,
            move_ctx,
            self.typec,
        );

        builder.add_generics(self.typec.pack_func_param_specs(func));

        let frame = self.start_scope_frame();
        let (block, args) = self.push_args(signature.args, &mut builder);
        self.node(body, None, false, &mut builder);
        self.discard_scope_frame(frame);

        builder.finish(args, module, block)
    }

    fn node(
        &mut self,
        TirNode { kind, ty, span, .. }: TirNode,
        dest: OptVRef<ValueMir>,
        r#move: bool,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        macro pass($dest:ident => $call:expr) {{
            let $dest = dest.unwrap_or_else(|| self.value(ty));
            $call
        }}

        use TirKind::*;
        match kind {
            Block(stmts) => self.block(stmts, dest, span, r#move, builder),
            Int(computed) => pass!(dest => self.int(computed, span, dest, builder)),
            Float(computed) => pass!(dest => self.float(computed, span, dest, builder)),
            Char => pass!(dest => self.char(span, dest, builder)),
            Bool(value) => pass!(dest => self.bool(value, span, dest, builder)),
            Access(access) => self.access(access, span, dest, r#move, builder),
            Call(&call) => pass!(dest => self.call(call, ty, span, dest, builder)),
            Return(ret) => self.r#return(ret, span, builder),
            Ctor(fields) => self.constructor(fields, ty, span, dest, r#move, builder),
            Deref(&node) => pass!(dest => self.deref(node, dest, span, builder)),
            Ref(&node) => pass!(dest => self.r#ref(node, span, dest, builder)),
            Field(&field) => pass!(dest => self.field(field, span, dest, r#move, builder)),
            Match(&r#match) => pass!(dest => self.r#match(r#match, span, dest, r#move, builder)),
            If(&r#if) => pass!(dest => self.r#if(r#if, dest, span, r#move, builder)),
            Loop(&r#loop) => {
                pass!(dest => self.r#loop(r#loop, dest, span, r#move, builder))
            }
            Continue(loop_id) => self.r#continue(loop_id, span, builder),
            Break(&r#break) => self.r#break(r#break, span, builder),
            Let(&r#let) => self.r#let(r#let, builder),
            Assign(&assign) => self.assign(assign, span, builder),
            ConstAccess(r#const) => {
                pass!(dest => self.r#const_access(dest, r#const, span, builder))
            }
        }
    }

    fn r#loop(
        &mut self,
        r#loop: LoopTir,
        dest: VRef<ValueMir>,
        span: Span,
        r#move: bool,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        let start = builder.create_block();
        self.start_loop(start, dest);

        self.close_block(
            span,
            ControlFlowMir::Goto {
                dest: start,
                ret: None,
            },
        );
        self.select_block(start);

        let terminated = self.node(r#loop.body, None, r#move, builder).is_none();

        if !terminated {
            self.mark_cycle(start);
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

    fn r#continue(
        &mut self,
        loop_id: VRef<LoopHeaderTir>,
        span: Span,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        let LoopMir { start, depth, .. } = builder[loop_id];
        self.check_loop_moves(span, depth);
        self.mark_cycle(start);
        self.close_block(
            span,
            ControlFlowMir::Goto {
                dest: start,
                ret: None,
            },
        );
        None
    }

    fn r#break(
        &mut self,
        BreakTir { loop_id, value }: BreakTir,
        span: Span,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        let LoopMir {
            dest,
            frame:
                MirVarFrame {
                    to_drop: drop_frame,
                    ..
                },
            ..
        } = builder[loop_id];

        let ret = value
            .map(|v| self.node(v, Some(dest), true, builder))
            .transpose()?;
        let &mut end = builder[loop_id]
            .end
            .get_or_insert_with(|| builder.create_block_with_passed(ret));

        builder.control_drop(drop_frame);

        self.close_block(span, ControlFlowMir::Goto { dest: end, ret });
        None
    }

    fn assign(
        &mut self,
        AssignTir { lhs, rhs }: AssignTir,
        span: Span,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        let dest = self.node(lhs, None, false, builder)?;
        self.move_in(dest, span);
        self.node(rhs, Some(dest), true, builder)?;
        Some(builder.uint())
    }

    fn r#let(&mut self, LetTir { pat, value }: LetTir, builder: &mut MirBuilder) -> NodeRes {
        let value = self.node(value, None, false, builder)?;
        self.bind_pattern_vars(pat, value, builder);
        Some(builder.uint())
    }

    fn r#if(
        &mut self,
        IfTir { top, elifs, r#else }: IfTir,
        dest: VRef<ValueMir>,
        span: Span,
        r#move: bool,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        self.start_branching();

        let mut reached = bumpvec![cap 1 + elifs.len() + 1];
        let mut dest_block = None;
        for &IfBranchTir {
            cond: cond_tir,
            body,
        } in iter::once(&top).chain(elifs)
        {
            let cond = self.node(cond_tir, None, true, builder)?;
            let then = builder.create_block();
            let otherwise = builder.create_block();
            self.close_block(
                cond_tir.span,
                ControlFlowMir::Split {
                    cond,
                    then,
                    otherwise,
                },
            );
            self.select_block(then);
            reached.push(self.branch(body, &mut dest_block, dest, None, r#move));
            self.select_block(otherwise);
        }

        if let Some(r#else) = r#else {
            reached.push(self.branch(r#else, &mut dest_block, dest, None, r#move));
        } else {
            let &mut dest = dest_block.get_or_insert_with(|| builder.create_block());
            self.close_block(span, ControlFlowMir::Goto { dest, ret: None });
            self.discard_branch();
            reached.push(Err(true));
        }

        self.end_branching(&reached, span);

        if let Some(dest_block) = dest_block {
            self.select_block(dest_block);
        }

        Some(dest)
    }

    fn r#match(
        &mut self,
        MatchTir { value, arms }: MatchTir,
        span: Span,
        dest: VRef<ValueMir>,
        r#move: bool,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        let mir_value = self.node(value, None, false, builder)?;

        let branch_patterns = arms
            .iter()
            .enumerate()
            .filter_map(|(ord, &MatchArmTir { pat, .. })| {
                let mut branch_nodes = bumpvec![];
                self.pattern_to_branch(pat, &mut branch_nodes);
                let (&start, rest) = branch_nodes.split_first()?;
                Some(Branch {
                    start,
                    nodes: self.arena.alloc_slice(rest),
                    ord,
                })
            })
            .collect::<BumpVec<_>>();

        let mut reachable = bumpvec![false; branch_patterns.len()];
        let tree = patterns::as_tree(self.arena, &branch_patterns, &mut reachable);
        if tree.has_missing {
            let gaps = patterns::find_gaps(tree)
                .into_iter()
                .map(|seq| self.display_pat(&seq, self.value_ty(mir_value)))
                .intersperse(", ".into())
                .collect::<String>();
            self.workspace.push(NonExhaustive {
                loc: SourceLoc {
                    span,
                    origin: self.source,
                },
                gaps,
            });
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
            let then = builder.create_block();
            let otherwise = builder.create_block();
            self.close_block(
                arm.pat.span,
                ControlFlowMir::Split {
                    cond,
                    then,
                    otherwise,
                },
            );
            self.select_block(then);
            reached.push(self.match_arm(arm, &mut dest_block, mir_value, dest, r#move));
            self.select_block(otherwise);
        }
        reached.push(self.match_arm(last, &mut dest_block, mir_value, dest, r#move));

        self.end_branching(&reached, span);

        let dest_block = dest_block?;
        self.select_block(dest_block);

        Some(dest)
    }

    #[allow(clippy::too_many_arguments)]
    fn match_arm(
        &mut self,
        MatchArmTir { pat, body }: MatchArmTir,
        dest_block: &mut OptVRef<BlockMir>,
        value: VRef<ValueMir>,
        dest: VRef<ValueMir>,
        r#move: bool,
        builder: &mut MirBuilder,
    ) -> BranchBlock {
        let frame = self.start_scope_frame();
        self.bind_pattern_vars(pat, value);
        self.branch(body, dest_block, dest, Some(frame), r#move)
    }

    fn branch(
        &mut self,
        body: TirNode,
        dest_block: &mut OptVRef<BlockMir>,
        dest: VRef<ValueMir>,
        wrapper: Option<MirVarFrame>,
        r#move: bool,
        builder: &mut MirBuilder,
    ) -> BranchBlock {
        if let ret @ Some(..) = self.node(body, Some(dest), r#move, builder) {
            if let Some(wrapper) = wrapper {
                self.end_scope_frame(wrapper, body.span);
            }
            let &mut dest = dest_block.get_or_insert_with(|| builder.create_block());
            let block = self.close_block(body.span, ControlFlowMir::Goto { dest, ret });
            builder.module.blocks[dest].passed = ret; // since all rets are either present or absent
            self.save_branch();
            block.ok_or(false)
        } else {
            self.discard_branch();
            if let Some(wrapper) = wrapper {
                self.discard_scope_frame(wrapper);
            }
            Err(false)
        }
    }

    fn bind_pattern_vars(
        &mut self,
        PatTir {
            kind,
            has_binding,
            ty,
            span,
            ..
        }: PatTir,
        value: VRef<ValueMir>,
        builder: &mut MirBuilder,
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
                        let dest = self.value(field.ty);
                        self.inst(InstMir::Field(value, i as u32, dest), field.span);
                        self.bind_pattern_vars(field, dest);
                    }
                }
                UnitPatKindTir::Binding(mutable, ..) => {
                    let dest = self.value(ty);
                    if mutable {
                        builder.module.set_mutable(dest);
                    }
                    self.move_out(value, span);
                    self.inst(InstMir::Var(value, dest), span);
                    self.create_var(dest);
                }
                UnitPatKindTir::Enum {
                    value: Some(&pat), ..
                } => {
                    let dest = self.value(pat.ty);
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
        builder: &mut MirBuilder,
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
                        let dest = self.value(field.ty);
                        self.inst(InstMir::Field(value, i as u32, dest), field.span);
                        let Some(val) = self.pattern_to_cond(field, dest) else {
                            continue;
                        };
                        ret_value = match ret_value {
                            Some(other) => {
                                let call = builder.module.calls.push(CallMir {
                                    callable: CallableMir::Func(Func::BOOL_BAND),
                                    params: default(),
                                    args: builder.module.value_args.extend([val, other]),
                                });
                                let ret = self.value(Ty::BOOL);
                                self.inst(InstMir::Call(call, ret), field.span);
                                Some(ret)
                            }
                            None => Some(val),
                        }
                    }

                    ret_value
                }
                UnitPatKindTir::Int(int) => {
                    let val = self.value(ty);
                    let lit = self.int(int.err(), int.unwrap_or(span), val)?;
                    let call = builder.module.calls.push(CallMir {
                        callable: CallableMir::Func(ty.int_eq().unwrap()),
                        params: default(),
                        args: builder.module.value_args.extend([lit, value]),
                    });
                    let cond = self.value(Ty::BOOL);
                    self.inst(InstMir::Call(call, cond), int.unwrap_or(span));
                    Some(cond)
                }
                UnitPatKindTir::Binding(..) | UnitPatKindTir::Wildcard => None,
                UnitPatKindTir::Enum {
                    id,
                    ty: enum_ty,
                    value: enum_value,
                } => {
                    let enum_flag_ty = self.typec.enum_flag_ty(enum_ty);
                    let enum_flag = (enum_flag_ty != Builtin::Unit).then(|| {
                        let dest = self.value(Ty::Builtin(enum_flag_ty));
                        self.inst(InstMir::Field(value, 0, dest), span);
                        let const_flag = self.value(Ty::Builtin(enum_flag_ty));
                        self.int(Some(id as i64), span, const_flag);
                        let call = builder.module.calls.push(CallMir {
                            callable: CallableMir::Func(
                                Ty::Builtin(enum_flag_ty).int_eq().unwrap(),
                            ),
                            params: default(),
                            args: builder.module.value_args.extend([dest, const_flag]),
                        });
                        let ret = self.value(Ty::BOOL);
                        self.inst(InstMir::Call(call, ret), span);
                        ret
                    });

                    let Some(val) = enum_value.and_then(|&pat| {
                        let enum_value = self.value(pat.ty);
                        self.inst(InstMir::Field(value, 1, enum_value), span);
                        self.pattern_to_cond(pat, enum_value)
                    }) else {
                        return enum_flag;
                    };

                    match enum_flag {
                        Some(other) => {
                            let call = builder.module.calls.push(CallMir {
                                callable: CallableMir::Func(Func::BOOL_BAND),
                                params: default(),
                                args: builder.module.value_args.extend([val, other]),
                            });
                            let ret = self.value(Ty::BOOL);
                            self.inst(InstMir::Call(call, ret), span);
                            Some(ret)
                        }
                        None => Some(val),
                    }
                }
            },
            PatKindTir::Or(..) => todo!(),
        }
    }

    fn pattern_to_branch(
        &mut self,
        PatTir { kind, .. }: PatTir,
        nodes: &mut BumpVec<Node>,
        builder: &mut MirBuilder,
    ) {
        match kind {
            PatKindTir::Unit(unit) => match unit {
                UnitPatKindTir::Struct { fields } => {
                    for &pat in fields {
                        self.pattern_to_branch(pat, nodes);
                    }
                }
                UnitPatKindTir::Int(value, ..) => {
                    let int = match value {
                        Ok(span) => span_str!(self, span).parse().unwrap(),
                        Err(lit) => lit as u128,
                    };
                    nodes.push(Node::Scalar(Range::at(int)));
                }
                UnitPatKindTir::Binding(..) | UnitPatKindTir::Wildcard => {
                    nodes.push(Node::Scalar(Range::full()))
                }
                UnitPatKindTir::Enum {
                    id,
                    ty: enum_ty,
                    value,
                } => {
                    nodes.push(Node::Scalar(
                        if id as usize == self.typec[enum_ty].variants.len() - 1 {
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

    fn field(
        &mut self,
        FieldTir { field, header }: FieldTir,
        span: Span,
        dest: VRef<ValueMir>,
        r#move: bool,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        let node = self.node(header, None, false, builder)?;
        self.gen_field(node, Some(dest), field, span);
        if r#move {
            self.move_out(dest, span);
        }
        Some(dest)
    }

    fn deref(
        &mut self,
        node: TirNode,
        dest: VRef<ValueMir>,
        span: Span,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        let node = self.node(node, None, false, builder)?;
        self.inst(InstMir::Deref(node, dest), span);
        self.connect_deref_owner(node, dest);
        Some(dest)
    }

    fn r#ref(
        &mut self,
        node: TirNode,
        span: Span,
        dest: VRef<ValueMir>,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        let node = self.node(node, None, false, builder)?;
        self.handle_referencing(node, span);
        self.inst(InstMir::Ref(node, dest), span);
        Some(dest)
    }

    fn constructor(
        &mut self,
        fields: &[TirNode],
        ty: Ty,
        span: Span,
        dest: OptVRef<ValueMir>,
        r#move: bool,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        let mir_fields = fields
            .iter()
            .map(|&field| self.value(field.ty))
            .collect::<BumpVec<_>>();
        let final_dest = dest.unwrap_or_else(|| self.value(ty));
        {
            let mir_fields = builder.module.value_args.bump_slice(&mir_fields);
            self.inst(InstMir::Ctor(mir_fields, final_dest, dest.is_some()), span);
        }
        for (&field, value) in fields.iter().zip(mir_fields) {
            self.node(field, Some(value), true, builder);
        }
        if r#move && dest.is_none() {
            self.move_out(final_dest, span);
        }
        Some(final_dest)
    }

    fn block(
        &mut self,
        nodes: &[TirNode],
        dest: OptVRef<ValueMir>,
        span: Span,
        r#move: bool,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        let Some((&last, nodes)) = nodes.split_last() else {
            return Some(builder.uint());
        };

        let frame = self.start_scope_frame();
        let res = try {
            for &node in nodes {
                let value = self.node(node, None, false, builder)?;
                self.drop(value, node.span);
                // TODO: drop temporary values
            }

            self.node(last, dest, r#move, builder)?
        };

        match res {
            Some(..) => self.end_scope_frame(frame, span),
            None => self.discard_scope_frame(frame),
        }

        res
    }

    fn const_access(
        &mut self,
        dest: VRef<ValueMir>,
        r#const: FragRef<Const>,
        span: Span,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        self.inst(InstMir::ConstAccess(r#const, dest), span);
        Some(dest)
    }

    fn access(
        &mut self,
        var: VRef<VarHeaderTir>,
        span: Span,
        dest: OptVRef<ValueMir>,
        r#move: bool,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        let var = builder.get_var(var);
        self.inst(InstMir::Access(var.value, dest), span);
        if r#move {
            self.move_out(var.value, span);
        }
        Some(dest.unwrap_or(var.value))
    }

    fn call(
        &mut self,
        CallTir {
            func, params, args, ..
        }: CallTir,
        ty: Ty,
        span: Span,
        dest: VRef<ValueMir>,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        let callable = match func {
            CallableTir::Func(func) => CallableMir::Func(func),
            CallableTir::SpecFunc(bound_func) => CallableMir::SpecFunc(bound_func),
            CallableTir::Pointer(..) => todo!(),
        };

        let params = builder.project_ty_slice(params, self.typec);

        let args = args
            .iter()
            .map(|&arg| self.node(arg, None, true))
            .collect::<Option<BumpVec<_>>>()?;
        let args = builder.module.value_args.extend(args);
        let callable = builder.module.calls.push(CallMir {
            callable,
            params,
            args,
        });
        let call = InstMir::Call(callable, dest);
        self.inst(call, span);

        if ty == Ty::TERMINAL {
            self.close_block(span, ControlFlowMir::Terminal);
            return None;
        }

        Some(dest)
    }

    fn int(
        &mut self,
        computed: Option<i64>,
        span: Span,
        dest: VRef<ValueMir>,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        let lit = computed.unwrap_or_else(|| {
            span_str!(self, span)
                .parse()
                .expect("Lexer should have validated this.")
        });
        self.inst(InstMir::Int(lit, dest), span);
        Some(dest)
    }

    fn float(
        &mut self,
        computed: Option<f64>,
        span: Span,
        dest: VRef<ValueMir>,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        let lit = computed.unwrap_or_else(|| {
            span_str!(self, span)
                .parse()
                .expect("Lexer should have validated this.")
        });
        self.inst(InstMir::Float(lit, dest), span);
        Some(dest)
    }

    fn char(&mut self, span: Span, dest: VRef<ValueMir>, builder: &mut MirBuilder) -> NodeRes {
        let lit = Self::parse_char(span_str!(self, span.shrink(1)).chars().by_ref())
            .expect("Lexer should have validated this.");
        self.inst(InstMir::Int(lit as i64, dest), span);
        Some(dest)
    }

    fn bool(
        &mut self,
        value: bool,
        span: Span,
        dest: VRef<ValueMir>,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        self.inst(InstMir::Int(value as i64, dest), span);
        Some(dest)
    }

    fn r#return(&mut self, val: Option<&TirNode>, span: Span, builder: &mut MirBuilder) -> NodeRes {
        let ret_val = val
            .map(|&val| self.node(val, Some(builder.ret), true, builder))
            .transpose()?;

        let to_drop = mem::take(&mut builder.to_drop);
        for &value in &to_drop {
            self.drop(value, span);
        }
        builder.to_drop = to_drop;

        self.close_block(
            span,
            ControlFlowMir::Return(ret_val.unwrap_or(builder.uint())),
        );
        None
    }

    fn push_args(
        &mut self,
        args: FragSlice<Ty>,
        builder: &mut MirBuilder,
    ) -> (VRef<BlockMir>, BumpVec<VRef<ValueMir>>) {
        let block = builder.create_block();
        self.select_block(block);

        let args = self.typec[args]
            .to_bumpvec()
            .into_iter()
            .map(|ty| {
                let value = self.value(ty);
                self.create_var(value);
                value
            })
            .collect::<BumpVec<_>>();

        (block, args)
    }

    fn parse_char(repr: &mut impl Iterator<Item = char>) -> Option<char> {
        let char = repr.next()?;

        Some(match char {
            '\\' => match repr.next()? {
                'n' => '\n',
                _ => return None,
            },
            c => c,
        })
    }

    pub fn value_ty(&self, value: VRef<ValueMir>) -> Ty {
        builder.value_ty(value)
    }

    pub fn start_scope_frame(&mut self) -> MirVarFrame {
        MirVarFrame {
            base: builder.vars.len(),
            to_drop: builder.to_drop.len(),
        }
    }

    pub fn end_scope_frame(&mut self, frame: MirVarFrame, span: Span) {
        builder.vars.truncate(frame.base);
        let mut to_drop = mem::take(&mut builder.to_drop);
        for value in to_drop.drain(frame.to_drop..) {
            self.drop(value, span);
        }
        builder.to_drop = to_drop;
    }

    pub fn discard_scope_frame(&mut self, frame: MirVarFrame) {
        builder.vars.truncate(frame.base);
        builder.to_drop.truncate(frame.to_drop);
    }

    // pub fn pointer_to(
    //     &mut self,
    //     value: VRef<ValueMir>,
    //     mutability: Mutability,
    //     span: Span,
    // ) -> VRef<ValueMir> {
    //     let ty = self.value_ty(value);
    //     let ptr_ty = self.typec.pointer_to(mutability, ty, self.interner).into();
    //     let dest = self.value(ptr_ty);
    //     builder.func.set_referenced(value);
    //     self.inst(InstMir::Ref(value, dest), span);
    //     dest
    // }

    pub fn inst(&mut self, kind: InstMir, span: Span) -> Option<()> {
        self.current_block?;
        builder.insts.push((kind, span));
        Some(())
    }

    pub fn value(&mut self, ty: Ty) -> VRef<ValueMir> {
        builder.value(ty, self.typec)
    }

    pub fn create_var(&mut self, value: VRef<ValueMir>) {
        self.store_in_var(value);
        builder.module.set_var(value);
        builder.vars.push(VarMir { value });
    }

    pub fn mark_cycle(&mut self, value: VRef<BlockMir>) {
        builder.module.blocks[value].cycles += 1;
    }

    pub fn close_block(&mut self, span: Span, control_flow: ControlFlowMir) -> OptVRef<BlockMir> {
        let current_block = self.current_block?;

        self.increment_block_refcount(control_flow);

        builder.close_block(current_block, control_flow);
        builder.dd.block_closers[current_block] = span;

        mem::take(&mut self.current_block)
    }

    pub fn increment_block_refcount(&mut self, control_flow: ControlFlowMir) {
        match control_flow {
            ControlFlowMir::Terminal | ControlFlowMir::Return(..) => {}
            ControlFlowMir::Split {
                then, otherwise, ..
            } => {
                builder.module.blocks[then].ref_count += 1;
                builder.module.blocks[otherwise].ref_count += 1;
            }
            ControlFlowMir::Goto { dest, .. } => {
                builder.module.blocks[dest].ref_count += 1;
            }
        }
    }

    pub fn select_block(&mut self, block: VRef<BlockMir>) -> bool {
        mem::replace(&mut self.current_block, Some(block)).is_some()
    }
}

ctl_errors! {
    #[err => "match is not exhaustive"]
    #[info => "missing patterns: {gaps}"]
    #[help => "adding '_ {{}}' will make the match exhaustive"]
    error NonExhaustive: fatal {
        #[err loc]
        gaps ref: String,
        loc: SourceLoc,
    }
}

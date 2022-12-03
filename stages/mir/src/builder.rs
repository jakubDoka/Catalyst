use std::{default::default, iter, mem, sync::Arc};

use diags::*;
use lexing_t::Span;
use mir_t::*;
use packaging_t::span_str;
use storage::*;
use typec_t::*;

use crate::{moves::BranchBlock, *};

pub type NodeRes = OptVRef<ValueMir>;

impl MirChecker<'_, '_> {
    pub fn funcs(&mut self, input: &mut BumpVec<(FragRef<Func>, TirNode)>) -> &mut Self {
        for (func, body) in input.drain(..) {
            let body = self.func(func, body);
            self.mir.bodies.insert(
                func,
                FuncMir {
                    inner: Arc::new(body),
                },
            );
            self.mir_ctx.just_compiled.push(func);
        }

        self
    }

    fn func(&mut self, func: FragRef<Func>, body: TirNode) -> FuncMirInner {
        let Func {
            signature, flags, ..
        } = self.typec.funcs[func];

        self.mir_ctx
            .generics
            .extend(self.typec.pack_func_param_specs(func));
        self.mir_ctx.no_moves = flags.contains(FuncFlags::NO_MOVES);
        self.push_args(signature.args);
        let ret = self.value(signature.ret);
        self.mir_ctx.func.ret = ret;

        self.node(body, None, false);

        self.mir_move_ctx.clear();
        self.mir_ctx.clear()
    }

    fn node(
        &mut self,
        TirNode { kind, ty, span }: TirNode,
        dest: OptVRef<ValueMir>,
        r#move: bool,
    ) -> NodeRes {
        macro pass($dest:ident => $call:expr) {{
            let $dest = dest.unwrap_or_else(|| self.value(ty));
            $call
        }}

        match kind {
            TirKind::Block(stmts) => self.block(stmts, dest, span, r#move),
            TirKind::Int(computed) => pass!(dest => self.int(computed, span, dest)),
            TirKind::Char => pass!(dest => self.char(span, dest)),
            TirKind::Bool(value) => pass!(dest => self.bool(value, span, dest)),
            TirKind::Access(access) => self.access(access, span, dest, r#move),
            TirKind::Call(&call) => pass!(dest => self.call(call, ty, span, dest)),
            TirKind::Return(ret) => self.r#return(ret, span),
            TirKind::Ctor(fields) => self.constructor(fields, ty, span, dest, r#move),
            TirKind::Deref(&node) => pass!(dest => self.deref(node, dest, span)),
            TirKind::Ref(&node) => pass!(dest => self.r#ref(node, span, dest)),
            TirKind::Field(&field) => pass!(dest => self.field(field, span, dest, r#move)),
            TirKind::Match(&r#match) => pass!(dest => self.r#match(r#match, span, dest, r#move)),
            TirKind::If(&r#if) => pass!(dest => self.r#if(r#if, dest, span, r#move)),
            TirKind::Loop(&r#loop) => pass!(dest => self.r#loop(r#loop, dest, span, r#move)),
            TirKind::Continue(loop_id) => self.r#continue(loop_id, span),
            TirKind::Break(&r#break) => self.r#break(r#break, span),
            TirKind::Let(&r#let) => self.r#let(r#let),
            TirKind::Assign(&assign) => self.assign(assign, span),
        }
    }

    fn r#loop(
        &mut self,
        r#loop: LoopTir,
        dest: VRef<ValueMir>,
        span: Span,
        r#move: bool,
    ) -> NodeRes {
        let start = self.mir_ctx.create_block();
        self.start_loop(start, dest);

        self.close_block(span, ControlFlowMir::Goto(start, ValueMir::UNIT));
        self.select_block(start);

        let terminated = self.node(r#loop.body, None, r#move).is_none();

        if !terminated {
            self.mark_cycle(start);
            self.close_block(span, ControlFlowMir::Goto(start, ValueMir::UNIT));
        }

        if let Some(end) = self.end_loop(span, terminated) {
            self.select_block(end);
            self.mir_ctx.args.push(dest);
            return Some(dest);
        }

        None
    }

    fn r#continue(&mut self, loop_id: VRef<LoopHeaderTir>, span: Span) -> NodeRes {
        let LoopMir { start, depth, .. } = self.mir_ctx.loops[loop_id.index()];
        self.check_loop_moves(span, depth);
        self.mark_cycle(start);
        self.close_block(span, ControlFlowMir::Goto(start, ValueMir::UNIT));
        None
    }

    fn r#break(&mut self, BreakTir { loop_id, value }: BreakTir, span: Span) -> NodeRes {
        let LoopMir {
            ref mut end,
            dest,
            frame:
                MirVarFrame {
                    to_drop: drop_frame,
                    ..
                },
            ..
        } = self.mir_ctx.loops[loop_id.index()];
        let &mut end =
            end.get_or_insert_with(|| self.mir_ctx.func.blocks.push(BlockMir::default()));
        let value = value
            .map(|value| self.node(value, Some(dest), false))
            .transpose()?
            .unwrap_or(ValueMir::UNIT);

        let to_drop = mem::take(&mut self.mir_ctx.to_drop);
        for &value in to_drop.iter().skip(drop_frame) {
            self.drop(value, span);
        }
        self.mir_ctx.to_drop = to_drop;

        self.close_block(span, ControlFlowMir::Goto(end, value));
        None
    }

    fn assign(&mut self, AssignTir { lhs, rhs }: AssignTir, span: Span) -> NodeRes {
        let dest = self.node(lhs, None, false)?;
        self.move_in(dest, span);
        self.node(rhs, Some(dest), true)?;
        Some(ValueMir::UNIT)
    }

    fn r#let(&mut self, LetTir { pat, value }: LetTir) -> NodeRes {
        let value = self.node(value, None, false)?;
        self.bind_pattern_vars(pat, value);
        Some(ValueMir::UNIT)
    }

    fn r#if(
        &mut self,
        IfTir { top, elifs, r#else }: IfTir,
        dest: VRef<ValueMir>,
        span: Span,
        r#move: bool,
    ) -> NodeRes {
        self.start_branching();

        let mut reached = bumpvec![cap elifs.len() + 1 + r#else.is_some() as usize];
        let mut dest_block = None;
        for &IfBranchTir { cond, body } in iter::once(&top).chain(elifs) {
            let cond_val = self.node(cond, None, true)?;
            let then = self.mir_ctx.create_block();
            let next = self.mir_ctx.create_block();
            self.close_block(cond.span, ControlFlowMir::Split(cond_val, then, next));
            self.select_block(then);
            reached.push(self.branch(body, &mut dest_block, dest, None, r#move));
            self.select_block(next);
        }

        if let Some(r#else) = r#else {
            reached.push(self.branch(r#else, &mut dest_block, dest, None, r#move));
        } else {
            self.discard_branch();
            reached.push(Err(true));
        }

        self.end_branching(&reached, span);

        if let Some(dest_block) = dest_block {
            self.select_block(dest_block);
            self.mir_ctx.args.push(dest);
        }

        Some(dest)
    }

    fn r#match(
        &mut self,
        MatchTir { value, arms }: MatchTir,
        span: Span,
        dest: VRef<ValueMir>,
        r#move: bool,
    ) -> NodeRes {
        let mir_value = self.node(value, None, false)?;

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
            let gaps = patterns::find_gaps(tree);
            self.non_exhaustive(gaps, self.value_ty(mir_value), span)?
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
            let block = self.mir_ctx.create_block();
            let next_block = self.mir_ctx.create_block();
            self.close_block(arm.pat.span, ControlFlowMir::Split(cond, block, next_block));
            self.select_block(block);
            reached.push(self.match_arm(arm, &mut dest_block, mir_value, dest, r#move));
            self.select_block(next_block);
        }
        reached.push(self.match_arm(last, &mut dest_block, mir_value, dest, r#move));

        self.end_branching(&reached, span);

        let dest_block = dest_block?;
        self.select_block(dest_block);
        self.mir_ctx.args.push(dest);

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
    ) -> BranchBlock {
        if let Some(ret) = self.node(body, Some(dest), r#move) {
            if let Some(wrapper) = wrapper {
                self.end_scope_frame(wrapper, body.span);
            }
            let &mut dest_block = dest_block.get_or_insert_with(|| self.mir_ctx.create_block());
            let block = self.close_block(body.span, ControlFlowMir::Goto(dest_block, ret));
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
                        self.mir_ctx.func.set_mutable(dest);
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
                                let call = self.mir_ctx.func.calls.push(CallMir {
                                    callable: CallableMir::Func(Func::BOOL_BAND),
                                    params: default(),
                                    args: self.mir_ctx.func.value_args.extend([val, other]),
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
                    let call = self.mir_ctx.func.calls.push(CallMir {
                        callable: CallableMir::Func(ty.int_eq().unwrap()),
                        params: default(),
                        args: self.mir_ctx.func.value_args.extend([lit, value]),
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
                    let enum_flag = self.typec.enum_flag_ty(enum_ty).map(|enum_flag_ty| {
                        let dest = self.value(Ty::Builtin(enum_flag_ty));
                        self.inst(InstMir::Field(value, 0, dest), span);
                        let const_flag = self.value(Ty::Builtin(enum_flag_ty));
                        self.int(Some(id as i64), span, const_flag);
                        let call = self.mir_ctx.func.calls.push(CallMir {
                            callable: CallableMir::Func(
                                Ty::Builtin(enum_flag_ty).int_eq().unwrap(),
                            ),
                            params: default(),
                            args: self.mir_ctx.func.value_args.extend([dest, const_flag]),
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
                            let call = self.mir_ctx.func.calls.push(CallMir {
                                callable: CallableMir::Func(Func::BOOL_BAND),
                                params: default(),
                                args: self.mir_ctx.func.value_args.extend([val, other]),
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

    fn pattern_to_branch<'a>(
        &mut self,
        PatTir { kind, .. }: PatTir,
        nodes: &mut BumpVec<Node<'a>>,
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
    ) -> NodeRes {
        let node = self.node(header, None, false)?;
        self.gen_field(node, Some(dest), field, span);
        if r#move {
            self.move_out(dest, span);
        }
        Some(dest)
    }

    fn deref(&mut self, node: TirNode, dest: VRef<ValueMir>, span: Span) -> NodeRes {
        let node = self.node(node, None, false)?;
        self.inst(InstMir::Deref(node, dest), span);
        self.connect_deref_owner(node, dest);
        Some(dest)
    }

    fn r#ref(&mut self, node: TirNode, span: Span, dest: VRef<ValueMir>) -> NodeRes {
        let node = self.node(node, None, false)?;
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
    ) -> NodeRes {
        let mir_fields = fields
            .iter()
            .map(|&field| self.value(field.ty))
            .collect::<BumpVec<_>>();
        let final_dest = dest.unwrap_or_else(|| self.value(ty));
        {
            let mir_fields = self.mir_ctx.func.value_args.bump_slice(&mir_fields);
            self.inst(InstMir::Ctor(mir_fields, final_dest, dest.is_some()), span);
        }
        for (&field, value) in fields.iter().zip(mir_fields) {
            self.node(field, Some(value), true);
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
    ) -> NodeRes {
        let Some((&last, nodes)) = nodes.split_last() else {
            return Some(ValueMir::UNIT);
        };

        let frame = self.start_scope_frame();
        let res = try {
            for &node in nodes {
                let value = self.node(node, None, false)?;
                self.drop(value, node.span);
                // TODO: drop temporary values
            }

            self.node(last, dest, r#move)?
        };

        match res {
            Some(..) => self.end_scope_frame(frame, span),
            None => self.discard_scope_frame(frame),
        }

        res
    }

    fn access(
        &mut self,
        var: VRef<VarHeaderTir>,
        span: Span,
        dest: OptVRef<ValueMir>,
        r#move: bool,
    ) -> NodeRes {
        let var = self.mir_ctx.get_var(var);
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
    ) -> NodeRes {
        let callable = match func {
            CallableTir::Func(func) => CallableMir::Func(func),
            CallableTir::SpecFunc(bound_func) => CallableMir::SpecFunc(bound_func),
            CallableTir::Pointer(..) => todo!(),
        };

        let params = self.mir_ctx.project_ty_slice(params, self.typec);

        let args = args
            .iter()
            .map(|&arg| self.node(arg, None, true))
            .collect::<Option<BumpVec<_>>>()?;
        let args = self.mir_ctx.func.value_args.extend(args);
        let callable = self.mir_ctx.func.calls.push(CallMir {
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

    fn int(&mut self, computed: Option<i64>, span: Span, dest: VRef<ValueMir>) -> NodeRes {
        let lit = computed.unwrap_or_else(|| {
            span_str!(self, span)
                .parse()
                .expect("Lexer should have validated this.")
        });
        self.inst(InstMir::Int(lit, dest), span);
        Some(dest)
    }

    fn char(&mut self, span: Span, dest: VRef<ValueMir>) -> NodeRes {
        let lit = Self::parse_char(span_str!(self, span.shrink(1)).chars().by_ref())
            .expect("Lexer should have validated this.");
        self.inst(InstMir::Int(lit as i64, dest), span);
        Some(dest)
    }

    fn bool(&mut self, value: bool, span: Span, dest: VRef<ValueMir>) -> NodeRes {
        self.inst(InstMir::Bool(value, dest), span);
        Some(dest)
    }

    fn r#return(&mut self, val: Option<&TirNode>, span: Span) -> NodeRes {
        let ret_val = val
            .map(|&val| self.node(val, Some(self.mir_ctx.func.ret), true))
            .transpose()?;

        let to_drop = mem::take(&mut self.mir_ctx.to_drop);
        for &value in &to_drop {
            self.drop(value, span);
        }
        self.mir_ctx.to_drop = to_drop;

        self.close_block(
            span,
            ControlFlowMir::Return(ret_val.unwrap_or(ValueMir::UNIT)),
        );
        None
    }

    fn push_args(&mut self, args: FragSlice<Ty>) {
        let block = self.mir_ctx.create_block();
        self.select_block(block);

        for ty in self.typec[args].to_bumpvec() {
            let value = self.value(ty);
            self.create_var(value);
            self.mir_ctx.args.push(value);
        }
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
        self.mir_ctx.func.value_ty(value)
    }

    pub fn start_scope_frame(&mut self) -> MirVarFrame {
        MirVarFrame {
            base: self.mir_ctx.vars.len(),
            to_drop: self.mir_ctx.to_drop.len(),
        }
    }

    pub fn end_scope_frame(&mut self, frame: MirVarFrame, span: Span) {
        self.mir_ctx.vars.truncate(frame.base);
        let mut to_drop = mem::take(&mut self.mir_ctx.to_drop);
        for value in to_drop.drain(frame.base..) {
            self.drop(value, span);
        }
        self.mir_ctx.to_drop = to_drop;
    }

    pub fn discard_scope_frame(&mut self, frame: MirVarFrame) {
        self.mir_ctx.vars.truncate(frame.base);
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
    //     self.mir_ctx.func.set_referenced(value);
    //     self.inst(InstMir::Ref(value, dest), span);
    //     dest
    // }

    pub fn inst(&mut self, kind: InstMir, span: Span) -> Option<()> {
        self.current_block?;
        self.mir_ctx.insts.push((kind, span));
        Some(())
    }

    pub fn value(&mut self, ty: Ty) -> VRef<ValueMir> {
        self.mir_ctx.value(ty, self.typec)
    }

    pub fn create_var(&mut self, value: VRef<ValueMir>) {
        self.store_in_var(value);
        self.mir_ctx.vars.push(VarMir { value });
    }

    pub fn mark_cycle(&mut self, value: VRef<BlockMir>) {
        self.mir_ctx.func.blocks[value].cycles += 1;
    }

    pub fn close_block(&mut self, span: Span, control_flow: ControlFlowMir) -> OptVRef<BlockMir> {
        let current_block = self.current_block?;

        self.increment_block_refcount(control_flow);

        self.mir_ctx.close_block(current_block, control_flow);
        self.mir_ctx.dd.block_closers[current_block] = span;

        mem::take(&mut self.current_block)
    }

    pub fn increment_block_refcount(&mut self, control_flow: ControlFlowMir) {
        match control_flow {
            ControlFlowMir::Terminal | ControlFlowMir::Return(..) => {}
            ControlFlowMir::Split(.., a, b) => {
                self.mir_ctx.func.blocks[a].ref_count += 1;
                self.mir_ctx.func.blocks[b].ref_count += 1;
            }
            ControlFlowMir::Goto(a, ..) => {
                self.mir_ctx.func.blocks[a].ref_count += 1;
            }
        }
    }

    pub fn select_block(&mut self, block: VRef<BlockMir>) -> bool {
        mem::replace(&mut self.current_block, Some(block)).is_some()
    }

    gen_error_fns! {
        push non_exhaustive(self, err: Vec<Vec<Range>>, ty: Ty, span: Span) {
            err: "match is not exhaustive";
            info: (
                "missing patterns: {}",
                err
                    .iter()
                    .map(|seq| self.display_pat(seq, ty))
                    .intersperse(", ".into())
                    .collect::<String>()
            );
            (span, self.source) {
                err[span]: "this does not cover all possible cases";
            }
        }

        push move_from_pointer(self, span: Span) {
            err: "cannot move out of a pointer";
            info: "you can disable move validation with `#[no_moves]`";
            (span, self.source) {
                err[span]: "discovered here";
            }
        }

        push double_move(self, span: Span, source_span: Span) {
            err: "cannot move out of a value more than once";
            (span.joined(source_span), self.source) {
                err[span]: "detected here";
                info[source_span]: "first move here";
            }
        }
    }
}

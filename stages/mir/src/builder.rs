use std::{default::default, iter};

use diags::gen_error_fns;
use lexing_t::Span;
use mir_t::*;
use packaging_t::span_str;
use storage::*;
use typec_t::*;

use crate::*;

pub type NodeRes = OptVRef<ValueMir>;

impl MirChecker<'_> {
    pub fn funcs(
        &mut self,
        ctx: &mut MirBuilderCtx,
        input: &mut Vec<(VRef<Func>, TirNode)>,
    ) -> &mut Self {
        for (func, body) in input.drain(..) {
            let body = self.func(func, body, ctx);
            self.mir.bodies[func] = Some(body);
            ctx.just_compiled.push(func);
        }

        self
    }

    fn func(&mut self, func: VRef<Func>, body: TirNode, ctx: &mut MirBuilderCtx) -> FuncMir {
        let Func { signature, .. } = self.typec.funcs[func];

        let mut builder = self.push_args(signature.args, ctx);
        let ret = builder.value(signature.ret, self.typec);
        builder.ctx.func.ret = ret;

        self.node(body, None, &mut builder);

        ctx.clear()
    }

    fn node(
        &mut self,
        TirNode { kind, ty, span }: TirNode,
        dest: OptVRef<ValueMir>,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        let mut dest_fn = || dest.unwrap_or_else(|| builder.value(ty, self.typec));
        match kind {
            TirKind::Block(stmts) => self.block(stmts, dest_fn(), builder),
            TirKind::Int(computed) => self.int(computed, span, dest_fn(), builder),
            TirKind::Char => self.char(span, dest_fn(), builder),
            TirKind::Bool(value) => self.bool(value, span, dest_fn(), builder),
            TirKind::Access(access) => self.access(access, span, dest_fn(), builder),
            TirKind::Call(&call) => self.call(call, ty, span, dest, builder),
            TirKind::Return(ret) => self.r#return(ret, span, builder),
            TirKind::Const(&r#const) => self.r#const(r#const, ty, span, dest_fn(), builder),
            TirKind::Ctor(fields) => self.constructor(fields, ty, span, dest, builder),
            TirKind::Deref(&node) => self.deref(node, ty, span, builder),
            TirKind::Ref(&node) => self.r#ref(node, span, dest_fn(), builder),
            TirKind::Field(&field) => self.field(field, span, dest_fn(), builder),
            TirKind::Match(&r#match) => self.r#match(r#match, span, dest_fn(), builder),
            TirKind::If(&r#if) => self.r#if(r#if, dest_fn(), builder),
            TirKind::Let(&r#let) => self.r#let(r#let, builder),
            TirKind::Assign(&assign) => self.assign(assign, builder),
        }
    }

    fn assign(&mut self, AssignTir { lhs, rhs }: AssignTir, builder: &mut MirBuilder) -> NodeRes {
        let dest = self.node(lhs, None, builder)?;
        self.node(rhs, Some(dest), builder)?;
        Some(dest)
    }

    fn r#let(&mut self, LetTir { pat, value }: LetTir, builder: &mut MirBuilder) -> NodeRes {
        let value = self.node(value, None, builder)?;
        self.bind_pattern_vars(pat, value, builder);
        Some(ValueMir::UNIT)
    }

    fn r#if(
        &mut self,
        IfTir { top, elifs, r#else }: IfTir,
        dest: VRef<ValueMir>,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        let mut dest_block = None;
        for &IfBranchTir { cond, body } in iter::once(&top).chain(elifs) {
            let cond_val = self.node(cond, None, builder)?;
            let then = builder.ctx.create_block();
            let next = builder.ctx.create_block();
            builder.close_block(cond.span, ControlFlowMir::Split(cond_val, then, next));
            builder.select_block(then);
            self.branch(body, &mut dest_block, dest, builder);
            builder.select_block(next);
        }

        if let Some(r#else) = r#else {
            self.branch(r#else, &mut dest_block, dest, builder);
        }

        if let Some(dest_block) = dest_block {
            builder.select_block(dest_block);
            if dest != ValueMir::UNIT {
                builder.ctx.args.push(dest);
            }
        }

        Some(dest)
    }

    fn r#match(
        &mut self,
        MatchTir { value, arms }: MatchTir,
        span: Span,
        dest: VRef<ValueMir>,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        let value = self.node(value, None, builder)?;

        let mut arena = builder.ctx.pattern_solver_arena.take().unwrap_or_default();

        let branch_patterns = arms
            .iter()
            .enumerate()
            .filter_map(|(ord, &MatchArmTir { pat, .. })| {
                let mut branch_nodes = bumpvec![];
                self.pattern_to_branch(pat, &mut branch_nodes, None, &arena, builder);
                let (&start, rest) = branch_nodes.split_first()?;
                Some(Branch {
                    start,
                    nodes: arena.alloc_slice(rest),
                    ord,
                })
            })
            .collect::<BumpVec<_>>();

        let mut reachable = bumpvec![false; branch_patterns.len()];
        let tree = patterns::as_tree(&arena, &branch_patterns, &mut reachable);
        if tree.has_missing {
            let gaps = patterns::find_gaps(tree);
            self.non_exhaustive(gaps, builder.ctx.func.value_ty(value), span)?
        }

        arena.clear();
        builder.ctx.pattern_solver_arena = Some(arena);

        let arms = arms
            .iter()
            .zip(reachable)
            .filter_map(|(&arm, reachable)| reachable.then_some(arm))
            .collect::<BumpVec<_>>();
        let (&last, rest) = arms.split_last()?;
        let mut dest_block = None;
        for &arm in rest {
            let cond = self
                .pattern_to_cond(arm.pat, value, builder)
                .expect("only last pattern can be non refutable");
            let block = builder.ctx.create_block();
            let next_block = builder.ctx.create_block();
            builder.close_block(arm.pat.span, ControlFlowMir::Split(cond, block, next_block));
            builder.select_block(block);
            self.match_arm(arm, &mut dest_block, value, dest, builder);
            builder.select_block(next_block);
        }
        self.match_arm(last, &mut dest_block, value, dest, builder);

        let dest_block = dest_block?;
        builder.select_block(dest_block);
        if dest != ValueMir::UNIT {
            builder.ctx.args.push(dest);
        }

        Some(dest)
    }

    fn match_arm(
        &mut self,
        MatchArmTir { pat, body }: MatchArmTir,
        dest_block: &mut OptVRef<BlockMir>,
        value: VRef<ValueMir>,
        dest: VRef<ValueMir>,
        builder: &mut MirBuilder,
    ) {
        let frame = builder.ctx.start_frame();
        self.bind_pattern_vars(pat, value, builder);
        self.branch(body, dest_block, dest, builder);
        builder.ctx.end_frame(frame);
    }

    fn branch(
        &mut self,
        body: TirNode,
        dest_block: &mut OptVRef<BlockMir>,
        dest: VRef<ValueMir>,
        builder: &mut MirBuilder,
    ) {
        if let Some(ret) = self.node(body, Some(dest), builder) {
            let ret = (ret != ValueMir::UNIT).then_some(ret);
            let &mut dest_block = dest_block.get_or_insert_with(|| builder.ctx.create_block());
            builder.close_block(body.span, ControlFlowMir::Goto(dest_block, ret));
        }
    }

    fn bind_pattern_vars(
        &mut self,
        PatTir {
            kind, has_binding, ..
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
                        let dest = builder.value(field.ty, self.typec);
                        builder.inst(InstMir::Field(value, i as u32, dest), field.span);
                        self.bind_pattern_vars(field, dest, builder);
                    }
                }
                UnitPatKindTir::Binding(mutable, ..) => {
                    if mutable {
                        builder.ctx.func.set_mutable(value);
                    }
                    builder.ctx.vars.push(VarMir { value });
                }
                UnitPatKindTir::Int(..) | UnitPatKindTir::Wildcard => unreachable!(),
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
                    let band = self.typec.get_band();
                    for (i, &field) in fields {
                        let dest = builder.value(field.ty, self.typec);
                        builder.inst(InstMir::Field(value, i as u32, dest), field.span);
                        let Some(val) = self.pattern_to_cond(field, dest, builder) else {
                            continue;
                        };
                        ret_value = match ret_value {
                            Some(other) => {
                                let call = CallMir {
                                    callable: CallableMir::Func(band),
                                    params: default(),
                                    args: builder.ctx.func.value_args.bump([val, other]),
                                };
                                let ret = builder.value(Ty::BOOL, self.typec);
                                builder.inst(InstMir::Call(call, Some(ret)), field.span);
                                Some(ret)
                            }
                            None => Some(val),
                        }
                    }

                    ret_value
                }
                UnitPatKindTir::Int(int, cmp) => {
                    let val = builder.value(ty, self.typec);
                    let lit = self.int(int.err(), int.unwrap_or(span), val, builder)?;
                    let call = CallMir {
                        callable: CallableMir::Func(cmp),
                        params: default(),
                        args: builder.ctx.func.value_args.bump([lit, value]),
                    };
                    let cond = builder.value(Ty::BOOL, self.typec);
                    builder.inst(InstMir::Call(call, Some(cond)), int.unwrap_or(span));
                    Some(cond)
                }
                UnitPatKindTir::Binding(..) | UnitPatKindTir::Wildcard => None,
            },
            PatKindTir::Or(..) => todo!(),
        }
    }

    fn pattern_to_branch<'a>(
        &mut self,
        PatTir { kind, ty, .. }: PatTir,
        nodes: &mut BumpVec<Node<'a>>,
        parent: Option<Ty>,
        _arena: &'a Arena,
        _builder: &MirBuilder,
    ) {
        match kind {
            PatKindTir::Unit(unit) => match unit {
                UnitPatKindTir::Struct { fields } => {
                    let mut ty = Some(ty);
                    for &pat in fields {
                        self.pattern_to_branch(pat, nodes, ty.take(), _arena, _builder);
                    }
                }
                UnitPatKindTir::Int(value, ..) => {
                    let int = match value {
                        Ok(span) => span_str!(self, span).parse().unwrap(),
                        Err(lit) => lit as u128,
                    };
                    if let Some(parent) = parent
                        && let Ty::Enum(enum_ty) = parent.caller(self.typec)
                        && int as usize == self.typec[self.typec[enum_ty].variants].len() - 1
                    {
                        nodes.push(Node::Scalar(Range {
                            start: int,
                            end: UpperBound::Outside,
                        }));
                    } else {
                        nodes.push(Node::Scalar(Range::at(int)));
                    }
                }
                UnitPatKindTir::Binding(..) | UnitPatKindTir::Wildcard => {
                    nodes.push(Node::Scalar(Range::full()))
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
        builder: &mut MirBuilder,
    ) -> NodeRes {
        let node = self.node(header, None, builder)?;
        builder.inst(InstMir::Field(node, field, dest), span);
        Some(dest)
    }

    fn deref(&mut self, node: TirNode, ty: Ty, span: Span, builder: &mut MirBuilder) -> NodeRes {
        let node = self.node(node, None, builder)?;
        let dest = builder.value(ty, self.typec);
        builder.inst(InstMir::Deref(node, dest), span);
        Some(dest)
    }

    fn r#ref(
        &mut self,
        node: TirNode,
        span: Span,
        dest: VRef<ValueMir>,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        let node = self.node(node, None, builder)?;
        builder.ctx.func.set_referenced(node);
        builder.inst(InstMir::Ref(node, dest), span);
        Some(dest)
    }

    fn constructor(
        &mut self,
        fields: &[TirNode],
        ty: Ty,
        span: Span,
        dest: OptVRef<ValueMir>,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        let mir_fields = fields
            .iter()
            .map(|&field| builder.value(field.ty, self.typec))
            .collect::<BumpVec<_>>();
        let final_dest = dest.unwrap_or_else(|| builder.value(ty, self.typec));
        {
            let mir_fields = builder.ctx.func.value_args.bump_slice(&mir_fields);
            builder.inst(InstMir::Ctor(mir_fields, final_dest, dest.is_some()), span);
        }
        for (&field, value) in fields.iter().zip(mir_fields) {
            self.node(field, Some(value), builder);
        }
        Some(final_dest)
    }

    fn r#const(
        &mut self,
        value: TirNode,
        ty: Ty,
        span: Span,
        dest: VRef<ValueMir>,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        let const_block = builder.ctx.create_block();
        let Some(prev_block) = builder.current_block.replace(const_block) else {
            builder.current_block.take();
            return None;
        };

        let local_dest = builder.value(ty, self.typec);
        self.node(value, Some(local_dest), builder)?;
        builder.close_block(span, ControlFlowMir::Return(Some(local_dest)));
        builder.select_block(prev_block);

        let ty = builder.ctx.project_ty(ty, self.typec);

        let const_mir = FuncConstMir {
            block: const_block,
            ty,
        };

        let const_mir_id = builder.ctx.func.constants.push(const_mir);
        builder.inst(InstMir::Const(const_mir_id, dest), span);

        Some(dest)
    }

    fn block(
        &mut self,
        nodes: &[TirNode],
        dest: VRef<ValueMir>,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        let Some((&last, nodes)) = nodes.split_last() else {
            return Some(ValueMir::UNIT);
        };

        let frame = builder.ctx.start_frame();
        let res = try {
            for &node in nodes {
                self.node(node, None, builder)?;
            }

            self.node(last, Some(dest), builder)?
        };
        builder.ctx.end_frame(frame);

        res
    }

    fn access(
        &mut self,
        var: VRef<VarHeaderTir>,
        span: Span,
        dest: VRef<ValueMir>,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        let var = builder.ctx.get_var(var);
        builder.inst(InstMir::Access(var.value, dest), span);
        Some(dest)
    }

    fn call(
        &mut self,
        CallTir {
            func, params, args, ..
        }: CallTir,
        ty: Ty,
        span: Span,
        dest: OptVRef<ValueMir>,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        let callable = match func {
            CallableTir::Func(func) => CallableMir::Func(func),
            CallableTir::SpecFunc(bound_func) => CallableMir::SpecFunc(bound_func),
            CallableTir::Pointer(..) => todo!(),
        };

        let params = builder.ctx.project_ty_slice(params, self.typec);

        let args = args
            .iter()
            .map(|&arg| self.node(arg, None, builder))
            .collect::<Option<BumpVec<_>>>()?;
        let args = builder.ctx.func.value_args.bump(args);

        let value = dest.or_else(|| {
            (ty != Ty::UNIT && ty != Ty::TERMINAL).then(|| builder.value(ty, self.typec))
        });
        let call = InstMir::Call(
            CallMir {
                callable,
                params,
                args,
            },
            value,
        );
        builder.inst(call, span);

        if ty == Ty::TERMINAL {
            builder.close_block(span, ControlFlowMir::Terminal);
            return None;
        }

        Some(value.unwrap_or(ValueMir::UNIT))
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
        builder.inst(InstMir::Int(lit, dest), span);
        Some(dest)
    }

    fn char(&mut self, span: Span, dest: VRef<ValueMir>, builder: &mut MirBuilder) -> NodeRes {
        let lit = Self::parse_char(span_str!(self, span.shrink(1)).chars().by_ref())
            .expect("Lexer should have validated this.");
        builder.inst(InstMir::Int(lit as i64, dest), span);
        Some(dest)
    }

    fn bool(
        &mut self,
        value: bool,
        span: Span,
        dest: VRef<ValueMir>,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        builder.inst(InstMir::Bool(value, dest), span);
        Some(dest)
    }

    fn r#return(&mut self, val: Option<&TirNode>, span: Span, builder: &mut MirBuilder) -> NodeRes {
        let ret_val = val
            .filter(|val| val.ty != Ty::UNIT)
            .map(|&val| self.node(val, Some(builder.ctx.func.ret), builder))
            .transpose()?;

        builder.close_block(span, ControlFlowMir::Return(ret_val));
        None
    }

    fn push_args<'a>(&mut self, args: VSlice<Ty>, ctx: &'a mut MirBuilderCtx) -> MirBuilder<'a> {
        let block = ctx.create_block();
        let mut builder = MirBuilder::new(block, ctx);

        for &ty in &self.typec[args] {
            let value = builder.value(ty, self.typec);
            builder.ctx.vars.push(VarMir { value });
            builder.ctx.args.push(value);
        }

        builder
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
    }
}

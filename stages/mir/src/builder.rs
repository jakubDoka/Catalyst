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
            TirKind::Var(var) => self.var(var, builder),
            TirKind::Int => self.int(span, dest_fn(), builder),
            TirKind::Char => self.char(span, dest_fn(), builder),
            TirKind::Access(access) => self.access(access, span, dest_fn(), builder),
            TirKind::Call(&call) => self.call(call, ty, span, dest, builder),
            TirKind::Return(ret) => self.r#return(ret, span, builder),
            TirKind::Const(&r#const) => self.r#const(r#const, ty, span, dest_fn(), builder),
            TirKind::Ctor(fields) => self.constructor(fields, ty, span, dest, builder),
            TirKind::Deref(&node) => self.deref(node, span, dest_fn(), builder),
            TirKind::Ref(&node) => self.r#ref(node, span, dest_fn(), builder),
            TirKind::Field(&field) => self.field(field, span, dest_fn(), builder),
            TirKind::Match(..) => todo!(),
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

    fn deref(
        &mut self,
        node: TirNode,
        span: Span,
        dest: VRef<ValueMir>,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        let node = self.node(node, None, builder)?;
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
        builder.ctx.func.referenced.insert(node.index());
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

    fn var(&mut self, value: Option<&TirNode>, builder: &mut MirBuilder) -> NodeRes {
        let &value = value.expect("Only func params have no value.");
        let dest = builder.value(value.ty, self.typec);
        self.node(value, Some(dest), builder)?;
        builder.ctx.vars.push(dest);
        Some(dest)
    }

    fn access(
        &mut self,
        var: VRef<Var>,
        span: Span,
        dest: VRef<ValueMir>,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        let var = builder.ctx.get_var(var);
        builder.inst(InstMir::Access(var, dest), span);
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

    fn int(&mut self, span: Span, dest: VRef<ValueMir>, builder: &mut MirBuilder) -> NodeRes {
        let lit = span_str!(self, span)
            .parse()
            .expect("Lexer should have validated this.");
        builder.inst(InstMir::Int(lit, dest), span);
        Some(dest)
    }

    fn char(&mut self, span: Span, dest: VRef<ValueMir>, builder: &mut MirBuilder) -> NodeRes {
        let lit = Self::parse_char(span_str!(self, span.shrink(1)).chars().by_ref())
            .expect("Lexer should have validated this.");
        builder.inst(InstMir::Int(lit as i64, dest), span);
        Some(dest)
    }

    fn r#return(&mut self, val: Option<&TirNode>, span: Span, builder: &mut MirBuilder) -> NodeRes {
        let ret_val = if let Some(&val) = val {
            let ret_dest = builder.value(val.ty, self.typec);
            self.node(val, Some(ret_dest), builder)?;
            Some(ret_dest)
        } else {
            None
        };

        builder.close_block(span, ControlFlowMir::Return(ret_val));
        None
    }

    fn push_args<'a>(&mut self, args: VSlice<Ty>, ctx: &'a mut MirBuilderCtx) -> MirBuilder<'a> {
        let block = ctx.create_block();
        let mut builder = MirBuilder::new(block, ctx);

        for &ty in &self.typec[args] {
            let val = builder.value(ty, self.typec);
            builder.ctx.vars.push(val);
            builder.ctx.args.push(val);
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
}

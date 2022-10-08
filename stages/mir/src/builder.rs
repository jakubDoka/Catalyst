use lexing_t::Span;
use mir_t::*;
use packaging_t::span_str;
use storage::*;
use typec_t::*;

use crate::*;

pub type NodeRes = Option<VRef<ValueMir>>;

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

        self.node(body, &mut builder);

        ctx.clear()
    }

    fn node(&mut self, TirNode { kind, ty, span }: TirNode, builder: &mut MirBuilder) -> NodeRes {
        match kind {
            TirKind::Block(stmts) => self.block(stmts, builder),
            TirKind::Var(var) => self.var(var, builder),
            TirKind::Int => self.int(ty, span, builder),
            TirKind::Char => self.char(span, builder),
            TirKind::Call(&call) => self.call(call, ty, span, builder),
            TirKind::Access(access) => self.access(access, span, builder),
            TirKind::Return(ret) => self.r#return(ret, span, builder),
            TirKind::Const(&r#const) => self.r#const(r#const, ty, span, builder),
            TirKind::Constructor(fields) => self.constructor(fields, ty, span, builder),
            TirKind::Deref(&node) => self.deref(node, ty, span, builder),
            TirKind::Ref(&node) => self.r#ref(node, ty, span, builder),
        }
    }

    fn deref(&mut self, node: TirNode, ty: Ty, span: Span, builder: &mut MirBuilder) -> NodeRes {
        let node = self.node(node, builder)?;

        let value = builder.value(ty, self.typec);
        builder.ctx.func.values[value]
            .flags
            .insert(ValueMirFlags::LOADED);
        builder.inst(InstMir::Deref(node, value), span);
        Some(value)
    }

    fn r#ref(&mut self, node: TirNode, ty: Ty, span: Span, builder: &mut MirBuilder) -> NodeRes {
        let node = self.node(node, builder)?;
        builder.ctx.func.values[node]
            .flags
            .insert(ValueMirFlags::REFERENCED);
        let value = builder.value(ty, self.typec);
        builder.inst(InstMir::Ref(node, value), span);
        Some(value)
    }

    fn constructor(
        &mut self,
        fields: &[TirNode],
        ty: Ty,
        span: Span,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        let fields = fields
            .iter()
            .map(|&field| self.node(field, builder))
            .collect::<Option<BumpVec<_>>>()?;
        let fields = builder.ctx.func.value_args.bump(fields);

        let value = builder.value(ty, self.typec);
        builder.inst(InstMir::Constructor(fields, value), span);
        Some(value)
    }

    fn r#const(&mut self, value: TirNode, ty: Ty, span: Span, builder: &mut MirBuilder) -> NodeRes {
        let const_block = builder.ctx.create_block();
        let Some(prev_block) = builder.current_block.replace(const_block) else {
            builder.current_block.take();
            return None;
        };

        let value = self.node(value, builder);
        builder.close_block(span, ControlFlowMir::Return(value));
        builder.select_block(prev_block);

        let ty = builder.ctx.project_ty(ty, self.typec);

        let const_mir = FuncConstMir {
            block: const_block,
            ty,
        };

        let value = builder.ctx.func.values.push(ValueMir {
            ty,
            ..Default::default()
        });
        let const_mir_id = builder.ctx.func.constants.push(const_mir);
        builder.inst(InstMir::Const(const_mir_id, value), span);

        Some(value)
    }

    fn block(&mut self, nodes: &[TirNode], builder: &mut MirBuilder) -> NodeRes {
        let Some((&last, nodes)) = nodes.split_last() else {
            return Some(ValueMir::UNIT);
        };

        let frame = builder.ctx.start_frame();
        let res = try {
            for &node in nodes {
                self.node(node, builder)?;
            }

            self.node(last, builder)?
        };
        builder.ctx.end_frame(frame);

        res
    }

    fn var(&mut self, value: Option<&TirNode>, builder: &mut MirBuilder) -> NodeRes {
        let &value = value.expect("Only func params have no value.");
        let value = self.node(value, builder)?;
        builder.ctx.vars.push(value);
        Some(ValueMir::UNIT)
    }

    fn access(&mut self, var: VRef<Var>, span: Span, builder: &mut MirBuilder) -> NodeRes {
        let var = builder.ctx.get_var(var);
        builder.inst(InstMir::Access(var), span);
        Some(var)
    }

    fn call(
        &mut self,
        CallTir {
            func, params, args, ..
        }: CallTir,
        ty: Ty,
        span: Span,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        let callable = match func {
            CallableTir::Func(func) => CallableMir::Func(func),
            CallableTir::SpecFunc(bound_func) => CallableMir::SpecFunc(bound_func),
            CallableTir::Pointer(expr) => CallableMir::Pointer(self.node(expr, builder)?),
        };

        let params = builder.ctx.project_ty_slice(params, self.typec);

        let args = args
            .iter()
            .map(|&arg| self.node(arg, builder))
            .collect::<Option<BumpVec<_>>>()?;
        let args = builder.ctx.func.value_args.bump(args);

        let value = builder.value(ty, self.typec);
        let call = InstMir::Call(
            CallMir {
                callable,
                params,
                args,
            },
            value,
        );
        builder.inst(call, span);
        Some(value)
    }

    fn int(&mut self, ty: Ty, span: Span, builder: &mut MirBuilder) -> NodeRes {
        let value = builder.value(ty, self.typec);
        let lit = span_str!(self, span)
            .parse()
            .expect("Lexer should have validated this.");
        builder.inst(InstMir::Int(lit, value), span);
        Some(value)
    }

    fn char(&mut self, span: Span, builder: &mut MirBuilder) -> NodeRes {
        let value = builder.value(Ty::CHAR, self.typec);
        let lit = Self::parse_char(span_str!(self, span.shrink(1)).chars().by_ref())
            .expect("Lexer should have validated this.");
        builder.inst(InstMir::Int(lit as i64, value), span);
        Some(value)
    }

    fn r#return(&mut self, val: Option<&TirNode>, span: Span, builder: &mut MirBuilder) -> NodeRes {
        let ret_val = if let Some(&val) = val {
            Some(self.node(val, builder)?)
        } else {
            None
        };

        builder.close_block(span, ControlFlowMir::Return(ret_val));
        None
    }

    fn push_args<'a>(&mut self, args: VSlice<Ty>, ctx: &'a mut MirBuilderCtx) -> MirBuilder<'a> {
        let block = ctx.create_block();
        let builder = MirBuilder::new(block, ctx);

        for &ty in &self.typec[args] {
            let mir_ty = builder.ctx.project_ty(ty, self.typec);
            let val = builder.ctx.func.values.push(ValueMir {
                ty: mir_ty,
                ..Default::default()
            });
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

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

    fn node(&mut self, node: TirNode, builder: &mut MirBuilder) -> NodeRes {
        match node {
            TirNode::Block(&block) => self.block(block, builder),
            TirNode::Var(&var) => self.var(var, builder),
            TirNode::Int(&int) => self.int(int, builder),
            TirNode::Char(span) => self.char(span, builder),
            TirNode::Call(&call) => self.call(call, builder),
            TirNode::Access(&access) => self.access(access, builder),
            TirNode::Return(&ret) => self.r#return(ret, builder),
            TirNode::Const(&r#const) => self.r#const(r#const, builder),
        }
    }

    fn r#const(&mut self, r#const: ConstTir, builder: &mut MirBuilder) -> NodeRes {
        let ConstTir { value, .. } = r#const;

        let const_block = builder.ctx.create_block();
        let Some(prev_block) = builder.current_block.replace(const_block) else {
            builder.current_block.take();
            return None;
        };

        let value = self.node(value, builder);
        builder.close_block(r#const.span, ControlFlowMir::Return(value));
        builder.select_block(prev_block);

        let const_mir = FuncConstMir {
            block: const_block,
            ty: builder.ctx.func.values[value?].ty,
        };

        let value = builder.ctx.func.values.push(ValueMir { ty: const_mir.ty });
        let const_mir_id = builder.ctx.func.constants.push(const_mir);
        builder.inst(InstMir::Const(const_mir_id, value), r#const.span);

        Some(value)
    }

    fn block(&mut self, BlockTir { nodes, .. }: BlockTir, builder: &mut MirBuilder) -> NodeRes {
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

    fn var(&mut self, Variable { value, .. }: Variable, builder: &mut MirBuilder) -> NodeRes {
        let value = value.expect("Only func params have no value.");
        let value = self.node(value, builder)?;
        builder.ctx.vars.push(value);
        Some(ValueMir::UNIT)
    }

    fn access(
        &mut self,
        AccessTir { span, var, .. }: AccessTir,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        let var = builder.ctx.get_var(var);
        builder.inst(InstMir::Access(var), span);
        Some(var)
    }

    fn call(
        &mut self,
        CallTir {
            func,
            params,
            args,
            ty,
            span,
            ..
        }: CallTir,
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

    fn int(&mut self, int: IntLit, builder: &mut MirBuilder) -> NodeRes {
        let value = builder.value(int.ty, self.typec);
        let lit = span_str!(self, int.span)
            .parse()
            .expect("Lexer should have validated this.");
        builder.inst(InstMir::Int(lit, value), int.span);
        Some(value)
    }

    fn char(&mut self, span: Span, builder: &mut MirBuilder) -> NodeRes {
        let value = builder.value(Ty::CHAR, self.typec);
        let lit = Self::parse_char(span_str!(self, span).chars().by_ref())
            .expect("Lexer should have validated this.");
        builder.inst(InstMir::Int(lit as i64, value), span);
        Some(value)
    }

    fn r#return(
        &mut self,
        ReturnTir { val, span }: ReturnTir,
        builder: &mut MirBuilder,
    ) -> NodeRes {
        let ret_val = if let Some(val) = val {
            Some(self.node(val, builder)?)
        } else {
            None
        };

        builder.close_block(span, ControlFlowMir::Return(ret_val));
        None
    }

    fn push_args<'a>(&mut self, args: VRefSlice<Ty>, ctx: &'a mut MirBuilderCtx) -> MirBuilder<'a> {
        let block = ctx.create_block();
        let builder = MirBuilder::new(block, ctx);

        for &ty in &self.typec.ty_slices[args] {
            let mir_ty = builder.ctx.project_ty(ty, self.typec);
            let val = builder.ctx.func.values.push(ValueMir { ty: mir_ty });
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

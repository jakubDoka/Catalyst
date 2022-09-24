use std::mem;

use mir_t::*;
use packaging_t::span_str;
use storage::*;
use typec_t::*;

use crate::*;

pub type NodeRes = Option<VRef<ValueMir>>;

impl MirChecker<'_> {
    pub fn funcs(&mut self, ctx: &mut MirBuilderCtx, input: &[(VRef<Func>, TirNode)]) -> &mut Self {
        let mut out = mem::take(&mut ctx.mir_funcs);
        out.extend(
            input
                .iter()
                .map(|&(func, body)| (func, self.func(func, body, ctx))),
        );
        ctx.mir_funcs = out;

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
            TirNode::Call(&call) => self.call(call, builder),
            TirNode::Access(&access) => self.access(access, builder),
            TirNode::Return(&ret) => self.r#return(ret, builder),
        }
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
            CallableTir::BoundFunc(bound_func) => CallableMir::BoundFunc(bound_func),
            CallableTir::Pointer(expr) => CallableMir::Pointer(self.node(expr, builder)?),
        };

        let params = builder.ctx.project_ty_slice(params);

        let args = args
            .iter()
            .map(|&arg| self.node(arg, builder))
            .collect::<Option<BumpVec<_>>>()?;
        let args = builder.ctx.func.value_args.bump(args);

        let value = builder.value(ty);
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
        let value = builder.value(int.ty);
        let lit = span_str!(self, int.span)
            .parse()
            .expect("Lexer should have validated this.");
        builder.inst(InstMir::Int(lit, value), int.span);
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

        builder.close_block(span, ControlFlowMir::Return(ret_val.into()));

        None
    }

    fn push_args<'a>(&mut self, args: VRefSlice<Ty>, ctx: &'a mut MirBuilderCtx) -> MirBuilder<'a> {
        let block = ctx.create_block();
        let builder = MirBuilder::new(block, ctx);

        for &ty in &self.typec.ty_slices[args] {
            let mir_ty = builder.ctx.project_ty(ty);
            let val = builder.ctx.func.values.push(ValueMir { ty: mir_ty });
            builder.ctx.vars.push(val);
            builder.ctx.args.push(val);
        }

        builder
    }
}
use mir_t::*;
use storage::*;
use typec_t::*;

use crate::*;

pub type MirFuncs = BumpVec<(VRef<Func>, FuncMir)>;
pub type NodeRes = Option<VRef<ValueMir>>;

impl MirChecker<'_> {
    pub fn funcs(
        &mut self,
        ctx: &mut MirBuilderCtx,
        input: &mut TypeCheckedFuncs,
        out: &mut MirFuncs,
    ) {
        out.extend(
            input
                .drain(..)
                .filter_map(|(func, body)| body.map(|b| (func, self.func(func, b, ctx)))),
        )
    }

    fn func(&mut self, func: VRef<Func>, body: TirNode, ctx: &mut MirBuilderCtx) -> FuncMir {
        let Func { signature, .. } = self.typec.funcs[func];

        let mut builder = self.push_args(signature.args, ctx);

        self.expr(body, &mut builder);

        ctx.clear()
    }

    fn expr(&mut self, body: TirNode, builder: &mut MirBuilder) -> NodeRes {
        match body {
            TirNode::Block(_) => todo!(),
            TirNode::Param(_) => todo!(),
            TirNode::Int(_) => todo!(),
            TirNode::Return(_) => todo!(),
            TirNode::Call(_) => todo!(),
            TirNode::Access(_) => todo!(),
        }
    }

    fn push_args<'a>(&mut self, args: VRefSlice<Ty>, ctx: &'a mut MirBuilderCtx) -> MirBuilder<'a> {
        let block = ctx.create_block();
        let builder = MirBuilder::new(block, ctx);

        for &ty in &self.typec.ty_slices[args] {
            let val = builder.ctx.func.values.push(ValueMir { ty });
            builder.ctx.vars.push(val);
            builder.ctx.args.push(val);
        }

        builder
    }
}

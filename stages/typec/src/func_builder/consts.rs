use super::*;

impl<'arena, 'ctx> TirBuilder<'arena, 'ctx> {
    pub fn build_const(
        &mut self,
        ConstAst { value, .. }: ConstAst,
        id: FragRef<Const>,
    ) -> Option<TirNode<'arena>> {
        let Const { ty, .. } = self.ext.typec[id];

        let frame = self.ctx.start_frame();
        let node = self.expr(value, Inference::Strong(ty));
        self.ctx.end_frame(frame);

        node.and_then(|n| self.return_low(Some(n), value.span()))
    }
}

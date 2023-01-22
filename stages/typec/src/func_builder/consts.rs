use super::*;

impl TyChecker<'_> {
    pub fn build_consts<'a>(
        &mut self,
        arena: &'a Arena,
        ctx: &mut TirBuilderCtx,
        input: &TypecOutput<ConstAst, Const>,
        output: &mut BumpVec<(FragRef<Const>, TirNode<'a>)>,
    ) -> &mut Self {
        input
            .iter()
            .filter_map(|&(ast, id)| {
                let body = self.build_const(arena, ctx, ast, id)?;
                Some((id, body))
            })
            .collect_into(&mut **output);

        self
    }

    pub fn build_const<'a>(
        &mut self,
        arena: &'a Arena,
        ctx: &mut TirBuilderCtx,
        ConstAst { value, ty: ret, .. }: ConstAst,
        id: FragRef<Const>,
    ) -> Option<TirNode<'a>> {
        let Const { ty, .. } = self.typec[id];
        let mut builder = TirBuilder::new(arena, ty, Some(ret.span()), ctx);

        let frame = self.scope.start_frame();
        let node = self.expr(value, Inference::Strong(ty), &mut builder);
        self.scope.end_frame(frame);

        node
    }
}

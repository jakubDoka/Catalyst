use super::*;

impl TyChecker<'_> {
    pub fn block<'a>(
        &mut self,
        block: BlockAst,
        inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let frame = VarHeaderTir::start_frame(builder.ctx);
        let scope_frame = self.scope.start_frame();

        let Some((last, other)) = block.elements.split_last() else {
            return Some(TirNode::new(Ty::UNIT, TirKind::Block(&[]), block.span()))
        };

        let mut store = bumpvec![cap block.len()];
        store.extend(
            other
                .iter()
                .filter_map(|expr| self.expr(expr.value, Inference::None, builder)),
        );
        let last = self.expr(last.value, inference, builder);
        frame.end(builder.ctx, ());
        let last = last?;
        store.push(last);

        self.scope.end_frame(scope_frame);

        let nodes = builder.arena.alloc_slice(&store);
        Some(TirNode::new(last.ty, TirKind::Block(nodes), block.span()))
    }

    pub fn r#return<'a>(
        &mut self,
        expr: Option<ExprAst>,
        span: Span,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let value = expr
            .map(|expr| self.expr(expr, Inference::Strong(builder.ret), builder))
            .transpose()?;

        let span = expr.map_or(span, |expr| span.joined(expr.span()));

        self.return_low(value, span, builder)
    }

    pub fn return_low<'a>(
        &mut self,
        value: ExprRes<'a>,
        span: Span,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        self.type_check(builder.ret, value.map_or(Ty::UNIT, |value| value.ty), span)?;

        Some(TirNode::new(
            Ty::TERMINAL,
            TirKind::Return(value.map(|v| builder.arena.alloc(v))),
            span,
        ))
    }

    pub fn r#break<'a>(
        &mut self,
        r#break: BreakAst,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let loop_id = self.find_loop(r#break.label, builder)?;

        let value = r#break
            .value
            .map(|expr| self.expr(expr, builder.ctx.loops[loop_id].inference, builder))
            .transpose()?;

        if let Some(value) = value {
            self.type_check(
                builder.ctx.loops[loop_id].return_type,
                value.ty,
                r#break.value.unwrap().span(),
            )?;
            // if return_type is terminal or equal to value.ty, type-check passes
            // and overriding it will account for changing the terminal state of
            // the loop
            builder.ctx.loops[loop_id].return_type = value.ty;
            builder.ctx.loops[loop_id].inference = Inference::Strong(value.ty);
        }

        Some(TirNode::new(
            Ty::TERMINAL,
            TirKind::Break(builder.arena.alloc(BreakTir { loop_id, value })),
            r#break.span(),
        ))
    }

    pub fn r#continue<'a>(
        &mut self,
        r#continue: ContinueAst,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let loop_id = self.find_loop(r#continue.label, builder)?;

        Some(TirNode::new(
            Ty::TERMINAL,
            TirKind::Continue(loop_id),
            r#continue.span(),
        ))
    }

    pub fn r#loop<'a>(
        &mut self,
        loop_expr: LoopAst,
        inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let loop_id = builder.ctx.loops.push(LoopHeaderTir {
            return_type: Ty::TERMINAL,
            inference,
            label: loop_expr.label.map(|label| label.ident),
        });

        let var_frame = VarHeaderTir::start_frame(builder.ctx);
        let frame = self.scope.start_frame();
        if let Some(label) = loop_expr.label {
            self.scope
                .push(label.ident, ScopeItem::LoopHeaderTir(loop_id), label.span);
        }

        let body = self.expr(loop_expr.body, Inference::None, builder)?;
        self.scope.end_frame(frame);
        var_frame.end(builder.ctx, ());

        let ty = builder
            .ctx
            .loops
            .pop()
            .expect("this functions is the only one that pops and pushes")
            .return_type;

        Some(TirNode::new(
            ty,
            TirKind::Loop(builder.arena.alloc(LoopTir { id: loop_id, body })),
            loop_expr.span(),
        ))
    }

    pub fn deref<'a>(
        &mut self,
        expr: UnitExprAst,
        _inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let expr = self.unit_expr(expr, Inference::None, builder)?;
        let Ty::Pointer(ptr) = expr.ty else {
            todo!();
        };

        let base = self.typec[ptr].base;

        Some(TirNode::new(
            base,
            TirKind::Deref(builder.arena.alloc(expr)),
            expr.span,
        ))
    }

    pub fn r#ref<'a>(
        &mut self,
        mutability: MutabilityAst,
        expr: UnitExprAst,
        _inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let expr = self.unit_expr(expr, Inference::None, builder)?;
        let mutability = self.mutability(mutability)?;
        let ptr = self.typec.pointer_to(mutability, expr.ty, self.interner);

        Some(TirNode::new(
            Ty::Pointer(ptr),
            TirKind::Ref(builder.arena.alloc(expr)),
            expr.span,
        ))
    }

    pub fn r#let<'a>(
        &mut self,
        r#let @ LetAst { pat, ty, value, .. }: LetAst,
        _inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let ty = ty.map(|(.., ty)| self.ty(ty)).transpose()?;
        let value = self.expr(value, ty.into(), builder)?;
        let pat = self.pattern(pat, value.ty, builder)?;

        Some(TirNode::new(
            Ty::UNIT,
            TirKind::Let(builder.arena.alloc(LetTir { pat, value })),
            r#let.span(),
        ))
    }

    pub fn r#if<'a>(
        &mut self,
        r#if @ IfAst {
            cond,
            body,
            elifs,
            r#else,
            ..
        }: IfAst,
        inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let top = self.if_branch(cond, body, inference, builder)?;

        let elifs = elifs
            .iter()
            .filter_map(|&ElifAst { cond, body, .. }| {
                self.if_branch(cond, body, inference, builder)
            })
            .collect::<BumpVec<_>>();
        let elifs = builder.arena.alloc_iter(elifs);

        let r#else = r#else
            .map(|(.., r#else)| self.if_block(r#else, inference, builder))
            .transpose()?;

        let ty = Self::combine_branch_types(
            elifs
                .iter()
                .copied()
                .map(|branch| branch.body.ty)
                .chain(r#else.map(|r#else| r#else.ty)),
        );

        Some(TirNode::new(
            ty,
            TirKind::If(builder.arena.alloc(IfTir { top, elifs, r#else })),
            r#if.span(),
        ))
    }

    pub fn if_branch<'a>(
        &mut self,
        cond: ExprAst,
        body: IfBlockAst,
        inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> Option<IfBranchTir<'a>> {
        let cond = self.expr(cond, Inference::Strong(Ty::BOOL), builder)?;
        let body = self.if_block(body, inference, builder)?;
        Some(IfBranchTir { cond, body })
    }

    pub fn if_block<'a>(
        &mut self,
        body: IfBlockAst,
        mut inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        inference = inference.weaken();
        match body {
            IfBlockAst::Block(body) => self.block(body, inference, builder),
            IfBlockAst::Arrow(.., body) => {
                let frame = self.scope.start_frame();
                let expr = self.expr(body, inference, builder);
                self.scope.end_frame(frame);
                let expr = expr?;
                Some(TirNode::new(
                    expr.ty,
                    TirKind::Block(builder.arena.alloc_iter([expr])),
                    expr.span,
                ))
            }
        }
    }

    pub fn enum_ctor<'a>(
        &mut self,
        ctor @ EnumCtorAst { path, value }: EnumCtorAst,
        inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let (ty, variant_name, params) = self.enum_path(path, inference)?;

        let (variant, variant_ty) = self.typec[self.typec[ty].variants]
            .iter()
            .enumerate()
            .find_map(|(i, variant)| {
                (variant.name == variant_name.ident).then_some((i, variant.ty))
            })
            .or_else(|| todo!())?;

        let flag_ty = self.typec.enum_flag_ty(ty).unwrap();

        let variant_value = TirNode::new(
            Ty::Builtin(flag_ty),
            TirKind::Int(Some(variant as i64)),
            variant_name.span,
        );

        let mut param_slots = bumpvec![None; self.typec[ty].generics.len()];
        if let Some(inference) = inference.ty() && let Ty::Instance(inst) = inference {
            param_slots.iter_mut().zip(&self.typec[self.typec[inst].args])
                .for_each(|(slot, &arg)| *slot = Some(arg))
        }

        for (slot, &param) in param_slots
            .iter_mut()
            .zip(params.iter().flat_map(|p| p.iter()))
        {
            *slot = self.ty(param);
        }

        let value = if let Some((.., value)) = value {
            let inference = self
                .typec
                .try_instantiate(variant_ty, &param_slots, self.interner);
            let res = self.expr(value, inference.into(), builder)?;
            if inference.is_none() {
                self.infer_params(&mut param_slots, res.ty, variant_ty, value.span())?;
            }
            Some(res)
        } else if variant_ty != Ty::UNIT {
            todo!();
        } else {
            None
        };

        let ty = if param_slots.is_empty() {
            Ty::Enum(ty)
        } else {
            let Some(args) = param_slots.iter().copied().collect::<Option<BumpVec<_>>>() else {
                todo!();
            };
            Ty::Instance(
                self.typec
                    .instance(GenericTy::Enum(ty), &args, self.interner),
            )
        };

        let values = iter::once(variant_value)
            .chain(value.into_iter())
            .collect::<BumpVec<_>>();
        let values = builder.arena.alloc_iter(values);
        Some(TirNode::new(ty, TirKind::Ctor(values), ctor.span()))
    }

    pub fn r#match<'a>(
        &mut self,
        MatchExprAst { expr, body, .. }: MatchExprAst,
        inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let value = self.expr(expr, Inference::None, builder)?;

        let arms = body
            .iter()
            .map(|&MatchArmAst { pattern, body, .. }| {
                let frame = self.scope.start_frame();
                let var_frame = VarHeaderTir::start_frame(builder.ctx);
                let pat = self.pattern(pattern, value.ty, builder);
                let body = self.if_block(body, inference, builder);
                self.scope.end_frame(frame);
                var_frame.end(builder.ctx, ());
                Some(MatchArmTir {
                    pat: pat?,
                    body: body?,
                })
            })
            .collect::<Option<BumpVec<_>>>()?;
        let arms = builder.arena.alloc_iter(arms);

        let ty = Self::combine_branch_types(arms.iter().map(|arm| arm.body.ty));

        let tir = builder.arena.alloc(MatchTir { value, arms });
        Some(TirNode::new(ty, TirKind::Match(tir), body.span()))
    }

    pub fn combine_branch_types(tys: impl IntoIterator<Item = Ty>) -> Ty {
        tys.into_iter()
            .reduce(|a, b| match Ty::compatible(a, b) {
                true if a != Ty::TERMINAL => a,
                true => b,
                false => Ty::UNIT,
            })
            .unwrap_or(Ty::TERMINAL)
    }
}
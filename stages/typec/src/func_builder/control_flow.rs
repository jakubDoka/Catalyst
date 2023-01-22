use super::*;

impl TyChecker<'_> {
    pub fn block<'a>(
        &mut self,
        block: ListAst<ExprAst>,
        inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let frame = VarHeaderTir::start_frame(builder.ctx);
        let scope_frame = self.scope.start_frame();

        let Some((&last, other)) = block.elements.split_last() else {
            return Some(TirNode::new(Ty::UNIT, TirKind::Block(&[]), block.span()))
        };

        let mut store = bumpvec![cap block.len()];
        store.extend(
            other
                .iter()
                .filter_map(|&expr| self.expr(expr.value, Inference::None, builder)),
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
            .map(|expr| {
                Some((
                    self.expr(expr, builder.ctx.loops[loop_id].inference, builder)?,
                    expr.span(),
                ))
            })
            .transpose()?;

        if let Some((value, span)) = value {
            self.type_check(builder.ctx.loops[loop_id].return_type, value.ty, span)?;
            // if return_type is terminal or equal to value.ty, type-check passes
            // and overriding it will account for changing the terminal state of
            // the loop
            builder.ctx.loops[loop_id].return_type = value.ty;
            builder.ctx.loops[loop_id].inference = Inference::Strong(value.ty);
        } else {
            self.type_check(
                builder.ctx.loops[loop_id].return_type,
                Ty::UNIT,
                r#break.span(),
            )?;
            builder.ctx.loops[loop_id].return_type = Ty::UNIT;
        }

        Some(TirNode::new(
            Ty::TERMINAL,
            TirKind::Break(builder.arena.alloc(BreakTir {
                loop_id,
                value: value.map(|(value, ..)| value),
            })),
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

        let body = self.expr(loop_expr.body, Inference::None, builder);
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
            TirKind::Loop(builder.arena.alloc(LoopTir {
                id: loop_id,
                body: body?,
            })),
            loop_expr.span(),
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
        body: BranchAst,
        inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> Option<IfBranchTir<'a>> {
        let cond = self.expr(cond, Inference::Strong(Ty::BOOL), builder)?;
        let body = self.if_block(body, inference, builder)?;
        Some(IfBranchTir { cond, body })
    }

    pub fn if_block<'a>(
        &mut self,
        body: BranchAst,
        mut inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        inference = inference.weaken();
        match body {
            BranchAst::Block(body) => self.block(body, inference, builder),
            BranchAst::Arrow(.., body) => {
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
            .or_else(|| {
                self.workspace.push(ComponentNotFound {
                    ty: self.typec.display_ty(Ty::Enum(ty), self.interner),
                    loc: SourceLoc {
                        origin: self.source,
                        span: variant_name.span,
                    },
                    suggestions: self.typec[self.typec[ty].variants]
                        .iter()
                        .map(|variant| variant.name)
                        .map(|name| &self.interner[name])
                        .intersperse(", ")
                        .collect(),
                    something: "variant",
                })?
            })?;

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
            self.workspace.push(MissingEnumCtorValue {
                ty: self.typec.display_ty(variant_ty, self.interner),
                loc: SourceLoc {
                    origin: self.source,
                    span: variant_name.span,
                },
            })?;
        } else {
            None
        };

        let params = self.unpack_param_slots(
            param_slots.iter().copied(),
            ctor.span(),
            builder,
            "enum constructor",
            "(<type_path>\\<variant_ident>\\[<param_ty>, ..]~<value>)",
        )?;

        let ty = if params.is_empty() {
            Ty::Enum(ty)
        } else {
            Ty::Instance(
                self.typec
                    .instance(GenericTy::Enum(ty), params, self.interner),
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

ctl_errors! {
    #[err => "dereference of non pointer type '{ty}'"]
    error NonPointerDereference: fatal {
        #[err loc]
        ty ref: String,
        loc: SourceLoc,
    }

    #[err => "missing value in enum constructor"]
    #[note => "expected value of type '{ty}'"]
    #[note => "only variants containing '()' don't need value"]
    error MissingEnumCtorValue: fatal {
        #[err loc]
        ty ref: String,
        loc: SourceLoc,
    }
}

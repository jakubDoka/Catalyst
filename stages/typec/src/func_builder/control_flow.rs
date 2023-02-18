use super::*;

impl<'arena, 'ctx> TirBuilder<'arena, 'ctx> {
    pub fn block(&mut self, block: ListAst<ExprAst>, inference: Inference) -> ExprRes<'arena> {
        let frame = self.ctx.start_frame();

        let Some((&last, other)) = block.elements.split_last() else {
            return Some(TirNode::new(Ty::UNIT, TirKind::Block(&[]), block.span()))
        };

        let mut store = bumpvec![cap block.len()];
        store.extend(
            other
                .iter()
                .filter_map(|&expr| self.expr(expr.value, Inference::None)),
        );
        let last = self.expr(last.value, inference);
        self.ctx.end_frame(frame);
        let last = last?;
        store.push(last);

        let nodes = self.arena.alloc_slice(&store);
        Some(TirNode::new(last.ty, TirKind::Block(nodes), block.span()))
    }

    pub fn r#return(&mut self, expr: Option<ExprAst>, span: Span) -> ExprRes<'arena> {
        let value = expr
            .map(|expr| self.expr(expr, Inference::from(self.ret)))
            .transpose()?;

        let span = expr.map_or(span, |expr| span.joined(expr.span()));

        self.return_low(value, span)
    }

    pub fn return_low(&mut self, value: ExprRes<'arena>, span: Span) -> ExprRes<'arena> {
        let checked_ty = value.map_or(Ty::UNIT, |value| value.ty);
        let &mut ret_ty = self.ret.get_or_insert(checked_ty);
        self.type_check(ret_ty, value.map_or(Ty::UNIT, |value| value.ty), span)?;

        Some(TirNode::new(
            Ty::TERMINAL,
            TirKind::Return(value.map(|v| self.arena.alloc(v))),
            span,
        ))
    }

    pub fn r#break(&mut self, r#break: BreakAst) -> ExprRes<'arena> {
        let loop_id = self.find_loop(r#break.label)?;
        let inference = self.ctx[loop_id].inference;

        let value = r#break
            .value
            .map(|expr| Some((self.expr(expr, inference)?, expr.span())))
            .transpose()?;

        let return_type = self.ctx[loop_id].return_type;
        if let Some((value, span)) = value {
            self.type_check(return_type, value.ty, span)?;
            // if return_type is terminal or equal to value.ty, type-check passes
            // and overriding it will account for changing the terminal state of
            // the loop
            self.ctx[loop_id].return_type = value.ty;
            self.ctx[loop_id].inference = Inference::Strong(value.ty);
        } else {
            self.type_check(return_type, Ty::UNIT, r#break.span())?;
            self.ctx[loop_id].return_type = Ty::UNIT;
        }

        Some(TirNode::new(
            Ty::TERMINAL,
            TirKind::Break(self.arena.alloc(BreakTir {
                loop_id,
                value: value.map(|(value, ..)| value),
            })),
            r#break.span(),
        ))
    }

    pub fn r#continue(&mut self, r#continue: ContinueAst) -> ExprRes<'arena> {
        let loop_id = self.find_loop(r#continue.label)?;

        Some(TirNode::new(
            Ty::TERMINAL,
            TirKind::Continue(loop_id),
            r#continue.span(),
        ))
    }

    pub fn r#loop(&mut self, loop_expr: LoopAst, inference: Inference) -> ExprRes<'arena> {
        let frame = self.ctx.start_frame();

        let loop_id = self.ctx.push_loop(inference, loop_expr.label);

        let body = self.expr(loop_expr.body, Inference::None);
        self.ctx.end_frame(frame);

        let ty = self.ctx.pop_loop();

        Some(TirNode::new(
            ty,
            TirKind::Loop(self.arena.alloc(LoopTir {
                id: loop_id,
                body: body?,
            })),
            loop_expr.span(),
        ))
    }

    pub fn r#if(
        &mut self,
        r#if @ IfAst {
            cond,
            body,
            elifs,
            r#else,
            ..
        }: IfAst,
        inference: Inference,
    ) -> ExprRes<'arena> {
        let top = self.if_branch(cond, body, inference)?;

        let elifs = elifs
            .iter()
            .filter_map(|&ElifAst { cond, body, .. }| self.if_branch(cond, body, inference))
            .collect::<BumpVec<_>>();
        let elifs = self.arena.alloc_iter(elifs);

        let r#else = r#else
            .map(|(.., r#else)| self.if_block(r#else, inference))
            .transpose()?;

        let ty = Self::combine_branch_types(
            elifs
                .iter()
                .chain(iter::once(&top))
                .copied()
                .map(|branch| branch.body.ty)
                .chain(r#else.map(|r#else| r#else.ty)),
        );

        Some(TirNode::new(
            ty,
            TirKind::If(self.arena.alloc(IfTir { top, elifs, r#else })),
            r#if.span(),
        ))
    }

    pub fn if_branch(
        &mut self,
        cond: ExprAst,
        body: BranchAst,
        inference: Inference,
    ) -> Option<IfBranchTir<'arena>> {
        let cond = self.expr(cond, Inference::Strong(Ty::BOOL))?;
        let body = self.if_block(body, inference)?;
        Some(IfBranchTir { cond, body })
    }

    pub fn if_block(&mut self, body: BranchAst, mut inference: Inference) -> ExprRes<'arena> {
        inference = inference.weaken();
        match body {
            BranchAst::Block(body) => self.block(body, inference),
            BranchAst::Arrow(.., body) => {
                let frame = self.ctx.start_frame();
                let expr = self.expr(body, inference);
                self.ctx.end_frame(frame);
                let expr = expr?;
                Some(TirNode::new(
                    expr.ty,
                    TirKind::Block(self.arena.alloc_iter([expr])),
                    expr.span,
                ))
            }
        }
    }

    pub fn enum_ctor(
        &mut self,
        ctor @ EnumCtorAst { path, value }: EnumCtorAst,
        inference: Inference,
    ) -> ExprRes<'arena> {
        let (enum_ty, variant_name, params) = self.enum_path(path, inference)?;

        let (variant, variant_ty) = self.ext.types[self.ext.types[enum_ty].variants]
            .iter()
            .enumerate()
            .find_map(|(i, variant)| {
                (variant.name == variant_name.ident).then_some((i, variant.ty))
            })
            .or_else(|| {
                let ty = self.ext.creator().display(Ty::Enum(enum_ty));
                self.ext.workspace.push(ComponentNotFound {
                    ty,
                    loc: self.meta.loc(variant_name.span),
                    suggestions: self.ext.types[self.ext.types[enum_ty].variants]
                        .iter()
                        .map(|variant| &variant.name)
                        .map(|name| name.get(self.ext.interner))
                        .intersperse(", ")
                        .collect(),
                    something: "variant",
                })?
            })?;

        let flag_ty = self.ext.types.enum_flag_ty(enum_ty);

        let variant_value = TirNode::new(
            Ty::Builtin(flag_ty),
            TirKind::Int(Some(variant as i64)),
            variant_name.span,
        );

        let mut param_slots = bumpvec![None; self.ext.types[enum_ty].generics.len()];
        if let Some(inference) = inference.ty() && let Ty::Instance(inst) = inference {
            param_slots.iter_mut().zip(&self.ext.types[self.ext.types[inst].args])
                .for_each(|(slot, &arg)| *slot = Some(arg))
        }

        for (slot, &param) in param_slots
            .iter_mut()
            .zip(params.iter().flat_map(|p| p.iter()))
        {
            *slot = self.parser().ty(param);
        }

        let value = if let Some((.., value)) = value {
            let inference = self.ext.creator().try_instantiate(variant_ty, &param_slots);
            let res = self.expr(value, inference.into())?;
            if inference.is_none() {
                self.infer_params(&mut param_slots, res.ty, variant_ty, value.span())?;
            }
            Some(res)
        } else if variant_ty != Ty::UNIT {
            MissingEnumCtorValue {
                ty: self.ext.creator().display(variant_ty),
                loc: self.meta.loc(variant_name.span),
            }
            .add(self.ext.workspace)?;
        } else {
            None
        };

        let params = self.unpack_param_slots(
            param_slots.iter().copied(),
            ctor.span(),
            "enum constructor",
            "(<type_path>\\<variant_ident>\\[<param_ty>, ..]~<value>)",
        )?;

        let ty = if params.is_empty() {
            Ty::Enum(enum_ty)
        } else {
            Ty::Instance(
                self.ext
                    .creator()
                    .instance(GenericTy::Enum(enum_ty), params),
            )
        };

        let values = iter::once(variant_value)
            .chain(value.into_iter())
            .collect::<BumpVec<_>>();
        let values = self.arena.alloc_iter(values);
        Some(TirNode::new(ty, TirKind::Ctor(values), ctor.span()))
    }

    pub fn r#match(
        &mut self,
        MatchExprAst { expr, body, .. }: MatchExprAst,
        inference: Inference,
    ) -> ExprRes<'arena> {
        let value = self.expr(expr, Inference::None)?;

        let arms = body
            .iter()
            .map(|&MatchArmAst { pattern, body, .. }| {
                let frame = self.ctx.start_frame();
                let pat = self.pattern(pattern, value.ty);
                let body = self.if_block(body, inference);
                self.ctx.end_frame(frame);
                Some(MatchArmTir {
                    pat: pat?,
                    body: body?,
                })
            })
            .collect::<Option<BumpVec<_>>>()?;
        let arms = self.arena.alloc_iter(arms);

        let ty = Self::combine_branch_types(arms.iter().map(|arm| arm.body.ty));

        let tir = self.arena.alloc(MatchTir { value, arms });
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
    pub error NonPointerDereference: fatal {
        #[err loc]
        ty ref: String,
        loc: SourceLoc,
    }

    #[err => "missing value in enum constructor"]
    #[note => "expected value of type '{ty}'"]
    #[note => "only variants containing '()' don't need value"]
    pub error MissingEnumCtorValue: fatal {
        #[err loc]
        ty ref: String,
        loc: SourceLoc,
    }
}

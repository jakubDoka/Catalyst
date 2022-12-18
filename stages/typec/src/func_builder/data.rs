use super::*;

impl TyChecker<'_> {
    pub fn struct_ctor<'a>(
        &mut self,
        ctor @ StructCtorAst { path, body, .. }: StructCtorAst,
        inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let (ty, params) = if let Some(path @ PathAst { slash: None, .. }) = path {
            let (ty, params) = self.ty_path(path)?;
            (
                match ty {
                    TyPathResult::Ty(ty) => ty.as_generic(),
                    _ => None,
                },
                params.and_then(|params| {
                    Some((
                        params.span(),
                        params
                            .iter()
                            .map(|&param| self.ty(param))
                            .nsc_collect::<Option<BumpVec<_>>>()?,
                    ))
                }),
            )
        } else {
            let ty = inference.ty().or_else(|| self.cannot_infer(ctor.span())?)?;
            match ty {
                Ty::Instance(instance) => (
                    Some(self.typec[instance].base),
                    Some((
                        default(),
                        self.typec[self.typec[instance].args].to_bumpvec(),
                    )),
                ),
                _ => (ty.as_generic(), None),
            }
        };

        let Some(GenericTy::Struct(struct_id)) = ty else {
            self.expected_struct_path(path.map_or(ctor.span(), |p| p.span()))?;
        };

        let struct_meta = self.typec[struct_id];

        let mut param_slots = bumpvec![None; struct_meta.generics.len()];

        if let Some((span, params)) = params {
            if params.len() > param_slots.len() {
                self.too_many_params(span, param_slots.len())?;
            }

            param_slots
                .iter_mut()
                .zip(params.iter().copied())
                .for_each(|(slot, param)| *slot = Some(param))
        }

        let mut fields = bumpvec![None; body.len()];

        for field_ast @ &StructCtorFieldAst { name, expr } in body.iter() {
            let (index, field) = self.typec.fields[struct_meta.fields]
                .iter()
                .copied()
                .enumerate()
                .find(|(.., field)| field.name == name.ident)
                .or_else(|| {
                    self.unknown_field(Ty::Struct(struct_id), struct_meta.fields, ctor.span())?
                })?;

            let inference = self
                .typec
                .try_instantiate(field.ty, &param_slots, self.interner);
            let expr = if let Some(expr) = expr {
                self.expr(expr, inference.into(), builder)
            } else {
                self.value_path(
                    PathAst {
                        start: PathItemAst::Ident(name),
                        segments: &[],
                        slash: None,
                    },
                    inference.into(),
                    builder,
                )
            };
            let Some(value) = expr else {
                continue;
            };

            self.infer_params(&mut param_slots, value.ty, field.ty, field_ast.span());

            if fields[index].replace(value).is_some() {
                self.duplicate_field(name.span());
            }
        }

        let Some(params) = param_slots.iter().copied().collect::<Option<BumpVec<_>>>() else {
            let missing_params = param_slots
                .iter()
                .copied()
                .enumerate()
                .filter_map(|(i, param)| param.is_none().then_some(i))
                .collect::<BumpVec<_>>();
            self.missing_constructor_params(ctor.span(), missing_params)?;
        };

        let Some(fields) = fields.iter().copied().collect::<Option<BumpVec<_>>>() else {
            let missing_fields = self.typec.fields[struct_meta.fields]
                .iter()
                .zip(fields.iter())
                .filter_map(|(field, value)| value.is_none().then_some(field.name))
                .collect::<BumpVec<_>>();

            self.missing_constructor_fields(ctor.span(), missing_fields.as_slice())?;
        };

        let final_ty = if params.is_empty() {
            Ty::Struct(struct_id)
        } else {
            Ty::Instance(
                self.typec
                    .instance(GenericTy::Struct(struct_id), &params, self.interner),
            )
        };

        Some(TirNode::new(
            final_ty,
            TirKind::Ctor(builder.arena.alloc_iter(fields)),
            ctor.span(),
        ))
    }

    pub fn balance_pointers<'a>(
        &mut self,
        node: &mut TirNode<'a>,
        ty: Ty,
        builder: &mut TirBuilder<'a, '_>,
    ) -> Option<()> {
        let (desired_pointer_depth, mutability) = match ty {
            Ty::Pointer(ptr) => (self.typec[ptr].depth, self.typec[ptr].mutability),
            _ => (0, Mutability::Immutable),
        };
        loop {
            let current_pointed_depth = node.ty.ptr_depth(self.typec);
            match desired_pointer_depth.cmp(&current_pointed_depth) {
                Ordering::Less => {
                    let ty = self.typec.deref(node.ty);
                    *node = TirNode::new(ty, TirKind::Deref(builder.arena.alloc(*node)), node.span);
                }
                Ordering::Greater => {
                    let ty = self.typec.pointer_to(mutability, node.ty, self.interner);
                    *node = TirNode::new(
                        Ty::Pointer(ty),
                        TirKind::Ref(builder.arena.alloc(*node)),
                        node.span,
                    );
                }
                Ordering::Equal => break,
            }
        }

        Some(())
    }

    pub fn int<'a>(&mut self, span: Span, inference: Inference) -> ExprRes<'a> {
        let span_str = span_str!(self, span);
        let (ty, postfix_len) = Ty::INTEGERS
            .iter()
            .map(|&ty| {
                (
                    ty,
                    match ty {
                        Ty::Builtin(b) => b.name(),
                        _ => unreachable!(),
                    },
                )
            })
            .find_map(|(ty, str)| span_str.ends_with(str).then_some((ty, str.len())))
            .or_else(|| {
                inference
                    .ty()
                    .filter(|ty| Ty::INTEGERS.contains(ty))
                    .map(|ty| (ty, 0))
            })
            .unwrap_or((Ty::UINT, 0));
        Some(TirNode::new(
            ty,
            TirKind::Int(None),
            span.sliced(..span_str.len() - postfix_len),
        ))
    }

    pub fn char<'a>(&mut self, span: Span) -> ExprRes<'a> {
        Some(TirNode::new(Ty::CHAR, TirKind::Char, span))
    }

    pub fn bool(&mut self, span: Span) -> ExprRes<'static> {
        Some(TirNode::new(
            Ty::BOOL,
            TirKind::Bool(span_str!(self, span).starts_with('t')),
            span,
        ))
    }
}

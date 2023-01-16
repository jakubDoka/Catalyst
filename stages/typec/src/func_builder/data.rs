use super::{
    control_flow::NonPointerDereference,
    lookup::{CannotInferExpression, UnexpectedType, WrongGenericParamCount},
    *,
};

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
                    TyPathResult::Ty(ty) => ty,
                    _ => todo!(),
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
            let Some(ty) = inference.ty() else {
                self.workspace.push(CannotInferExpression {
                    help: "add struct path before the slash",
                    loc: SourceLoc { origin: self.source, span: ctor.span() },
                })?;
            };
            let (ty, params) = ty.base_with_params(self.typec);
            (ty, Some((default(), self.typec[params].to_bumpvec())))
        };

        let Ty::Struct(struct_ty) = ty else {
            self.workspace.push(UnexpectedType {
                expected: "struct",
                found: self.typec.display_ty(ty, self.interner),
                loc: SourceLoc { origin: self.source, span: ctor.span() },
            })?;
        };

        let struct_meta = self.typec[struct_ty];

        let mut param_slots = bumpvec![None; struct_meta.generics.len()];

        if let Some((span, ref params)) = params {
            if params.len() > param_slots.len() {
                self.workspace.push(WrongGenericParamCount {
                    expected: param_slots.len(),
                    found: params.len(),
                    loc: SourceLoc {
                        origin: self.source,
                        span,
                    },
                });
            }

            param_slots
                .iter_mut()
                .zip(params.iter().copied())
                .for_each(|(slot, param)| *slot = Some(param))
        }

        let mut fields = bumpvec![None; body.len()];

        let params = params.map_or(default(), |(.., p)| p);

        for field_ast @ &StructCtorFieldAst { name, value, .. } in body.iter() {
            let Some((index, .., ty)) = self.find_field(struct_ty, params.as_slice(), name) else {
                continue;
            };

            let inference = self.typec.try_instantiate(ty, &param_slots, self.interner);
            let expr = if let Some((.., expr)) = value {
                self.expr(expr, inference.into(), builder)
            } else {
                self.value_path(
                    PathAst {
                        start: PathSegmentAst::Name(name),
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

            self.infer_params(&mut param_slots, value.ty, ty, field_ast.span());

            if let Some(prev) = fields[index].replace(value) {
                self.workspace.push(DuplicateCtorField {
                    prev: prev.span,
                    loc: SourceLoc {
                        origin: self.source,
                        span: name.span,
                    },
                });
            }
        }

        let params = self.unpack_param_slots(
            param_slots.iter().copied(),
            ctor.span(),
            builder,
            "struct constructor",
            "(<struct_path>\\[<param_ty>, ...]\\{...})",
        )?;

        let missing_fields = self.typec[struct_meta.fields]
            .iter()
            .zip(fields.iter())
            .filter_map(|(field, value)| value.is_none().then_some(field.name))
            .map(|name| &self.interner[name])
            .intersperse(", ")
            .collect::<String>();

        if !missing_fields.is_empty() {
            self.workspace.push(MissingCtorFields {
                missing_fields,
                loc: SourceLoc {
                    origin: self.source,
                    span: ctor.span(),
                },
            })?;
        }

        let final_ty = if params.is_empty() {
            Ty::Struct(struct_ty)
        } else {
            Ty::Instance(
                self.typec
                    .instance(GenericTy::Struct(struct_ty), params, self.interner),
            )
        };

        Some(TirNode::new(
            final_ty,
            TirKind::Ctor(
                builder.arena.alloc_iter(
                    fields
                        .into_iter()
                        .map(|f| f.expect("since missing fields are empty, all fields are some")),
                ),
            ),
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
            Ty::Pointer(ptr) => (ptr.depth, ptr.mutability),
            _ => (0, RawMutability::IMMUTABLE),
        };
        let mut total_mutability = true;
        loop {
            let current_pointed_depth = node.ty.ptr_depth();
            match desired_pointer_depth.cmp(&current_pointed_depth) {
                Ordering::Less => {
                    let ty = self.typec.deref(node.ty);
                    let mutability = node.ty.mutability();
                    let mutable = mutability == RawMutability::MUTABLE && total_mutability;
                    total_mutability = mutable;
                    *node = TirNode::with_flags(
                        ty,
                        TirKind::Deref(builder.arena.alloc(*node)),
                        TirFlags::IMMUTABLE & !mutable,
                        node.span,
                    );
                }
                Ordering::Greater => {
                    if mutability == RawMutability::MUTABLE
                        && node.flags.contains(TirFlags::IMMUTABLE)
                    {
                        self.workspace.push(NotMutable {
                            loc: SourceLoc {
                                origin: self.source,
                                span: node.span,
                            },
                        });
                    }
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

        if node.ty.mutability() != RawMutability::MUTABLE && mutability == RawMutability::MUTABLE {
            self.workspace.push(NotMutable {
                loc: SourceLoc {
                    origin: self.source,
                    span: node.span,
                },
            });
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

    pub fn deref<'a>(
        &mut self,
        expr: UnitExprAst,
        _inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let expr = self.unit_expr(expr, Inference::None, builder)?;
        let Ty::Pointer(ptr) = expr.ty else {
            self.workspace.push(NonPointerDereference {
                ty: self.typec.display_ty(expr.ty, self.interner),
                loc: SourceLoc { origin: self.source, span: expr.span },
            })?;
        };

        let base = self.typec[ptr.ty()];

        Some(TirNode::with_flags(
            base,
            TirKind::Deref(builder.arena.alloc(expr)),
            TirFlags::IMMUTABLE
                & (ptr.mutability == RawMutability::IMMUTABLE
                    || (expr.flags.contains(TirFlags::IMMUTABLE)
                        && !matches!(expr.kind, TirKind::Access(..)))),
            expr.span,
        ))
    }

    pub fn r#ref<'a>(
        &mut self,
        mutability: Option<MutabilityAst>,
        expr: UnitExprAst,
        _inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let expr = self.unit_expr(expr, Inference::None, builder)?;
        let mutability = self.mutability(mutability)?;
        let ptr = self.typec.pointer_to(
            RawMutability::new(mutability).expect("todo"),
            expr.ty,
            self.interner,
        );

        if mutability == Mutability::Mutable && expr.flags.contains(TirFlags::IMMUTABLE) {
            self.workspace.push(NotMutable {
                loc: SourceLoc {
                    origin: self.source,
                    span: expr.span,
                },
            })?;
        }

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
}

ctl_errors! {
    #[err => "field is getting initialized twice"]
    error DuplicateCtorField: fatal {
        #[info loc.origin, prev, "previous initialization was here"]
        #[err loc]
        prev: Span,
        loc: SourceLoc,
    }

    #[err => "missing fields in struct constructor"]
    #[err => "missing: {missing_fields}"]
    error MissingCtorFields: fatal {
        #[err loc]
        missing_fields ref: String,
        loc: SourceLoc,
    }
}

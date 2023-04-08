use super::{
    control_flow::NonPointerDereference,
    lookup::{CannotInferExpression, UnexpectedType, WrongGenericParamCount},
    *,
};

impl<'arena, 'ctx> TirBuilder<'arena, 'ctx> {
    pub(super) fn struct_ctor(
        &mut self,
        ctor @ StructCtorAst { path, body, .. }: StructCtorAst,
        inference: Inference,
    ) -> ExprRes<'arena> {
        let (ty, params) = if let Some(path @ PathAst { slash: None, .. }) = path {
            let (ty, params) = self.parser().ty_path(path)?;
            let ty = match ty {
                TyPathResult::Ty(ty) => ty.base(self.ext.types),
                TyPathResult::Spec(..) => None,
            };
            let params = match params {
                Some(p) => Some((
                    p.span(),
                    p.iter()
                        .map(|&param| self.parser().ty(param))
                        .nsc_collect::<Option<BumpVec<_>>>()?,
                )),
                None => None,
            };
            (ty, params)
        } else {
            match inference
                .ty()
                .map(|ty| ty.to_base_and_params(self.ext.types))
            {
                Some(Ok((ty, params))) => (
                    Some(ty),
                    Some((default(), self.ext.types[params].to_bumpvec())),
                ),
                _ => (None, None),
            }
        };

        let Some(ty) = ty else {
            CannotInferExpression {
                help: "add struct path before the slash",
                loc: self.meta.loc(ctor.span()),
            }.add(self.ext.workspace)?;
        };

        let BaseTy::Struct(struct_ty) = ty else {
            UnexpectedType {
                expected: "struct",
                found: self.ext.creator().display(ty),
                loc: self.meta.loc(ctor.span()),
            }.add(self.ext.workspace)?;
        };

        let struct_meta = self.ext.types[struct_ty];

        let mut param_slots = bumpvec![None; struct_meta.generics.len()];

        if let Some((span, ref params)) = params {
            if params.len() > param_slots.len() {
                WrongGenericParamCount {
                    expected: param_slots.len(),
                    found: params.len(),
                    loc: self.meta.loc(span),
                }
                .add(self.ext.workspace);
            }

            param_slots
                .iter_mut()
                .zip(params.iter().copied())
                .for_each(|(slot, param)| *slot = Some(param))
        }

        let mut fields = bumpvec![None; struct_meta.fields.len()];

        let params = params.map_or(default(), |(.., p)| p);

        for field_ast @ &StructCtorFieldAst { name, value, .. } in body.iter() {
            let Some((index, .., ty)) = self.find_field(struct_ty, params.as_slice(), name) else {
                continue;
            };

            let inference = self.ext.creator().try_instantiate(ty, &param_slots);
            let expr = if let Some((.., expr)) = value {
                self.expr(expr, inference.into())
            } else {
                self.value_path(
                    PathAst {
                        start: PathSegmentAst::Name(name),
                        segments: &[],
                        slash: None,
                    },
                    inference.into(),
                )
            };
            let Some(value) = expr else {
                continue;
            };

            self.infer_params(&mut param_slots, value.ty, ty, field_ast.span());

            if let Some(prev) = fields[index].replace(value) {
                DuplicateCtorField {
                    prev: prev.span,
                    loc: self.meta.loc(name.span),
                }
                .add(self.ext.workspace);
            }
        }

        let params = self.unpack_param_slots(
            param_slots.iter().copied(),
            ctor.span(),
            "struct constructor",
            "(<struct_path>\\[<param_ty>, ...]\\{...})",
        )?;

        let missing_fields = self.ext.types[struct_meta.fields]
            .iter()
            .zip(fields.iter())
            .filter_map(|(field, value)| value.is_none().then_some(&field.name))
            .map(|name| name.get(self.ext.interner))
            .intersperse(", ")
            .collect::<String>();

        if !missing_fields.is_empty() {
            MissingCtorFields {
                missing_fields,
                loc: self.meta.loc(ctor.span()),
            }
            .add(self.ext.workspace)?;
        }

        let final_ty = if params.is_empty() {
            struct_ty.into()
        } else {
            Ty::Node(Node::Instance(
                self.ext
                    .creator()
                    .instance(BaseTy::Struct(struct_ty), params),
            ))
        };

        Some(TirNode::new(
            final_ty,
            TirKind::Ctor(
                self.arena.alloc_iter(
                    fields
                        .into_iter()
                        .map(|f| f.expect("since missing fields are empty, all fields are some")),
                ),
            ),
            ctor.span(),
        ))
    }

    pub(super) fn balance_pointers(&mut self, node: &mut TirNode<'arena>, ty: Ty) -> Option<()> {
        let (desired_pointer_depth, mutability) = match ty {
            Ty::Pointer(ptr) => (ptr.depth, ptr.mutability),
            _ => (0, RawMutability::IMMUTABLE),
        };
        let mut total_mutability = true;
        loop {
            let current_pointed_depth = node.ty.ptr_depth();
            match desired_pointer_depth.cmp(&current_pointed_depth) {
                Ordering::Less => {
                    let ty = self.ext.types.dereference(node.ty);
                    let mutability = node.ty.mutability();
                    let mutable = mutability == RawMutability::MUTABLE && total_mutability;
                    total_mutability = mutable;
                    *node = TirNode::with_flags(
                        ty,
                        TirKind::Deref(self.arena.alloc(*node)),
                        TirFlags::IMMUTABLE & !mutable,
                        node.span,
                    );
                }
                Ordering::Greater => {
                    if mutability == RawMutability::MUTABLE
                        && node.flags.contains(TirFlags::IMMUTABLE)
                    {
                        NotMutable {
                            loc: self.meta.loc(node.span),
                        }
                        .add(self.ext.workspace);
                    }
                    let ty = self.ext.creator().pointer_to(mutability, node.ty);
                    *node = TirNode::new(
                        Ty::Pointer(ty),
                        TirKind::Ref(self.arena.alloc(*node)),
                        node.span,
                    );
                }
                Ordering::Equal => break,
            }
        }

        if node.ty.mutability() != RawMutability::MUTABLE && mutability == RawMutability::MUTABLE {
            NotMutable {
                loc: self.meta.loc(node.span),
            }
            .add(self.ext.workspace);
        }

        Some(())
    }

    pub(super) fn int(&mut self, span: Span, inference: Inference) -> ExprRes<'arena> {
        let span_str = self.span_str(span);
        let (ty, postfix_len) =
            Self::infer_constant_type(span_str, inference, &Builtin::INTEGERS, Builtin::UINT);
        Some(TirNode::new(
            ty,
            TirKind::Int(None),
            span.sliced(..span_str.len() - postfix_len),
        ))
    }

    pub(super) fn float(&mut self, span: Span, inference: Inference) -> ExprRes<'arena> {
        let span_str = self.span_str(span);
        let (ty, postfix_len) =
            Self::infer_constant_type(span_str, inference, &Builtin::FLOATS, Builtin::F32);
        Some(TirNode::new(
            ty,
            TirKind::Float(None),
            span.sliced(..span_str.len() - postfix_len),
        ))
    }

    fn span_str(&self, span: Span) -> &'ctx str {
        self.meta.span_str(span, self.ext.resources)
    }

    fn infer_constant_type(
        value: &str,
        inference: Inference,
        group: &[Builtin],
        default: Builtin,
    ) -> (Ty, usize) {
        let (b, i) = group
            .iter()
            .map(|&ty| (ty, ty.name()))
            .find_map(|(ty, str)| value.ends_with(str).then_some((ty, str.len())))
            .or_else(|| {
                inference
                    .ty()
                    .and_then(|ty| match ty {
                        Ty::Builtin(builtin) => Some(builtin),
                        _ => None,
                    })
                    .filter(|ty| group.contains(ty))
                    .map(|ty| (ty, 0))
            })
            .unwrap_or((default, 0));
        (Ty::Builtin(b), i)
    }

    pub(super) fn char(&mut self, span: Span) -> ExprRes<'arena> {
        Some(TirNode::new(Ty::CHAR, TirKind::Char, span))
    }

    pub(super) fn bool(&mut self, span: Span) -> ExprRes<'static> {
        Some(TirNode::new(
            Ty::BOOL,
            TirKind::Bool(self.span_str(span).starts_with('t')),
            span,
        ))
    }

    pub(super) fn deref(&mut self, expr: UnitExprAst, _inference: Inference) -> ExprRes<'arena> {
        let expr = self.unit_expr(expr, Inference::None)?;
        let Ty::Pointer(ptr) = expr.ty else {
            NonPointerDereference {
                ty: self.ext.creator().display(expr.ty),
                loc: self.meta.loc(expr.span),
            }.add(self.ext.workspace)?;
        };

        let base = self.ext.types[ptr.ty()];

        Some(TirNode::with_flags(
            base,
            TirKind::Deref(self.arena.alloc(expr)),
            TirFlags::IMMUTABLE
                & (ptr.mutability == RawMutability::IMMUTABLE
                    || (expr.flags.contains(TirFlags::IMMUTABLE)
                        && !matches!(expr.kind, TirKind::Access(..)))),
            expr.span,
        ))
    }

    pub(super) fn r#ref(
        &mut self,
        mutability: Option<MutabilityAst>,
        expr: UnitExprAst,
        _inference: Inference,
    ) -> ExprRes<'arena> {
        let expr = self.unit_expr(expr, Inference::None)?;
        let mutability = self.parser().mutability(mutability)?;
        let ptr = self
            .ext
            .creator()
            .pointer_to(RawMutability::new(mutability).expect("todo"), expr.ty);

        if mutability == Mutability::Mutable && expr.flags.contains(TirFlags::IMMUTABLE) {
            NotMutable {
                loc: self.meta.loc(expr.span),
            }
            .add(self.ext.workspace)?;
        }

        Some(TirNode::new(
            Ty::Pointer(ptr),
            TirKind::Ref(self.arena.alloc(expr)),
            expr.span,
        ))
    }

    pub(super) fn r#let(
        &mut self,
        r#let @ LetAst { pat, ty, value, .. }: LetAst,
        _inference: Inference,
    ) -> ExprRes<'arena> {
        let ty = ty.map(|(.., ty)| self.parser().ty(ty)).transpose()?;
        let value = self.expr(value, ty.into())?;
        let pat = self.pattern(pat, value.ty)?;

        Some(TirNode::new(
            Ty::UNIT,
            TirKind::Let(self.arena.alloc(LetTir { pat, value })),
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

use super::*;

impl TyChecker<'_> {
    pub fn find_loop(
        &mut self,
        label: Option<NameAst>,
        builder: &mut TirBuilder,
    ) -> OptVRef<LoopHeaderTir> {
        let Some(label) = label else {
            return builder.ctx.loops
                .iter()
                .rev()
                .find_map(|(key, header)| header.label.is_none().then_some(key));
        };

        Some(lookup!(LoopHeaderTir self, label.ident, label.span))
    }

    pub fn enum_path<'a>(
        &mut self,
        path: PathAst<'a>,
        inference: Inference,
    ) -> Option<(FragRef<Enum>, NameAst, Option<TyGenericsAst<'a>>)> {
        pub fn resolve<'a>(
            s: &mut TyChecker,
            enum_ty: FragRef<Enum>,
            segments: &[PathItemAst<'a>],
            backup_span: Span,
        ) -> Option<(FragRef<Enum>, NameAst, Option<TyGenericsAst<'a>>)> {
            Some(match *segments {
                [PathItemAst::Ident(name)] => (enum_ty, name, None),
                [PathItemAst::Params(params), PathItemAst::Ident(name)] => {
                    (enum_ty, name, Some(params))
                }
                ref other => s.workspace.push(InvalidPathSegment {
                    loc: SourceLoc {
                        origin: s.source,
                        span: segments
                            .iter()
                            .map(|s| s.span())
                            .reduce(|a, b| a.joined(b))
                            .unwrap_or(backup_span),
                    },
                    message: "expected optional generics followed by an ident (enum-variant name)",
                })?,
            })
        }

        if path.slash.is_some() {
            let Some(expected) = inference.ty() else {
                self.workspace.push(CannotInferExpression {
                    help: "add an enum type-name before the leading '\\'",
                    loc: SourceLoc { origin: self.source, span: path.span() },
                })?;
            };

            let Ty::Enum(enum_ty) = expected.base(self.typec) else {
                self.workspace.push(UnexpectedInferenceType {
                    expected: "enum",
                    found: self.typec.display_ty(expected, self.interner),
                    loc: SourceLoc { origin: self.source, span: path.span() },
                })?;
            };

            let PathItemAst::Ident(name) = path.start else {
                self.workspace.push(InvalidPathSegment {
                    loc: SourceLoc { origin: self.source, span: path.start.span() },
                    message: "expected an ident (enum-variant name)",
                })?;
            };

            return Some((enum_ty, name, grab_trailing_params(path.segments)));
        }

        let PathItemAst::Ident(name) = path.start else {
            self.workspace.push(InvalidPathSegment {
                loc: SourceLoc { origin: self.source, span: path.start.span() },
                message: "expected an ident (enum or module)",
            })?;
        };

        let module = match self.lookup(name.ident, name.span, "module or enum")? {
            ScopeItem::Ty(Ty::Enum(enum_ty)) => {
                return resolve(self, enum_ty, path.segments, name.span);
            }
            ScopeItem::Module(module) => module,
            item => self.invalid_symbol_type(item, name.span, "module or enum")?,
        };

        let &[PathItemAst::Ident(r#enum), ref segments @ ..] = path.segments else {
            self.workspace.push(InvalidPathSegment {
                loc: SourceLoc { origin: self.source, span: path.after_start_span() },
                message: "expected an enum name",
            })?;
        };

        let id = self.interner.intern_scoped(module.index(), r#enum.ident);
        let enum_ty = match self.lookup(id, r#enum.span, "enum")? {
            ScopeItem::Ty(Ty::Enum(enum_ty)) => enum_ty,
            item => self.invalid_symbol_type(item, r#enum.span, "enum")?,
        };

        resolve(self, enum_ty, segments, r#enum.span)
    }

    pub fn dot_path(
        &mut self,
        ty: Ty,
        path @ PathAst { slash, start, .. }: PathAst,
        _builder: &mut TirBuilder,
    ) -> Option<DotPathResult> {
        if slash.is_some() {
            todo!();
        }

        let Ty::Struct(struct_id) = ty.base(self.typec) else {
            self.non_struct_field_access(ty, path.span())?;
        };

        let PathItemAst::Ident(name) = start else {
            todo!();
        };

        let Some((field, Field { ty: mut field_ty, .. })) = Struct::find_field(struct_id, name.ident, self.typec) else {
            self.field_not_found(struct_id, name)?;
        };

        if let Ty::Instance(instance) = ty {
            field_ty = self
                .typec
                .instantiate(ty, self.typec[instance].args, self.interner);
        }

        Some(DotPathResult::Field(field as u32, field_ty))
    }

    pub fn find_struct_field(
        &self,
        struct_id: FragRef<Struct>,
        field_name: Ident,
    ) -> Option<(usize, FragRef<Field>, Ty)> {
        let Struct { fields, .. } = self.typec[struct_id];
        self.typec
            .fields
            .indexed(fields)
            .enumerate()
            .find_map(|(i, (id, field))| (field.name == field_name).then_some((i, id, field.ty)))
    }

    pub fn func_path<'a, 'b>(
        &mut self,
        path @ PathAst {
            start, segments, ..
        }: PathAst<'b>,
        builder: &mut TirBuilder<'a, '_>,
    ) -> Option<(FuncLookupResult<'a>, Option<Ty>, Option<TyGenericsAst<'b>>)> {
        let PathItemAst::Ident(start) = start else {
            todo!();
        };

        let module = match self.lookup(start.ident, start.span, FUNC_OR_MOD)? {
            ScopeItem::Func(func) => {
                return Some((
                    FuncLookupResult::Func(func),
                    None,
                    grab_trailing_params(segments),
                ))
            }
            ScopeItem::Module(module) => module,
            ScopeItem::Ty(ty) => {
                let (ty, segments) = match (ty.as_generic(), segments) {
                    (Some(ty), &[PathItemAst::Params(params), ref segments @ ..]) => {
                        let params = params
                            .iter()
                            .map(|&p| self.ty(p))
                            .nsc_collect::<Option<BumpVec<_>>>()?;
                        (
                            Ty::Instance(self.typec.instance(ty, &params, self.interner)),
                            segments,
                        )
                    }
                    _ => (ty, segments),
                };

                let &[start, ref segments @ ..] = segments else {
                    self.invalid_expr_path(path.span())?
                };
                return self.method_path(
                    ty,
                    PathAst {
                        start,
                        segments,
                        slash: None,
                    },
                    builder,
                );
            }
            item => self.invalid_symbol_type(item, start.span, FUNC_OR_MOD)?,
        };

        let &[PathItemAst::Ident(func_or_type), ref segments @ ..] = segments else {
            self.invalid_expr_path(path.span())?
        };

        let id = self
            .interner
            .intern_scoped(module.as_u32(), func_or_type.ident);
        let (ty, segments) = match self.lookup(id, func_or_type.span, FUNC)? {
            ScopeItem::Func(func) => {
                return Some((
                    FuncLookupResult::Func(func),
                    None,
                    grab_trailing_params(segments),
                ))
            }
            ScopeItem::Ty(ty) => match (ty.as_generic(), segments) {
                (Some(ty), &[PathItemAst::Params(params), ref segments @ ..]) => {
                    let params = params
                        .iter()
                        .map(|&p| self.ty(p))
                        .nsc_collect::<Option<BumpVec<_>>>()?;
                    (
                        Ty::Instance(self.typec.instance(ty, &params, self.interner)),
                        segments,
                    )
                }
                _ => (ty, segments),
            },
            item => self.invalid_symbol_type(item, func_or_type.span, FUNC)?,
        };
        let &[start, ref segments @ ..] = segments else {
            self.invalid_expr_path(path.span())?
        };
        self.method_path(
            ty,
            PathAst {
                start,
                segments,
                slash: None,
            },
            builder,
        )
    }

    pub fn method_path<'a, 'b>(
        &mut self,
        ty: Ty,
        path @ PathAst {
            start, segments, ..
        }: PathAst<'b>,
        builder: &mut TirBuilder<'a, '_>,
    ) -> Option<(FuncLookupResult<'a>, Option<Ty>, Option<TyGenericsAst<'b>>)> {
        let lty = ty.caller(self.typec);
        let PathItemAst::Ident(ident) = start else {
            self.invalid_expr_path(path.span())?
        };

        let (module, spec_base_or_method, segments) = match self.scope.get(ident.ident) {
            Ok(ScopeItem::SpecBase(spec)) => (None, Ok(spec), segments),
            Ok(ScopeItem::Module(module)) => {
                let &[PathItemAst::Ident(spec), ref segments @ ..] = segments else {
                    self.invalid_expr_path(path.span())?
                };

                let id = self.interner.intern_scoped(module.index(), spec.ident);
                match self.scope.get(id) {
                    Ok(ScopeItem::SpecBase(spec)) => (Some(module), Ok(spec), segments),
                    _ => (Some(module), Err(spec), segments),
                }
            }
            _ => (None, Err(ident), segments),
        };

        let spec_base = match spec_base_or_method {
            Ok(spec) => spec,
            Err(method) => {
                let local_id = self.interner.intern_scoped(lty, method.ident);
                let id = module.map_or(local_id, |m| {
                    self.interner.intern_scoped(m.index(), local_id)
                });
                return match self.lookup(id, path.span(), FUNC)? {
                    ScopeItem::Func(func) => Some((
                        FuncLookupResult::Func(func),
                        Some(ty),
                        grab_trailing_params(segments),
                    )),
                    ScopeItem::SpecFunc(func) => Some((
                        FuncLookupResult::SpecFunc(func, lty),
                        Some(ty),
                        grab_trailing_params(segments),
                    )),
                    item => self.invalid_symbol_type(item, path.span(), FUNC)?,
                };
            }
        };

        let (spec, segments) = if let &[PathItemAst::Params(params), ref segments @ ..] = segments {
            let params = params
                .iter()
                .map(|&p| self.ty(p))
                .nsc_collect::<Option<BumpVec<_>>>()?;
            (
                Spec::Instance(self.typec.spec_instance(spec_base, &params, self.interner)),
                segments,
            )
        } else {
            (Spec::Base(spec_base), segments)
        };

        let &[PathItemAst::Ident(method), ref segments @ ..] = segments else {
            self.invalid_expr_path(path.span())?
        };

        let (method_index, method) = self
            .typec
            .spec_funcs
            .indexed(self.typec[spec_base].methods)
            .enumerate()
            .find_map(|(i, (key, func))| (method.ident == func.name).then_some((i, key)))
            .or_else(|| todo!())?;

        if let Some(r#impl) = self.typec.find_implementation(
            ty,
            spec,
            builder.ctx.generics.as_slice(),
            &mut None,
            self.interner,
        ) {
            if let Some((r#impl, _)) = r#impl {
                let func = self.typec[self.typec[r#impl].methods][method_index];
                return Some((
                    FuncLookupResult::Func(func),
                    Some(ty),
                    grab_trailing_params(segments),
                ));
            }
        } else {
            todo!(
                "{:?} {:?} {:?}",
                ty,
                spec,
                builder
                    .ctx
                    .generics
                    .iter()
                    .map(|&g| &self.typec[g])
                    .collect::<Vec<_>>()
            );
        }

        Some((
            FuncLookupResult::SpecFunc(method, lty),
            Some(ty),
            grab_trailing_params(segments),
        ))
    }

    pub fn value_path<'a>(
        &mut self,
        path @ PathAst { slash, start, .. }: PathAst,
        inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        if slash.is_some() {
            let Some(inferred) = inference.ty() else {
                self.workspace.push(CannotInferExpression {
                    help: "Try specifying the type before '\\'",
                    loc: SourceLoc { origin: self.source, span: path.span() },
                })?;
            };

            match inferred.base(self.typec) {
                Ty::Enum(..) => {
                    return self.enum_ctor(EnumCtorAst { path, value: None }, inference, builder)
                }
                Ty::Struct(_) => todo!(),
                Ty::Instance(_) => todo!(),
                Ty::Pointer(_) => todo!(),
                Ty::Param(_) => todo!(),
                Ty::Builtin(_) => todo!(),
            }
        }

        let PathItemAst::Ident(start) = start else {
            self.invalid_expr_path(path.span())?
        };

        let res = match self.lookup(start.ident, start.span, "variable or enum")? {
            ScopeItem::Func(_) => todo!(),
            ScopeItem::SpecFunc(_) => todo!(),
            ScopeItem::Ty(ty) => match ty {
                Ty::Struct(_) => todo!(),
                Ty::Enum(..) => {
                    return self.enum_ctor(EnumCtorAst { path, value: None }, inference, builder)
                }
                Ty::Instance(_) => todo!(),
                Ty::Pointer(_) => todo!(),
                Ty::Param(_) => todo!(),
                Ty::Builtin(_) => todo!(),
            },
            ScopeItem::SpecBase(_) => todo!(),
            ScopeItem::VarHeaderTir(var) => {
                TirNode::new(builder.get_var(var).ty, TirKind::Access(var), path.span())
            }
            item => self.invalid_symbol_type(item, start.span, "variable or enum")?,
        };

        Some(res)
    }

    pub fn find_binary_func(&mut self, op: NameAst, lhs_ty: Ty, rhs_ty: Ty) -> OptFragRef<Func> {
        let base_id = op.ident;
        let id = self
            .interner
            .intern_with(|s, t| self.typec.binary_op_id(base_id, lhs_ty, rhs_ty, t, s));
        Some(lookup!(Func self, id, op.span()))
    }
}

ctl_errors! {
    #[err => "cannot infer type of expression"]
    #[help => "{help}"]
    error CannotInferExpression: fatal {
        #[err loc]
        help: &'static str,
        loc: SourceLoc,
    }

    #[err => "inferred type of expression is not a {expected}"]
    #[info => "the inferred type is '{found}' which is not a {expected}"]
    error UnexpectedInferenceType: fatal {
        #[err loc]
        expected: &'static str,
        found ref: String,
        loc: SourceLoc,
    }

    #[err => "invalid path segment"]
    #[note => "{message}"]
    error InvalidPathSegment: fatal {
        #[err loc]
        message: &'static str,
        loc: SourceLoc,
    }
}

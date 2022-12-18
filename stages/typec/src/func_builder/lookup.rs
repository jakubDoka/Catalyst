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

            return Some((
                enum_ty,
                name,
                self.grab_trailing_params(path.segments, name.span.as_end()),
            ));
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
        let (Ty::Struct(struct_ty), params) = ty.base_with_params(self.typec) else {
            self.workspace.push(FieldAccessOnNonStruct {
                found: self.typec.display_ty(ty, self.interner),
                loc: SourceLoc { origin: self.source, span: path.span() },
            })?;
        };

        self.assert_no_slash(slash)?;

        let PathItemAst::Ident(name) = start else {
            self.workspace.push(InvalidPathSegment {
                loc: SourceLoc { origin: self.source, span: start.span() },
                message: "expected a field name",
            })?;
        };

        let (field, field_ty) = self.find_field(struct_ty, params, name)?;

        Some(DotPathResult::Field(field as u32, field_ty))
    }

    pub fn func_path<'a, 'b>(
        &mut self,
        path @ PathAst {
            start,
            segments,
            slash,
            ..
        }: PathAst<'b>,
        builder: &mut TirBuilder<'a, '_>,
    ) -> Option<(FuncLookupResult<'a>, Option<Ty>, Option<TyGenericsAst<'b>>)> {
        self.assert_no_slash(slash)?;

        let PathItemAst::Ident(start) = start else {
            self.workspace.push(InvalidPathSegment {
                loc: SourceLoc { origin: self.source, span: start.span() },
                message: "expected ident (function, module or type)",
            })?;
        };

        let module = match self.lookup(start.ident, start.span, FUNC_OR_MOD)? {
            ScopeItem::Func(func) => {
                return Some((
                    FuncLookupResult::Func(func),
                    None,
                    self.grab_trailing_params(segments, start.span.as_end()),
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
                    self.workspace.push(InvalidPathSegment {
                        loc: SourceLoc {
                            origin: self.source,
                            span: path.after_start_span(), // TODO: make this bit more accurate
                        },
                        message: "expected a function name, module or spec name",
                    })?;
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
            self.workspace.push(InvalidPathSegment {
                loc: SourceLoc {
                    origin: self.source,
                    span: path.after_start_span(),
                },
                message: "expected a ident (function or type)",
            })?;
        };

        let id = self
            .interner
            .intern_scoped(module.as_u32(), func_or_type.ident);
        let (ty, segments) = match self.lookup(id, func_or_type.span, FUNC)? {
            ScopeItem::Func(func) => {
                return Some((
                    FuncLookupResult::Func(func),
                    None,
                    self.grab_trailing_params(segments, func_or_type.span.as_end()),
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
            self.workspace.push(InvalidPathSegment {
                loc: SourceLoc {
                    origin: self.source,
                    span: path.after_start_span(),
                },
                message: "expected ident (function, spec or module)",
            })?;
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
            slash,
            start,
            segments,
            ..
        }: PathAst<'b>,
        builder: &mut TirBuilder<'a, '_>,
    ) -> Option<(FuncLookupResult<'a>, Option<Ty>, Option<TyGenericsAst<'b>>)> {
        self.assert_no_slash(slash)?;

        let lty = ty.caller(self.typec);
        let PathItemAst::Ident(ident) = start else {
            self.workspace.push(InvalidPathSegment {
                loc: SourceLoc { origin: self.source, span: start.span() },
                message: "expected ident (function or spec)",
            })?;
        };

        let (module, spec_base_or_method, segments) = match self.scope.get(ident.ident) {
            Ok(ScopeItem::SpecBase(spec)) => (None, Ok(spec), segments),
            Ok(ScopeItem::Module(module)) => {
                let &[PathItemAst::Ident(spec_or_method), ref segments @ ..] = segments else {
                    self.workspace.push(InvalidPathSegment {
                        loc: SourceLoc {
                            origin: self.source,
                            span: path.after_start_span(),
                        },
                        message: "expected ident (method or spec)",
                    })?;
                };

                let id = self
                    .interner
                    .intern_scoped(module.index(), spec_or_method.ident);
                match self.scope.get(id) {
                    Ok(ScopeItem::SpecBase(spec)) => (Some(module), Ok(spec), segments),
                    _ => (Some(module), Err(spec_or_method), segments),
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
                let func = match self.lookup(id, method.span(), "method")? {
                    ScopeItem::Func(func) => FuncLookupResult::Func(func),
                    ScopeItem::SpecFunc(func) => FuncLookupResult::SpecFunc(func, lty),
                    item => self.invalid_symbol_type(item, path.span(), "method")?,
                };
                return Some((
                    func,
                    Some(ty),
                    self.grab_trailing_params(segments, method.span().as_end()),
                ));
            }
        };

        let (spec, start, segments) =
            if let &[start @ PathItemAst::Params(params), ref segments @ ..] = segments {
                let params = params
                    .iter()
                    .map(|&p| self.ty(p))
                    .nsc_collect::<Option<BumpVec<_>>>()?;

                if params.len() != self.typec[spec_base].generics.len() {
                    self.workspace.push(WrongGenericParamCount {
                        expected: self.typec[spec_base].generics.len(),
                        found: params.len(),
                        loc: SourceLoc {
                            origin: self.source,
                            span: path.after_start_span(),
                        },
                    })?;
                }

                (
                    Spec::Instance(self.typec.spec_instance(spec_base, &params, self.interner)),
                    start,
                    segments,
                )
            } else {
                (Spec::Base(spec_base), path.start, segments)
            };

        let &[PathItemAst::Ident(method_ident), ref segments @ ..] = segments else {
            self.workspace.push(InvalidPathSegment {
                loc: SourceLoc {
                    origin: self.source,
                    span: start.span().as_end(),
                },
                message: "expected ident (method)",
            })?;
        };

        let (method_index, method) = self
            .typec
            .spec_funcs
            .indexed(self.typec[spec_base].methods)
            .enumerate()
            .find_map(|(i, (key, func))| (method_ident.ident == func.name).then_some((i, key)))
            .or_else(|| {
                self.workspace.push(ComponentNotFound {
                    ty: self.typec.display_spec(spec, self.interner),
                    loc: SourceLoc {
                        origin: self.source,
                        span: method_ident.span(),
                    },
                    suggestions: self.typec[self.typec[spec_base].methods]
                        .iter()
                        .map(|func| &self.interner[func.name])
                        .intersperse(", ")
                        .collect(),
                    something: "method",
                })?
            })?;

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
                    self.grab_trailing_params(segments, method_ident.span().as_end()),
                ));
            }
        } else {
            self.workspace.push(MissingImpl {
                ty: self.typec.display_ty(ty, self.interner),
                spec: self.typec.display_spec(spec, self.interner),
                loc: SourceLoc {
                    origin: self.source,
                    span: path.span(),
                },
            })?;
        }

        Some((
            FuncLookupResult::SpecFunc(method, lty),
            Some(ty),
            self.grab_trailing_params(segments, method_ident.span().as_end()),
        ))
    }

    pub fn assert_no_slash(&mut self, slash: Option<Span>) -> Option<()> {
        if let Some(slash) = slash {
            self.workspace.push(InvalidPathSegment {
                loc: SourceLoc {
                    origin: self.source,
                    span: slash,
                },
                message: "leading slash in this context is invalid",
            })?;
        }
        Some(())
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
                Ty::Struct(..)
                | Ty::Instance(..)
                | Ty::Pointer(..)
                | Ty::Param(..)
                | Ty::Builtin(..) => self.workspace.push(UnexpectedInferenceType {
                    expected: "enum",
                    found: self.typec.display_ty(inferred, self.interner),
                    loc: SourceLoc {
                        origin: self.source,
                        span: path.span(),
                    },
                })?,
            }
        }

        let PathItemAst::Ident(start) = start else {
            self.workspace.push(InvalidPathSegment {
                loc: SourceLoc {
                    origin: self.source,
                    span: path.start.span().as_end(),
                },
                message: "expected ident (variable or enum)",
            })?;
        };

        let res = match self.lookup(start.ident, start.span, "variable or enum")? {
            ScopeItem::Ty(ty) => match ty {
                Ty::Enum(..) => {
                    return self.enum_ctor(EnumCtorAst { path, value: None }, inference, builder)
                }
                Ty::Struct(..)
                | Ty::Instance(..)
                | Ty::Pointer(..)
                | Ty::Param(..)
                | Ty::Builtin(..) => self.workspace.push(UnexpectedInferenceType {
                    expected: "enum",
                    found: self.typec.display_ty(ty, self.interner),
                    loc: SourceLoc {
                        origin: self.source,
                        span: path.span(),
                    },
                })?,
            },
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

    pub fn find_field(
        &mut self,
        struct_ty: FragRef<Struct>,
        params: impl TypecCtxSlice<Ty>,
        name: NameAst,
    ) -> Option<(usize, Ty)> {
        self.typec
            .find_struct_field(struct_ty, params, name.ident, self.interner)
            .map(|(field_id, .., field_ty)| (field_id, field_ty))
            .or_else(|| {
                self.workspace.push(ComponentNotFound {
                    loc: SourceLoc {
                        origin: self.source,
                        span: name.span,
                    },
                    ty: self.typec.display_ty(Ty::Struct(struct_ty), self.interner),
                    suggestions: self.typec[self.typec[struct_ty].fields]
                        .iter()
                        .map(|f| &self.interner[f.name])
                        .intersperse(", ")
                        .collect(),
                    something: "field",
                })?
            })
    }

    pub fn grab_trailing_params<'a>(
        &mut self,
        segments: &[PathItemAst<'a>],
        backup_span: Span,
    ) -> Option<TyGenericsAst<'a>> {
        match *segments {
            [] => None,
            [PathItemAst::Params(params)] => Some(params),
            ref segments => self.workspace.push(InvalidPathSegment {
                message: "expected parameters or end of the path",
                loc: SourceLoc {
                    origin: self.source,
                    span: segments.last().unwrap().span(),
                },
            })?,
        }
    }
}

ctl_errors! {
    #[err => "'{ty}' does not implement '{spec}'"]
    error MissingImpl: fatal {
        #[err loc]
        ty ref: String,
        spec ref: String,
        loc: SourceLoc,
    }

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

    #[err => "invalid path segment(s)"]
    #[note => "{message}"]
    error InvalidPathSegment: fatal {
        #[err loc]
        message: &'static str,
        loc: SourceLoc,
    }

    #[err => "field access on non-struct type"]
    #[info => "the type is '{found}' which is not a struct"]
    error FieldAccessOnNonStruct: fatal {
        #[err loc]
        found ref: String,
        loc: SourceLoc,
    }

    #[err => "wrong number of generic parameters"]
    #[info => "expected {expected} parameters, found {found}"]
    error WrongGenericParamCount: fatal {
        #[err loc]
        expected: usize,
        found: usize,
        loc: SourceLoc,
    }
}

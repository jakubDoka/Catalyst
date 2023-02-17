use {super::*, lexing::*};

impl<'arena, 'ctx> TirBuilder<'arena, 'ctx> {
    pub(super) fn find_loop(&mut self, label: Option<NameAst>) -> OptVRef<LoopHeaderTir> {
        let Some(label) = label else {
            return self.ctx.first_unlabeled_loop().or_else(|| {
                self.ext.workspace.push(TodoSnippet {
                    loc: self.meta.loc(label.span()),
                    message: "no unlabeled loop to break from".into(),
                })?
            });
        };

        Some(lookup!(LoopHeaderTir self, label.ident, label.span))
    }

    pub(super) fn enum_path<'a>(
        &mut self,
        path: PathAst<'a>,
        inference: Inference,
    ) -> Option<(FragRef<Enum>, NameAst, Option<ListAst<'a, TyAst<'a>>>)> {
        fn resolve<'arena>(
            s: &mut TirBuilder<'_, '_>,
            enum_ty: FragRef<Enum>,
            segments: &[PathSegmentAst<'arena>],
            backup_span: Span,
        ) -> Option<(
            FragRef<Enum>,
            NameAst,
            Option<ListAst<'arena, TyAst<'arena>>>,
        )> {
            Some(match *segments {
                [PathSegmentAst::Name(name)] => (enum_ty, name, None),
                [PathSegmentAst::Params(params), PathSegmentAst::Name(name)] => {
                    (enum_ty, name, Some(params))
                }
                _ => s.ext.workspace.push(InvalidPathSegment {
                    loc: s.meta.loc(
                        segments
                            .iter()
                            .map(|s| s.span())
                            .reduce(|a, b| a.joined(b))
                            .unwrap_or(backup_span),
                    ),
                    message: "expected optional generics followed by an ident (enum-variant name)",
                })?,
            })
        }

        if path.slash.is_some() {
            let Some(expected) = inference.ty() else {
                CannotInferExpression {
                    help: "add an enum type-name before the leading '\\'",
                    loc: self.meta.loc(path.span()),
                }.add(self.ext.workspace)?;
            };

            let Ty::Enum(enum_ty) = expected.base(self.ext.typec) else {
                UnexpectedInferenceType {
                    expected: "enum",
                    found: self.ext.creator().display(expected),
                    loc: self.meta.loc(path.span()),
                }.add(self.ext.workspace)?;
            };

            let PathSegmentAst::Name(name) = path.start else {
                InvalidPathSegment {
                    loc: self.meta.loc(path.start.span()),
                    message: "expected an ident (enum-variant name)",
                }.add(self.ext.workspace)?;
            };

            return Some((
                enum_ty,
                name,
                self.grab_trailing_params(path.segments, name.span.as_end()),
            ));
        }

        let PathSegmentAst::Name(name) = path.start else {
            InvalidPathSegment {
                loc: self.meta.loc(path.start.span()),
                message: "expected an ident (enum or module)",
            }.add(self.ext.workspace)?;
        };

        let module = match self.lookup(name.ident, name.span, "module or enum")? {
            ScopeItem::Ty(Ty::Enum(enum_ty)) => {
                return resolve(self, enum_ty, path.segments, name.span);
            }
            ScopeItem::Module(module) => module,
            item => self.invalid_symbol_type(item, name.span, "module or enum")?,
        };

        let &[PathSegmentAst::Name(r#enum), ref segments @ ..] = path.segments else {
            InvalidPathSegment {
                loc: self.meta.loc(Self::segment_slice_span_low(name.span, path.segments)),
                message: "expected an enum name",
            }.add(self.ext.workspace)?;
        };

        let id = self
            .ext
            .interner
            .intern_scoped(module.index(), r#enum.ident);
        let enum_ty = match self.lookup(id, r#enum.span, "enum")? {
            ScopeItem::Ty(Ty::Enum(enum_ty)) => enum_ty,
            item => self.invalid_symbol_type(item, r#enum.span, "enum")?,
        };

        resolve(self, enum_ty, segments, r#enum.span)
    }

    pub(super) fn dot_path(
        &mut self,
        ty: Ty,
        path @ PathAst { slash, start, .. }: PathAst,
    ) -> Option<DotPathResult> {
        let (Ty::Struct(struct_ty), params) = ty.base_with_params(self.ext.typec) else {
            FieldAccessOnNonStruct {
                found: self.ext.creator().display(ty),
                loc: self.meta.loc(path.span()),
            }.add(self.ext.workspace)?;
        };

        self.assert_no_slash(slash)?;

        let PathSegmentAst::Name(name) = start else {
            InvalidPathSegment {
                loc: self.meta.loc(start.span()),
                message: "expected a field name",
            }.add(self.ext.workspace)?;
        };

        let (field, mutable, field_ty) = self.find_field(struct_ty, params, name)?;

        Some(DotPathResult::Field(field as u32, mutable, field_ty))
    }

    pub(super) fn func_path<'b>(
        &mut self,
        PathAst {
            start,
            segments,
            slash,
        }: PathAst<'b>,
    ) -> Option<(
        FuncLookupResult<'arena>,
        Option<Ty>,
        Option<ListAst<'b, TyAst<'b>>>,
    )> {
        self.assert_no_slash(slash)?;

        let PathSegmentAst::Name(start) = start else {
            InvalidPathSegment {
                loc: self.meta.loc(start.span()),
                message: "expected ident (function, module or type)",
            }.add(self.ext.workspace)?;
        };

        let module = match self.lookup(start.ident, start.span, "function or module")? {
            ScopeItem::Func(func) => {
                return Some((
                    FuncLookupResult::Func(func),
                    None,
                    self.grab_trailing_params(segments, start.span.as_end()),
                ))
            }
            ScopeItem::Module(module) => module,
            ScopeItem::Ty(ty) => {
                let (span, ty, segments) = match (ty.as_generic(), segments) {
                    (Some(ty), &[PathSegmentAst::Params(ast_params), ref segments @ ..]) => {
                        let params = ast_params
                            .iter()
                            .map(|&p| self.parser().ty(p))
                            .nsc_collect::<Option<BumpVec<_>>>()?;
                        (
                            ast_params.span(),
                            Ty::Instance(self.ext.creator().instance(ty, &params)),
                            segments,
                        )
                    }
                    _ => (start.span, ty, segments),
                };

                let &[start, ref segments @ ..] = segments else {
                    InvalidPathSegment {
                        loc: self.meta.loc(Self::segment_slice_span_low(span, segments)),
                        message: "expected ident (function, module or spec)",
                    }.add(self.ext.workspace)?;
                };

                return self.method_path(
                    ty,
                    PathAst {
                        start,
                        segments,
                        slash: None,
                    },
                );
            }
            item => self.invalid_symbol_type(item, start.span, "function or module")?,
        };

        let &[PathSegmentAst::Name(func_or_type), ref segments @ ..] = segments else {
            InvalidPathSegment {
                loc: self.meta.loc(Self::segment_slice_span_low(start.span, segments)),
                message: "expected a ident (function or type)",
            }.add(self.ext.workspace)?;
        };

        let id = self
            .ext
            .interner
            .intern_scoped(module.as_u32(), func_or_type.ident);
        let (ty, segments) = match self.lookup(id, func_or_type.span, "function")? {
            ScopeItem::Func(func) => {
                return Some((
                    FuncLookupResult::Func(func),
                    None,
                    self.grab_trailing_params(segments, func_or_type.span.as_end()),
                ))
            }
            ScopeItem::Ty(ty) => match (ty.as_generic(), segments) {
                (Some(ty), &[PathSegmentAst::Params(params), ref segments @ ..]) => {
                    let params = params
                        .iter()
                        .map(|&p| self.parser().ty(p))
                        .nsc_collect::<Option<BumpVec<_>>>()?;
                    (
                        Ty::Instance(self.ext.creator().instance(ty, &params)),
                        segments,
                    )
                }
                _ => (ty, segments),
            },
            item => self.invalid_symbol_type(item, func_or_type.span, "function")?,
        };
        let &[start, ref segments @ ..] = segments else {
            InvalidPathSegment {
                loc: self.meta.loc(Self::segment_slice_span(PathSegmentAst::Name(func_or_type), segments)),
                message: "expected ident (function, spec or module)",
            }.add(self.ext.workspace)?;
        };
        self.method_path(
            ty,
            PathAst {
                start,
                segments,
                slash: None,
            },
        )
    }

    pub(super) fn method_path<'b>(
        &mut self,
        ty: Ty,
        path @ PathAst {
            slash,
            start,
            segments,
            ..
        }: PathAst<'b>,
    ) -> Option<(
        FuncLookupResult<'arena>,
        Option<Ty>,
        Option<ListAst<'b, TyAst<'b>>>,
    )> {
        self.assert_no_slash(slash)?;

        let lty = ty.caller(self.ext.typec);
        let PathSegmentAst::Name(ident) = start else {
            InvalidPathSegment {
                loc: self.meta.loc(start.span()),
                message: "expected ident (function or spec)",
            }.add(self.ext.workspace)?;
        };

        let (module, spec_base_or_method, segments) = match self.ctx.try_lookup(ident.ident) {
            Ok(ScopeItem::SpecBase(spec)) => (None, Ok(spec), segments),
            Ok(ScopeItem::Module(module)) => {
                let &[PathSegmentAst::Name(spec_or_method), ref segments @ ..] = segments else {
                    InvalidPathSegment {
                        loc: self.meta.loc(Self::segment_slice_span(start, segments)),
                        message: "expected ident (method or spec)",
                    }.add(self.ext.workspace)?;
                };

                let id = self
                    .ext
                    .interner
                    .intern_scoped(module.index(), spec_or_method.ident);
                match self.ctx.try_lookup(id) {
                    Ok(ScopeItem::SpecBase(spec)) => (Some(module), Ok(spec), segments),
                    _ => (Some(module), Err(spec_or_method), segments),
                }
            }
            _ => (None, Err(ident), segments),
        };

        let spec_base = match spec_base_or_method {
            Ok(spec) => spec,
            Err(method) => {
                let local_id = self.ext.interner.intern_scoped(lty, method.ident);
                let id = module.map_or(local_id, |m| {
                    self.ext.interner.intern_scoped(m.index(), local_id)
                });
                let func = match self.lookup(id, method.span, "method")? {
                    ScopeItem::Func(func) => FuncLookupResult::Func(func),
                    ScopeItem::SpecFunc(func) => FuncLookupResult::SpecFunc(func, lty),
                    item => self.invalid_symbol_type(item, path.span(), "method")?,
                };
                return Some((
                    func,
                    Some(ty),
                    self.grab_trailing_params(segments, method.span.as_end()),
                ));
            }
        };

        let (spec, start, segments) =
            if let &[start @ PathSegmentAst::Params(ast_params), ref segments @ ..] = segments {
                let params = ast_params
                    .iter()
                    .map(|&p| self.parser().ty(p))
                    .nsc_collect::<Option<BumpVec<_>>>()?;

                if params.len() != self.ext.typec[spec_base].generics.len() {
                    WrongGenericParamCount {
                        expected: self.ext.typec[spec_base].generics.len(),
                        found: params.len(),
                        loc: self.meta.loc(ast_params.span()),
                    }
                    .add(self.ext.workspace)?;
                }

                (
                    Spec::Instance(self.ext.creator().spec_instance(spec_base, &params)),
                    start,
                    segments,
                )
            } else {
                (Spec::Base(spec_base), path.start, segments)
            };

        let &[PathSegmentAst::Name(method_ident), ref segments @ ..] = segments else {
            InvalidPathSegment {
                loc: self.meta.loc(start.span().as_end()),
                message: "expected ident (method)",
            }.add(self.ext.workspace)?;
        };

        let Some((method_index, method)) = self
            .ext
            .typec
            .cache
            .spec_funcs
            .indexed(self.ext.typec[spec_base].methods)
            .enumerate()
            .find_map(|(i, (key, func))| (method_ident.ident == func.name).then_some((i, key)))
            else {
                ComponentNotFound {
                    ty: self.ext.creator().display(spec),
                    loc: self.meta.loc(method_ident.span),
                    suggestions: self.ext.typec[self.ext.typec[spec_base].methods]
                        .iter()
                        .map(|func| func.name.get(self.ext.interner))
                        .intersperse(", ")
                        .collect(),
                    something: "method",
                }
                .add(self.ext.workspace)?
            };

        if let Some(r#impl) =
            self.ext
                .creator()
                .find_implementation(ty, spec, self.ctx.generics(), &mut None)
        {
            if let Some((r#impl, _)) = r#impl {
                let func = self.ext.typec[self.ext.typec[r#impl].methods][method_index];
                return Some((
                    FuncLookupResult::Func(func),
                    Some(ty),
                    self.grab_trailing_params(segments, method_ident.span.as_end()),
                ));
            }
        } else {
            MissingImpl {
                ty: self.ext.creator().display(ty),
                spec: self.ext.creator().display(spec),
                loc: self.meta.loc(path.span()),
            }
            .add(self.ext.workspace)?;
        }

        Some((
            FuncLookupResult::SpecFunc(method, lty),
            Some(ty),
            self.grab_trailing_params(segments, method_ident.span.as_end()),
        ))
    }

    fn assert_no_slash(&mut self, slash: Option<SourceInfo>) -> Option<()> {
        if let Some(slash) = slash {
            InvalidPathSegment {
                loc: self.meta.loc(slash.span),
                message: "leading slash in this context is invalid",
            }
            .add(self.ext.workspace)?;
        }
        Some(())
    }

    pub(super) fn value_path(
        &mut self,
        path @ PathAst {
            slash,
            start,
            segments,
        }: PathAst,
        inference: Inference,
    ) -> ExprRes<'arena> {
        if slash.is_some() {
            let Some(inferred) = inference.ty() else {
                CannotInferExpression {
                    help: "Try specifying the type before '\\'",
                    loc: self.meta.loc(path.span()),
                }.add(self.ext.workspace)?;
            };

            match inferred.base(self.ext.typec) {
                Ty::Enum(..) => {
                    return self.enum_ctor(EnumCtorAst { path, value: None }, inference)
                }
                Ty::Struct(..)
                | Ty::Array(..)
                | Ty::Instance(..)
                | Ty::Pointer(..)
                | Ty::Param(..)
                | Ty::Builtin(..) => UnexpectedType {
                    expected: "enum",
                    found: self.ext.creator().display(inferred),
                    loc: self.meta.loc(path.span()),
                }
                .add(self.ext.workspace)?,
            }
        }

        let PathSegmentAst::Name(start) = start else {
            InvalidPathSegment {
                loc: self.meta.loc(path.start.span().as_end()),
                message: "expected ident (variable or enum)",
            }.add(self.ext.workspace)?;
        };

        let choices = "variable, enum, constant or module";
        let sub_choices = "variable, enum or constant";
        let (item, _segments) = match self.lookup(start.ident, start.span, choices)? {
            ScopeItem::Module(module) => {
                let &[PathSegmentAst::Name(name), ref others @ ..] = segments else {
                    InvalidPathSegment {
                        message: "expected identirier (variable, enum or constant)",
                        loc: self.meta.loc(start.span.as_end()),
                    }.add(self.ext.workspace)?;
                };

                let scoped = self.ext.interner.intern_scoped(module.index(), name.ident);
                (self.lookup(scoped, name.span, sub_choices)?, others)
            }
            item => (item, segments),
        };

        Some(match item {
            ScopeItem::Ty(ty) => match ty {
                Ty::Enum(..) => {
                    return self.enum_ctor(EnumCtorAst { path, value: None }, inference);
                }
                Ty::Struct(..)
                | Ty::Instance(..)
                | Ty::Pointer(..)
                | Ty::Param(..)
                | Ty::Array(..)
                | Ty::Builtin(..) => UnexpectedInferenceType {
                    expected: "enum",
                    found: self.ext.creator().display(ty),
                    loc: self.meta.loc(path.span()),
                }
                .add(self.ext.workspace)?,
            },
            ScopeItem::Const(r#const) => TirNode::with_flags(
                self.ext.typec[r#const].ty,
                TirKind::ConstAccess(r#const),
                TirFlags::IMMUTABLE,
                path.span(),
            ),
            ScopeItem::VarHeaderTir(var) => TirNode::with_flags(
                self.ctx[var].ty,
                TirKind::Access(var),
                TirFlags::IMMUTABLE & !self.ctx[var].mutable,
                path.span(),
            ),
            item => self.invalid_symbol_type(item, start.span, sub_choices)?,
        })
    }

    pub(super) fn find_binary_func(
        &mut self,
        op: NameAst,
        lhs_ty: Ty,
        rhs_ty: Ty,
    ) -> OptFragRef<Func> {
        let base_id = op.ident;
        let id = self.ext.interner.intern_with(|s, t| {
            typec_u::display_bin_op(self.ext.typec, s, base_id, lhs_ty, rhs_ty, t)
        });
        Some(lookup!(Func self, id, op.span))
    }

    pub(super) fn find_field(
        &mut self,
        struct_ty: FragRef<Struct>,
        params: impl TypecCtxSlice<Ty>,
        name: NameAst,
    ) -> Option<(usize, bool, Ty)> {
        self.ext
            .creator()
            .find_struct_field(struct_ty, params, name.ident)
            .or_else(|| {
                let ty = self.ext.creator().display(Ty::Struct(struct_ty));
                self.ext.workspace.push(ComponentNotFound {
                    loc: self.meta.loc(name.span),
                    ty,
                    suggestions: self.ext.typec[self.ext.typec[struct_ty].fields]
                        .iter()
                        .map(|f| f.name.get(self.ext.interner))
                        .intersperse(", ")
                        .collect(),
                    something: "field",
                })?
            })
            .and_then(|(field_id, field, field_ty)| {
                self.can_access(
                    self.ext.typec[struct_ty].loc,
                    self.ext.typec[field].vis,
                    name.span,
                    self.ext.typec[field].span,
                )
                .then_some((
                    field_id,
                    self.ext.typec[field].flags.contains(FieldFlags::MUTABLE),
                    field_ty,
                ))
            })
    }

    fn can_access(
        &mut self,
        loc: Option<Loc>,
        vis: Option<Vis>,
        code_span: Span,
        def_span: Span,
    ) -> bool {
        self.meta
            .can_access(loc, vis, code_span, def_span, &mut self.ext)
    }

    fn grab_trailing_params<'a>(
        &mut self,
        segments: &[PathSegmentAst<'a>],
        backup_span: Span,
    ) -> Option<ListAst<'a, TyAst<'a>>> {
        match *segments {
            [] => None,
            [PathSegmentAst::Params(params)] => Some(params),
            ref segments => self.ext.workspace.push(InvalidPathSegment {
                message: "expected parameters or end of the path",
                loc: self.meta.loc(
                    segments
                        .iter()
                        .map(|s| s.span())
                        .reduce(Span::joined)
                        .unwrap_or(backup_span),
                ),
            })?,
        }
    }

    fn segment_slice_span(start: PathSegmentAst, segments: &[PathSegmentAst]) -> Span {
        let start = start.span();
        segments
            .iter()
            .map(|s| s.span())
            .fold(start.as_end(), Span::joined)
    }

    fn segment_slice_span_low(start: Span, segments: &[PathSegmentAst]) -> Span {
        segments
            .iter()
            .map(|s| s.span())
            .fold(start.as_end(), Span::joined)
    }

    fn lookup(&mut self, sym: Ident, span: Span, what: &'static str) -> Option<ScopeItem> {
        self.ctx.lookup(sym, span, what, &mut self.ext, &self.meta)
    }

    fn invalid_symbol_type(
        &mut self,
        item: ScopeItem,
        span: Span,
        expected: &'static str,
    ) -> Option<!> {
        self.ctx
            .invalid_symbol_type(item, span, expected, self.ext.workspace, &self.meta)
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
    pub error CannotInferExpression: fatal {
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

    #[err => "type of expression is not a {expected}"]
    #[info => "the type is '{found}' which is not a {expected}"]
    pub error UnexpectedType: fatal {
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
    pub error WrongGenericParamCount: fatal {
        #[err loc]
        expected: usize,
        found: usize,
        loc: SourceLoc,
    }
}

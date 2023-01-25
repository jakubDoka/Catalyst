use lexing::*;

use crate::ty_parser::InaccessibleScopeItem;

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
    ) -> Option<(FragRef<Enum>, NameAst, Option<ListAst<'a, TyAst<'a>>>)> {
        pub fn resolve<'a>(
            s: &mut TyChecker,
            enum_ty: FragRef<Enum>,
            segments: &[PathSegmentAst<'a>],
            backup_span: Span,
        ) -> Option<(FragRef<Enum>, NameAst, Option<ListAst<'a, TyAst<'a>>>)> {
            Some(match *segments {
                [PathSegmentAst::Name(name)] => (enum_ty, name, None),
                [PathSegmentAst::Params(params), PathSegmentAst::Name(name)] => {
                    (enum_ty, name, Some(params))
                }
                _ => s.workspace.push(InvalidPathSegment {
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

            let PathSegmentAst::Name(name) = path.start else {
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

        let PathSegmentAst::Name(name) = path.start else {
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

        let &[PathSegmentAst::Name(r#enum), ref segments @ ..] = path.segments else {
            self.workspace.push(InvalidPathSegment {
                loc: SourceLoc { origin: self.source, span: Self::segment_slice_span_low(name.span, path.segments) },
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

        let PathSegmentAst::Name(name) = start else {
            self.workspace.push(InvalidPathSegment {
                loc: SourceLoc { origin: self.source, span: start.span() },
                message: "expected a field name",
            })?;
        };

        let (field, mutable, field_ty) = self.find_field(struct_ty, params, name)?;

        Some(DotPathResult::Field(field as u32, mutable, field_ty))
    }

    pub fn func_path<'a, 'b>(
        &mut self,
        PathAst {
            start,
            segments,
            slash,
        }: PathAst<'b>,
        builder: &mut TirBuilder<'a, '_>,
    ) -> Option<(
        FuncLookupResult<'a>,
        Option<Ty>,
        Option<ListAst<'b, TyAst<'b>>>,
    )> {
        self.assert_no_slash(slash)?;

        let PathSegmentAst::Name(start) = start else {
            self.workspace.push(InvalidPathSegment {
                loc: SourceLoc { origin: self.source, span: start.span() },
                message: "expected ident (function, module or type)",
            })?;
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
                            .map(|&p| self.ty(p))
                            .nsc_collect::<Option<BumpVec<_>>>()?;
                        (
                            ast_params.span(),
                            Ty::Instance(self.typec.instance(ty, &params, self.interner)),
                            segments,
                        )
                    }
                    _ => (start.span, ty, segments),
                };

                let &[start, ref segments @ ..] = segments else {
                    self.workspace.push(InvalidPathSegment {
                        loc: SourceLoc {
                            origin: self.source,
                            span: Self::segment_slice_span_low(span, segments)
                        },
                        message: "expected ident (function, module or spec)",
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
            item => self.invalid_symbol_type(item, start.span, "function or module")?,
        };

        let &[PathSegmentAst::Name(func_or_type), ref segments @ ..] = segments else {
            self.workspace.push(InvalidPathSegment {
                loc: SourceLoc {
                    origin: self.source,
                    span: Self::segment_slice_span_low(start.span, segments),
                },
                message: "expected a ident (function or type)",
            })?;
        };

        let id = self
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
                        .map(|&p| self.ty(p))
                        .nsc_collect::<Option<BumpVec<_>>>()?;
                    (
                        Ty::Instance(self.typec.instance(ty, &params, self.interner)),
                        segments,
                    )
                }
                _ => (ty, segments),
            },
            item => self.invalid_symbol_type(item, func_or_type.span, "function")?,
        };
        let &[start, ref segments @ ..] = segments else {
            self.workspace.push(InvalidPathSegment {
                loc: SourceLoc {
                    origin: self.source,
                    span: Self::segment_slice_span(PathSegmentAst::Name(func_or_type), segments),
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
    ) -> Option<(
        FuncLookupResult<'a>,
        Option<Ty>,
        Option<ListAst<'b, TyAst<'b>>>,
    )> {
        self.assert_no_slash(slash)?;

        let lty = ty.caller(self.typec);
        let PathSegmentAst::Name(ident) = start else {
            self.workspace.push(InvalidPathSegment {
                loc: SourceLoc { origin: self.source, span: start.span() },
                message: "expected ident (function or spec)",
            })?;
        };

        let (module, spec_base_or_method, segments) = match self.scope.get(ident.ident) {
            Ok(ScopeItem::SpecBase(spec)) => (None, Ok(spec), segments),
            Ok(ScopeItem::Module(module)) => {
                let &[PathSegmentAst::Name(spec_or_method), ref segments @ ..] = segments else {
                    self.workspace.push(InvalidPathSegment {
                        loc: SourceLoc {
                            origin: self.source,
                            span: Self::segment_slice_span(start, segments),
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
                    .map(|&p| self.ty(p))
                    .nsc_collect::<Option<BumpVec<_>>>()?;

                if params.len() != self.typec[spec_base].generics.len() {
                    self.workspace.push(WrongGenericParamCount {
                        expected: self.typec[spec_base].generics.len(),
                        found: params.len(),
                        loc: SourceLoc {
                            origin: self.source,
                            span: ast_params.span(),
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

        let &[PathSegmentAst::Name(method_ident), ref segments @ ..] = segments else {
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
            .cache
            .spec_funcs
            .indexed(self.typec[spec_base].methods)
            .enumerate()
            .find_map(|(i, (key, func))| (method_ident.ident == func.name).then_some((i, key)))
            .or_else(|| {
                self.workspace.push(ComponentNotFound {
                    ty: self.typec.display_spec(spec, self.interner),
                    loc: SourceLoc {
                        origin: self.source,
                        span: method_ident.span,
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
                    self.grab_trailing_params(segments, method_ident.span.as_end()),
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
            self.grab_trailing_params(segments, method_ident.span.as_end()),
        ))
    }

    pub fn assert_no_slash(&mut self, slash: Option<SourceInfo>) -> Option<()> {
        if let Some(slash) = slash {
            self.workspace.push(InvalidPathSegment {
                loc: SourceLoc {
                    origin: self.source,
                    span: slash.span,
                },
                message: "leading slash in this context is invalid",
            })?;
        }
        Some(())
    }

    pub fn value_path<'a>(
        &mut self,
        path @ PathAst {
            slash,
            start,
            segments,
        }: PathAst,
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
                | Ty::Builtin(..) => self.workspace.push(UnexpectedType {
                    expected: "enum",
                    found: self.typec.display_ty(inferred, self.interner),
                    loc: SourceLoc {
                        origin: self.source,
                        span: path.span(),
                    },
                })?,
            }
        }

        let PathSegmentAst::Name(start) = start else {
            self.workspace.push(InvalidPathSegment {
                loc: SourceLoc {
                    origin: self.source,
                    span: path.start.span().as_end(),
                },
                message: "expected ident (variable or enum)",
            })?;
        };

        let choices = "variable, enum, constant or module";
        let sub_choices = "variable, enum or constant";
        let (item, _segments) = match self.lookup(start.ident, start.span, choices)? {
            ScopeItem::Module(module) => {
                let &[PathSegmentAst::Name(name), ref others @ ..] = segments else {
                    self.workspace.push(InvalidPathSegment {
                        message: "expected identirier (variable, enum or constant)",
                        loc: SourceLoc { origin: self.source, span: start.span.as_end() },
                    })?;
                };

                let scoped = self.interner.intern_scoped(module.index(), name.ident);
                (self.lookup(scoped, name.span, sub_choices)?, others)
            }
            item => (item, segments),
        };

        Some(match item {
            ScopeItem::Ty(ty) => match ty {
                Ty::Enum(..) => {
                    return self.enum_ctor(EnumCtorAst { path, value: None }, inference, builder);
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
            ScopeItem::Const(r#const) => TirNode::with_flags(
                self.typec[r#const].ty,
                TirKind::ConstAccess(r#const),
                TirFlags::IMMUTABLE,
                path.span(),
            ),
            ScopeItem::VarHeaderTir(var) => TirNode::with_flags(
                builder.get_var(var).ty,
                TirKind::Access(var),
                TirFlags::IMMUTABLE & !builder.get_var(var).mutable,
                path.span(),
            ),
            item => self.invalid_symbol_type(item, start.span, sub_choices)?,
        })
    }

    pub fn find_binary_func(&mut self, op: NameAst, lhs_ty: Ty, rhs_ty: Ty) -> OptFragRef<Func> {
        let base_id = op.ident;
        let id = self
            .interner
            .intern_with(|s, t| self.typec.binary_op_id(base_id, lhs_ty, rhs_ty, t, s));
        Some(lookup!(Func self, id, op.span))
    }

    pub fn find_field(
        &mut self,
        struct_ty: FragRef<Struct>,
        params: impl TypecCtxSlice<Ty>,
        name: NameAst,
    ) -> Option<(usize, bool, Ty)> {
        self.typec
            .find_struct_field(struct_ty, params, name.ident, self.interner)
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
            .and_then(|(field_id, field, field_ty)| {
                self.can_access(
                    self.typec[struct_ty].loc,
                    self.typec[field].vis,
                    name.span,
                    self.typec[field].span,
                )
                .then_some((
                    field_id,
                    self.typec[field].flags.contains(FieldFlags::MUTABLE),
                    field_ty,
                ))
            })
    }

    pub fn can_access(
        &mut self,
        loc: Option<Loc>,
        vis: Option<Vis>,
        code_span: Span,
        def_span: Span,
    ) -> bool {
        let Some(loc) = loc else {
            return true;
        };

        let (position, accessible) =
            Scope::compute_accessibility(self.module, loc.module, vis, self.resources);

        if let Some(pos) = position && !accessible {
            self.workspace.push(InaccessibleScopeItem {
                pos,
                span: code_span,
                item_def: SourceLoc {
                    origin: self.resources.modules[loc.module].source,
                    span: def_span,
                },
                source: self.source,
            });
        }

        accessible
    }

    pub fn grab_trailing_params<'a>(
        &mut self,
        segments: &[PathSegmentAst<'a>],
        backup_span: Span,
    ) -> Option<ListAst<'a, TyAst<'a>>> {
        match *segments {
            [] => None,
            [PathSegmentAst::Params(params)] => Some(params),
            ref segments => self.workspace.push(InvalidPathSegment {
                message: "expected parameters or end of the path",
                loc: SourceLoc {
                    origin: self.source,
                    span: segments
                        .iter()
                        .map(|s| s.span())
                        .reduce(Span::joined)
                        .unwrap_or(backup_span),
                },
            })?,
        }
    }

    fn segment_slice_span(start: PathSegmentAst, segments: &[PathSegmentAst]) -> Span {
        Self::segment_slice_span_low(start.span(), segments)
    }

    fn segment_slice_span_low(start: Span, segments: &[PathSegmentAst]) -> Span {
        segments
            .iter()
            .map(|s| s.span())
            .fold(start.as_end(), Span::joined)
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

    #[err => "type of expression is not a {expected}"]
    #[info => "the type is '{found}' which is not a {expected}"]
    error UnexpectedType: fatal {
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

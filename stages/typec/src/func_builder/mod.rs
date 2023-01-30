use {
    crate::{ty_parser::TyPathResult, *},
    diags::*,
    lexing_t::*,
    packaging_t::*,
    parsing_t::*,
    std::{cmp::Ordering, default::default, iter, vec},
    storage::*,
    typec_t::*,
};

mod call;
mod consts;
mod control_flow;
mod data;
mod lookup;
mod spec;

pub type ExprRes<'a> = Option<TirNode<'a>>;

impl TyChecker<'_> {
    pub fn build_funcs<'a>(
        &mut self,
        arena: &'a Arena,
        input: &[(FuncDefAst, FragRef<Func>)],
        compiled_funcs: &mut BumpVec<(FragRef<Func>, TirFunc<'a>)>,
        extern_funcs: &mut Vec<FragRef<Func>>,
        ctx: &mut TirBuilderCtx,
        offset: usize,
    ) -> &mut Self {
        input
            .iter()
            .filter_map(|&(ast, func)| {
                let res = self.build_func(ast, func, arena, ctx, offset)?;

                let Some(body) = res else {
                    extern_funcs.push(func);
                    return None;
                };

                Some((func, body))
            })
            .collect_into(&mut **compiled_funcs);

        self
    }

    pub fn build_func<'a>(
        &mut self,
        FuncDefAst {
            signature:
                FuncSigAst {
                    generics,
                    args,
                    ret,
                    ..
                },
            body,
            ..
        }: FuncDefAst,
        func: FragRef<Func>,
        arena: &'a Arena,
        ctx: &mut TirBuilderCtx,
        offset: usize,
    ) -> Option<Option<TirFunc<'a>>> {
        let Func {
            signature,
            generics: self_generics,
            ..
        } = self.typec[func];

        ctx.generics.extend(self.typec.pack_func_param_specs(func));
        let mut builder =
            TirBuilder::new(arena, signature.ret, ret.map(|(.., ret)| ret.span()), ctx);
        let frame = builder.start_frame(self.scope);

        self.insert_generics(generics, offset);
        self.insert_spec_functions(self_generics, offset);
        let args = self.args(signature.args, args, &mut builder);

        let tir_body = match body {
            FuncBodyAst::Arrow(.., expr) => {
                self.expr(expr, Inference::Weak(signature.ret), &mut builder)
            }
            FuncBodyAst::Block(body) => {
                self.block(body, Inference::Weak(signature.ret), &mut builder)
            }
            FuncBodyAst::Extern(..) => {
                builder.end_frame(self.scope, frame);
                return Some(None);
            }
        };

        builder.end_frame(self.scope, frame);

        builder.ctx.generics.clear();

        let tir_body = tir_body?;

        let body = if tir_body.ty == Ty::TERMINAL {
            tir_body
        } else if tir_body.ty == signature.ret && signature.ret != Ty::UNIT {
            self.return_low(Some(tir_body), body.span(), &mut builder)?
        } else {
            let ret = self.return_low(None, body.span(), &mut builder)?;
            TirNode::new(
                Ty::TERMINAL,
                TirKind::Block(builder.arena.alloc([tir_body, ret])),
                tir_body.span,
            )
        };

        Some(Some(TirFunc { args, body }))
    }

    pub fn expr<'a>(
        &mut self,
        expr: ExprAst,
        inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let value = match expr {
            ExprAst::Unit(&unit) => self.unit_expr(unit, inference, builder),
            ExprAst::Binary(&binary) => self.binary_expr(binary, builder),
        }?;

        if let Inference::Strong(ty) = inference {
            self.type_check(ty, value.ty, expr.span())?;
        }

        Some(value)
    }

    pub fn unit_expr<'a>(
        &mut self,
        unit_ast: UnitExprAst,
        inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        use UnitExprAst::*;
        match unit_ast {
            Path(path) => self.value_path(path, inference, builder),
            Return(ReturnAst { keyword, expr }) => self.r#return(expr, keyword.span, builder),
            Int(source_info) => self.int(source_info.span, inference),
            Float(source_info) => self.float(source_info.span, inference),
            Char(source_info) => self.char(source_info.span),
            Bool(source_info) => self.bool(source_info.span),
            Call(&call) => self.call(call, inference, builder),
            StructCtor(ctor) => self.struct_ctor(ctor, inference, builder),
            EnumCtor(ctor) => self.enum_ctor(ctor, inference, builder),
            Match(match_expr) => self.r#match(match_expr, inference, builder),
            If(r#if) => self.r#if(r#if, inference, builder),
            Loop(loop_expr) => self.r#loop(loop_expr, inference, builder),
            Break(r#break) => self.r#break(r#break, builder),
            Continue(r#continue) => self.r#continue(r#continue, builder),
            DotExpr(&expr) => self.dot_expr(expr, inference, builder),
            Let(r#let) => self.r#let(r#let, inference, builder),
            Deref(.., &expr) => self.deref(expr, inference, builder),
            Ref(.., mutability, &expr) => self.r#ref(mutability, expr, inference, builder),
            Block(block) => self.block(block, inference, builder),
        }
    }

    pub fn dot_expr<'a>(
        &mut self,
        DotExprAst { lhs, rhs, .. }: DotExprAst,
        _inference: Inference,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let mut header = self.unit_expr(lhs, Inference::None, builder)?;

        let deref = header.ty.ptr_base(self.typec);
        let caller = deref.base(self.typec);
        let res = self.dot_path(caller, rhs, builder)?;

        self.balance_pointers(&mut header, deref, builder)?;

        Some(match res {
            DotPathResult::Field(field, mutable, ty) => TirNode::with_flags(
                ty,
                TirKind::Field(builder.arena.alloc(FieldTir { header, field })),
                header.flags | (TirFlags::IMMUTABLE & !mutable),
                rhs.span(),
            ),
        })
    }

    pub fn pattern<'a>(
        &mut self,
        pattern: PatAst,
        ty: Ty,
        builder: &mut TirBuilder<'a, '_>,
    ) -> Option<PatTir<'a>> {
        match pattern {
            PatAst::Binding(mutable, name) => {
                let var = builder.create_var(mutable.is_some(), ty, name.span);
                self.scope.push(name.ident, var, name.span);
                Some(PatTir {
                    kind: PatKindTir::Unit(UnitPatKindTir::Binding(mutable.is_some(), var)),
                    span: name.span,
                    has_binding: true,
                    is_refutable: false,
                    ty,
                })
            }
            PatAst::StructCtor(StructCtorPatAst { fields, .. }) => {
                let (Ty::Struct(struct_ty), params) = ty.caller_with_params(self.typec) else {
                    self.workspace.push(UnexpectedPatternType {
                        loc: SourceLoc { origin: self.source, span: fields.span() },
                        ty: self.typec.display_ty(ty, self.interner),
                        ty_loc: None, //TODO: make a typec getter for loc on type
                        something: "struct",
                    })?;
                };

                let mut tir_fields = bumpvec![None; fields.len()];
                let mut double_dot = None;
                for &field in fields.iter() {
                    match field {
                        StructCtorPatFieldAst::Simple { name, mutable } => {
                            let (field_id, .., field_ty) =
                                self.find_field(struct_ty, params, name)?;
                            let field =
                                self.pattern(PatAst::Binding(mutable, name), field_ty, builder)?;
                            tir_fields[field_id] = Some(field);
                        }
                        StructCtorPatFieldAst::Named { name, pat, .. } => {
                            let (field_id, .., field_ty) =
                                self.find_field(struct_ty, params, name)?;
                            let field = self.pattern(pat, field_ty, builder)?;
                            tir_fields[field_id] = Some(field);
                        }
                        StructCtorPatFieldAst::DoubleDot(source_info) => {
                            if let Some(prev) = double_dot.replace(source_info) {
                                self.workspace.push(DuplicateDoubleDot {
                                    loc: SourceLoc {
                                        origin: self.source,
                                        span: source_info.span,
                                    },
                                    prev: prev.span,
                                })?;
                            }
                        }
                    }
                }

                if let Some(double_dot) = double_dot {
                    tir_fields.iter_mut().filter(|f| f.is_none()).for_each(|f| {
                        *f = Some(PatTir {
                            kind: PatKindTir::Unit(UnitPatKindTir::Wildcard),
                            span: double_dot.span,
                            ty: Ty::UNIT,
                            has_binding: false,
                            is_refutable: false,
                        })
                    });
                }

                let missing_fields = tir_fields
                    .iter()
                    .zip(&self.typec[self.typec[struct_ty].fields])
                    .filter_map(|(opt, f)| opt.is_none().then_some(&f.name))
                    .map(|name| name.get(self.interner))
                    .intersperse(", ")
                    .collect::<String>();

                if !missing_fields.is_empty() {
                    self.workspace.push(MissingStructPatternFields {
                        loc: SourceLoc {
                            origin: self.source,
                            span: fields.span(),
                        },
                        missing_fields,
                        struct_loc: self.typec[struct_ty]
                            .loc
                            .map(|loc| loc.source_loc(self.typec, self.resources)),
                    })?;
                }

                Some(PatTir {
                    has_binding: tir_fields.iter().flatten().any(|f| f.has_binding),
                    is_refutable: tir_fields.iter().flatten().any(|f| f.is_refutable),
                    kind: PatKindTir::Unit(UnitPatKindTir::Struct {
                        fields: builder.arena.alloc_iter(tir_fields.into_iter().map(|f| {
                            f.expect("missing_fields is empty which implies this is impossible")
                        })),
                    }),
                    span: fields.span(),
                    ty,
                })
            }
            PatAst::Int(source_info) => Some(PatTir {
                kind: PatKindTir::Unit(UnitPatKindTir::Int(Ok(source_info.span))),
                span: source_info.span,
                ty,
                has_binding: false,
                is_refutable: true,
            }),
            PatAst::EnumCtor(ctor) => {
                let ty_base = ty.ptr_base(self.typec);
                let Ty::Enum(enum_ty) = ty_base.base(self.typec) else {
                    self.workspace.push(UnexpectedPatternType {
                        loc: SourceLoc { origin: self.source, span: ctor.span() },
                        ty: self.typec.display_ty(ty, self.interner),
                        ty_loc: None, //TODO: make a typec getter for loc on type
                        something: "enum",
                    })?;
                };

                let (index, variant_ty) = ty_base
                    .find_component(ctor.name.ident, self.typec, self.interner)
                    .or_else(|| {
                        self.workspace.push(ComponentNotFound {
                            loc: SourceLoc {
                                origin: self.source,
                                span: ctor.span(),
                            },
                            ty: self.typec.display_ty(Ty::Enum(enum_ty), self.interner),
                            suggestions: self.typec[self.typec[enum_ty].variants]
                                .iter()
                                .map(|v| v.name.get(self.interner))
                                .intersperse(", ")
                                .collect(),
                            something: "variant",
                        })?
                    })?;

                let value = ctor
                    .value
                    .map(|(.., body)| self.pattern(body, variant_ty, builder))
                    .transpose()?;

                Some(PatTir {
                    kind: PatKindTir::Unit(UnitPatKindTir::Enum {
                        id: index as u32,
                        ty: enum_ty,
                        value: value.map(|value| builder.arena.alloc(value)),
                    }),
                    span: ctor.span(),
                    ty,
                    has_binding: value.map_or(false, |v| v.has_binding),
                    is_refutable: self.typec[enum_ty].variants.len() > 1,
                })
            }
            PatAst::Wildcard(source_info) => Some(PatTir {
                kind: PatKindTir::Unit(UnitPatKindTir::Wildcard),
                has_binding: false,
                is_refutable: false,
                span: source_info.span,
                ty,
            }),
        }
    }

    pub fn infer_params(
        &mut self,
        params: &mut [Option<Ty>],
        reference: Ty,
        template: Ty,
        span: Span,
    ) -> Option<()> {
        self.typec
            .compatible(params, reference, template)
            .map_err(|_| {
                self.workspace.push(GenericTypeMismatch {
                    expected: self.typec.display_ty(reference, self.interner),
                    got: self.typec.display_ty(template, self.interner),
                    loc: SourceLoc {
                        span,
                        origin: self.source,
                    },
                })
            })
            .ok()
    }

    pub fn binary_expr<'a>(
        &mut self,
        binary_ast @ BinaryExprAst { lhs, op, rhs }: BinaryExprAst,
        builder: &mut TirBuilder<'a, '_>,
    ) -> ExprRes<'a> {
        let lhs = self.expr(lhs, Inference::None, builder);

        if op.ident == Interner::ASSIGN {
            let rhs = self.expr(rhs, lhs.map(|lhs| lhs.ty).into(), builder)?;

            if let Some(lhs) = lhs && lhs.flags.contains(TirFlags::IMMUTABLE) {
                self.workspace.push(NotMutable {
                    loc: SourceLoc {
                        origin: self.source,
                        span: lhs.span,
                    },
                });
            }

            return Some(TirNode::new(
                Ty::UNIT,
                TirKind::Assign(builder.arena.alloc(AssignTir { lhs: lhs?, rhs })),
                binary_ast.span(),
            ));
        }

        let rhs = self.expr(rhs, Inference::None, builder);
        let (lhs, rhs) = (lhs?, rhs?); // recovery

        let func = self.find_binary_func(op, lhs.ty, rhs.ty)?;

        let ty = self.typec[func].signature.ret;
        let call = CallTir {
            func: CallableTir::Func(func),
            params: default(),
            args: builder.arena.alloc_slice(&[lhs, rhs]),
        };
        Some(TirNode::new(
            ty,
            TirKind::Call(builder.arena.alloc(call)),
            binary_ast.span(),
        ))
    }

    pub fn args<'a>(
        &mut self,
        types: FragSlice<Ty>,
        args: Option<ListAst<FuncArgAst>>,
        builder: &mut TirBuilder<'a, '_>,
    ) -> &'a [PatTir<'a>] {
        let Some(args) = args else {return &[]};
        let args = self.typec[types]
            .to_bumpvec()
            .into_iter()
            .zip(args.iter())
            .filter_map(|(ty, &arg)| self.pattern(arg.pat, ty, builder))
            .nsc_collect::<BumpVec<_>>();
        builder.arena.alloc_iter(args)
    }

    pub fn type_check(&mut self, expected: Ty, got: Ty, span: Span) -> Option<()> {
        self.type_check_detailed(expected, got, |s| {
            s.workspace.push(GenericTypeMismatch {
                expected: s.typec.display_ty(expected, s.interner),
                got: s.typec.display_ty(got, s.interner),
                loc: SourceLoc {
                    span,
                    origin: s.source,
                },
            })
        })
    }

    pub fn type_check_detailed<A>(
        &mut self,
        expected: Ty,
        got: Ty,
        display: impl Fn(&mut Self) -> A,
    ) -> Option<()> {
        if Ty::compatible(expected, got) {
            return Some(());
        }

        display(self);

        None
    }
}

pub enum DotPathResult {
    Field(u32, bool, Ty),
}

pub enum FuncLookupResult<'a> {
    Func(FragRef<Func>),
    SpecFunc(FragRef<SpecFunc>, Ty),
    #[allow(dead_code)]
    Var(TirNode<'a>),
}

pub enum SignatureCheckError {
    ArgCountMismatch(usize, usize),
    ArgMismatch(BumpVec<(Option<usize>, Ty, Ty)>),
    MissingSpecs(BumpVec<ImplKey>),
    CCMismatch(Option<Ident>, Option<Ident>),
}

ctl_errors! {
    #[err => "expected mutable value"]
    error NotMutable: fatal {
        #[err loc]
        loc: SourceLoc,
    }

    #[err => "type mismatch"]
    #[info => "expected '{expected}' but got '{got}'"]
    error GenericTypeMismatch: fatal {
        #[err loc]
        expected ref: String,
        got ref: String,
        loc: SourceLoc,
    }

    #[err => "missing struct pattern fields"]
    #[info => "missing fields: {missing_fields}"]
    #[help => "add '..' to the pattern to ignore the missing fields"]
    error MissingStructPatternFields: fatal {
        #[info struct_loc, "struct defined here"]
        #[err loc]
        missing_fields ref: String,
        loc: SourceLoc,
        struct_loc: Option<SourceLoc>,
    }

    #[err => "unexpected {something} pattern"]
    #[info => "the type '{ty}' is not a {something}"]
    error UnexpectedPatternType: fatal {
        #[info ty_loc, "type defined here"]
        #[err loc]
        ty ref: String,
        loc: SourceLoc,
        ty_loc: Option<SourceLoc>,
        something: &'static str,
    }

    #[err => "{something} not found on '{ty}'"]
    #[info => "possible {something}s: {suggestions}"]
    error ComponentNotFound: fatal {
        #[err loc]
        ty ref: String,
        loc: SourceLoc,
        suggestions ref: String,
        something: &'static str,
    }

    #[warn => "duplicate '..' in struct pattern"]
    #[note => "one '..' is enough"]
    error DuplicateDoubleDot {
        #[note loc.origin, prev, "'..' is already here"]
        #[warn loc]
        loc: SourceLoc,
        prev: Span,
    }
}

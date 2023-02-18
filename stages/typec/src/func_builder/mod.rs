use crate::{
    context::{TypecCtx, TypecExternalCtx, TypecMeta},
    ty_parser::TypecParser,
};

use self::lookup::CannotInferExpression;

use {
    crate::ty_parser::TyPathResult,
    diags::*,
    span::*,
    ast::*,
    std::{cmp::Ordering, default::default, iter},
    storage::*,
    types::*,
};

mod call;
mod control_flow;
mod data;
mod lookup;

pub type ExprRes<'arena> = Option<TirNode<'arena>>;

pub struct TirBuilder<'arena, 'ctx> {
    ret: Option<Ty>,
    arena: &'arena Arena,
    ctx: &'ctx mut TypecCtx,
    ext: TypecExternalCtx<'arena, 'ctx>,
    meta: TypecMeta,
}

impl<'arena, 'ctx> TirBuilder<'arena, 'ctx> {
    pub fn new(
        ret: impl Into<Option<Ty>>,
        arena: &'arena Arena,
        ctx: &'ctx mut TypecCtx,
        ext: TypecExternalCtx<'arena, 'ctx>,
        meta: TypecMeta,
    ) -> Self {
        Self {
            arena,
            ret: ret.into(),
            ctx,
            ext,
            meta,
        }
    }

    pub(crate) fn build_func(
        mut self,
        FuncDefAst {
            signature: FuncSigAst { generics, args, .. },
            body,
            ..
        }: FuncDefAst,
        func: FragRef<Func>,
        offset: usize,
    ) -> Option<Option<TirFunc<'arena>>> {
        let Func {
            signature,
            generics: self_generics,
            ..
        } = self.ext.types[func];

        let body = match body {
            FuncBodyAst::Arrow(arrow, expr) => BranchAst::Arrow(arrow, expr),
            FuncBodyAst::Block(body) => BranchAst::Block(body),
            FuncBodyAst::Extern(..) => return Some(None),
        };

        self.ctx
            .load_generics(self.ext.types.pack_func_param_specs(func));

        let frame = self.ctx.start_frame();

        self.ctx.insert_generics(generics, offset);
        self.ctx
            .insert_spec_functions(self_generics, offset, self.ext.types, self.ext.interner);
        let args = self.args(signature.args, args);

        let tir_body = match body {
            BranchAst::Arrow(.., expr) => self.expr(expr, Inference::Weak(signature.ret)),
            BranchAst::Block(body) => self.block(body, Inference::Weak(signature.ret)),
        };

        self.ctx.end_frame(frame);

        let tir_body = tir_body?;

        let body = if tir_body.ty == Ty::TERMINAL {
            tir_body
        } else if tir_body.ty == signature.ret && signature.ret != Ty::UNIT {
            self.return_low(Some(tir_body), body.span())?
        } else {
            let ret = self.return_low(None, body.span())?;
            TirNode::new(
                Ty::TERMINAL,
                TirKind::Block(self.arena.alloc([tir_body, ret])),
                tir_body.span,
            )
        };

        Some(Some(TirFunc { args, body }))
    }

    #[inline]
    fn unpack_param_slots(
        &mut self,
        params: impl Iterator<Item = Option<Ty>> + Clone + ExactSizeIterator,
        span: Span,

        something: &'static str,
        syntax: &'static str,
    ) -> Option<&'arena [Ty]> {
        if let Some(params) = params.clone().collect::<Option<BumpVec<_>>>() {
            return Some(self.arena.alloc_iter(params));
        }

        let missing = params
            .enumerate()
            .filter_map(|(i, param)| param.is_none().then_some(i))
            .map(|i| format!("#{i}"))
            .intersperse_with(|| ", ".into())
            .collect::<String>();

        UnknownTypeParameters {
            loc: self.meta.loc(span),
            missing,
            something,
            syntax,
        }
        .add(self.ext.workspace)?
    }

    fn expr(&mut self, expr: ExprAst, inference: Inference) -> ExprRes<'arena> {
        let value = match expr {
            ExprAst::Unit(&unit) => self.unit_expr(unit, inference),
            ExprAst::Binary(&binary) => self.binary_expr(binary),
        }?;

        if let Inference::Strong(ty) = inference {
            self.type_check(ty, value.ty, expr.span())?;
        }

        Some(value)
    }

    fn unit_expr(&mut self, unit_ast: UnitExprAst, inference: Inference) -> ExprRes<'arena> {
        use UnitExprAst::*;
        match unit_ast {
            Path(path) => self.value_path(path, inference),
            Return(ReturnAst { keyword, expr }) => self.r#return(expr, keyword.span),
            Int(source_info) => self.int(source_info.span, inference),
            Float(source_info) => self.float(source_info.span, inference),
            Char(source_info) => self.char(source_info.span),
            Bool(source_info) => self.bool(source_info.span),
            Call(&call) => self.call(call, inference),
            StructCtor(ctor) => self.struct_ctor(ctor, inference),
            EnumCtor(ctor) => self.enum_ctor(ctor, inference),
            Match(match_expr) => self.r#match(match_expr, inference),
            If(r#if) => self.r#if(r#if, inference),
            Loop(loop_expr) => self.r#loop(loop_expr, inference),
            Break(r#break) => self.r#break(r#break),
            Continue(r#continue) => self.r#continue(r#continue),
            DotExpr(&expr) => self.dot_expr(expr, inference),
            Let(r#let) => self.r#let(r#let, inference),
            Deref(.., &expr) => self.deref(expr, inference),
            Ref(.., mutability, &expr) => self.r#ref(mutability, expr, inference),
            Block(block) => self.block(block, inference),
            Array(array) => self.array(array, inference),
        }
    }

    fn array(&mut self, array: ListAst<ExprAst>, mut inference: Inference) -> ExprRes<'arena> {
        let mut elements = bumpvec![cap array.len()];

        inference = inference.map(|ty| ty.array_base(self.ext.types));

        for &expr in array.iter() {
            let Some(element) = self.expr(expr, inference) else {continue};
            inference = Inference::Strong(element.ty);
            elements.push(element);
        }

        let Some(elem_ty) = inference.ty() else {
            CannotInferExpression {
                loc: self.meta.loc(array.span()),
                help: "array element is infered based of its first element",
            }.add(self.ext.workspace)?;
        };

        let array_ty = self
            .ext
            .creator()
            .array_of(elem_ty, elements.len() as ArraySize);

        Some(TirNode::new(
            array_ty.into(),
            TirKind::Ctor(self.arena.alloc_iter(elements)),
            array.span(),
        ))
    }

    fn dot_expr(
        &mut self,
        DotExprAst { lhs, rhs, .. }: DotExprAst,
        _inference: Inference,
    ) -> ExprRes<'arena> {
        let mut header = self.unit_expr(lhs, Inference::None)?;

        let deref = header.ty.ptr_base(self.ext.types);
        let caller = deref.base(self.ext.types);
        let res = self.dot_path(caller, rhs)?;

        self.balance_pointers(&mut header, deref)?;

        Some(match res {
            DotPathResult::Field(field, mutable, ty) => TirNode::with_flags(
                ty,
                TirKind::Field(self.arena.alloc(FieldTir { header, field })),
                header.flags | (TirFlags::IMMUTABLE & !mutable),
                rhs.span(),
            ),
        })
    }

    fn pattern(&mut self, pattern: PatAst, ty: Ty) -> Option<PatTir<'arena>> {
        match pattern {
            PatAst::Binding(mutable, name) => {
                let var = self.ctx.create_var(mutable.is_some(), ty, name);
                Some(PatTir {
                    kind: PatKindTir::Unit(UnitPatKindTir::Binding(mutable.is_some(), var)),
                    span: name.span,
                    has_binding: true,
                    is_refutable: false,
                    ty,
                })
            }
            PatAst::StructCtor(StructCtorPatAst { fields, .. }) => {
                let (Ty::Struct(struct_ty), params) = ty.caller_with_params(self.ext.types) else {
                    UnexpectedPatternType {
                        loc: self.meta.loc(fields.span()),
                        ty: self.ext.creator().display(ty),
                        ty_loc: None, //TODO: make a types getter for loc on type
                        something: "struct",
                    }.add(self.ext.workspace)?;
                };

                let mut tir_fields = bumpvec![None; fields.len()];
                let mut double_dot = None;
                for &field in fields.iter() {
                    match field {
                        StructCtorPatFieldAst::Simple { name, mutable } => {
                            let (field_id, .., field_ty) =
                                self.find_field(struct_ty, params, name)?;
                            let field = self.pattern(PatAst::Binding(mutable, name), field_ty)?;
                            tir_fields[field_id] = Some(field);
                        }
                        StructCtorPatFieldAst::Named { name, pat, .. } => {
                            let (field_id, .., field_ty) =
                                self.find_field(struct_ty, params, name)?;
                            let field = self.pattern(pat, field_ty)?;
                            tir_fields[field_id] = Some(field);
                        }
                        StructCtorPatFieldAst::DoubleDot(source_info) => {
                            if let Some(prev) = double_dot.replace(source_info) {
                                DuplicateDoubleDot {
                                    loc: self.meta.loc(source_info.span),
                                    prev: prev.span,
                                }
                                .add(self.ext.workspace)?;
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
                    .zip(&self.ext.types[self.ext.types[struct_ty].fields])
                    .filter_map(|(opt, f)| opt.is_none().then_some(&f.name))
                    .map(|name| name.get(self.ext.interner))
                    .intersperse(", ")
                    .collect::<String>();

                if !missing_fields.is_empty() {
                    self.ext.workspace.push(MissingStructPatternFields {
                        loc: self.meta.loc(fields.span()),
                        missing_fields,
                        struct_loc: self.ext.types[struct_ty].loc.source_loc(self.ext.types),
                    })?;
                }

                Some(PatTir {
                    has_binding: tir_fields.iter().flatten().any(|f| f.has_binding),
                    is_refutable: tir_fields.iter().flatten().any(|f| f.is_refutable),
                    kind: PatKindTir::Unit(UnitPatKindTir::Struct {
                        fields: self.arena.alloc_iter(tir_fields.into_iter().map(|f| {
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
                let ty_base = ty.ptr_base(self.ext.types);
                let Ty::Enum(enum_ty) = ty_base.base(self.ext.types) else {
                    UnexpectedPatternType {
                        loc: self.meta.loc(ctor.span()),
                        ty: self.ext.creator().display(ty),
                        ty_loc: None, //TODO: make a types getter for loc on type
                        something: "enum",
                    }.add(self.ext.workspace)?;
                };

                let (index, variant_ty) = self
                    .ext
                    .creator()
                    .find_component(ty_base, ctor.name.ident)
                    .or_else(|| {
                        ComponentNotFound {
                            loc: self.meta.loc(ctor.span()),
                            ty: self.ext.creator().display(Ty::Enum(enum_ty)),
                            suggestions: self.ext.types[self.ext.types[enum_ty].variants]
                                .iter()
                                .map(|v| v.name.get(self.ext.interner))
                                .intersperse(", ")
                                .collect(),
                            something: "variant",
                        }
                        .add(self.ext.workspace)?
                    })?;

                let value = ctor
                    .value
                    .map(|(.., body)| self.pattern(body, variant_ty))
                    .transpose()?;

                Some(PatTir {
                    kind: PatKindTir::Unit(UnitPatKindTir::Enum {
                        id: index as u32,
                        ty: enum_ty,
                        value: value.map(|value| self.arena.alloc(value)),
                    }),
                    span: ctor.span(),
                    ty,
                    has_binding: value.map_or(false, |v| v.has_binding),
                    is_refutable: self.ext.types[enum_ty].variants.len() > 1,
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

    fn infer_params(
        &mut self,
        params: &mut [Option<Ty>],
        reference: Ty,
        template: Ty,
        span: Span,
    ) -> Option<()> {
        self.ext
            .types
            .compatible(params, reference, template)
            .map_err(|_| {
                GenericTypeMismatch {
                    expected: self.ext.creator().display(reference),
                    got: self.ext.creator().display(template),
                    loc: self.meta.loc(span),
                }
                .add(self.ext.workspace)
            })
            .ok()
    }

    fn binary_expr(
        &mut self,
        binary_ast @ BinaryExprAst { lhs, op, rhs }: BinaryExprAst,
    ) -> ExprRes<'arena> {
        let lhs = self.expr(lhs, Inference::None);

        if op.ident == Interner::ASSIGN {
            let rhs = self.expr(rhs, lhs.map(|lhs| lhs.ty).into())?;

            if let Some(lhs) = lhs && lhs.flags.contains(TirFlags::IMMUTABLE) {
                NotMutable {
                    loc: self.meta.loc(lhs.span),
                }.add(self.ext.workspace);
            }

            return Some(TirNode::new(
                Ty::UNIT,
                TirKind::Assign(self.arena.alloc(AssignTir { lhs: lhs?, rhs })),
                binary_ast.span(),
            ));
        }

        let rhs = self.expr(rhs, Inference::None);
        let (lhs, rhs) = (lhs?, rhs?); // recovery

        let func = self.find_binary_func(op, lhs.ty, rhs.ty)?;

        let ty = self.ext.types[func].signature.ret;
        let call = CallTir {
            func: CallableTir::Func(func),
            params: default(),
            args: self.arena.alloc_slice(&[lhs, rhs]),
        };
        Some(TirNode::new(
            ty,
            TirKind::Call(self.arena.alloc(call)),
            binary_ast.span(),
        ))
    }

    fn args(
        &mut self,
        types: FragSlice<Ty>,
        args: Option<ListAst<FuncArgAst>>,
    ) -> &'arena [PatTir<'arena>] {
        let Some(args) = args else {return &[]};
        let args = self.ext.types[types]
            .to_bumpvec()
            .into_iter()
            .zip(args.iter())
            .filter_map(|(ty, &arg)| self.pattern(arg.pat, ty))
            .nsc_collect::<BumpVec<_>>();
        self.arena.alloc_iter(args)
    }

    fn type_check(&mut self, expected: Ty, got: Ty, span: Span) -> Option<()> {
        self.type_check_detailed(expected, got, |s| {
            GenericTypeMismatch {
                expected: s.ext.creator().display(expected),
                got: s.ext.creator().display(got),
                loc: s.meta.loc(span),
            }
            .add(s.ext.workspace)
        })
    }

    fn type_check_detailed<A>(
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

    fn parser(&mut self) -> TypecParser<'arena, '_> {
        TypecParser::new(self.arena, self.ctx, self.ext.clone_borrow(), self.meta)
    }

    pub(crate) fn expr_body(
        &mut self,
        expr: ExprAst,
        inference: Inference,
    ) -> Option<(TirNode<'arena>, Ty)> {
        let expr = self.expr(expr, inference)?;
        Some((
            self.return_low(Some(expr), expr.span)?,
            self.ret.expect("return_low ensures ret is set"),
        ))
    }
}

pub enum DotPathResult {
    Field(u32, bool, Ty),
}

pub enum FuncLookupResult<'arena> {
    Func(FragRef<Func>),
    SpecFunc(FragRef<SpecFunc>, Ty),
    #[allow(dead_code)]
    Var(TirNode<'arena>),
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

    #[err => "not all {something} parameters can be inferred"]
    #[info => "missing parameters: {missing}"]
    #[help => "provide the parameters {syntax}"]
    error UnknownTypeParameters: fatal {
        #[err loc]
        missing ref: String,
        loc: SourceLoc,
        something: &'static str,
        syntax: &'static str,
    }
}

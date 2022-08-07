use std::default::default;

use diags::*;
use lexing_t::*;
use packaging_t::*;
use parsing_t::*;
use scope::*;
use storage::*;
use type_checking_t::*;

use crate::*;

type Expected = Option<Ty>;

impl FuncParser<'_> {
    pub fn bound_impls(&mut self, impls: impl IntoIterator<Item = (AstEnt, Impl)>) {
        for (ast, r#impl) in impls {
            let [generics, .., body] = self.ast_data[ast.children] else {
                unreachable!();
            };

            self.scope.start_frame();
            self.push_generics(generics, self.typec.impls[r#impl].params);

            for &item in &self.ast_data[body.children] {
                match item.kind {
                    AstKind::ImplType => (),
                    AstKind::ImplUse | AstKind::Func { .. } => (),
                    kind => unimplemented!("{:?}", kind),
                }
            }

            let mut funcs = vec![Maybe::none(); self.typec.func_count_of_impl(r#impl)];

            for &item in &self.ast_data[body.children] {
                match item.kind {
                    AstKind::Func { .. } => drop(self.bound_impl_func(item, r#impl, &mut funcs)),
                    AstKind::ImplUse => (),
                    AstKind::ImplType => (),
                    kind => unimplemented!("{:?}", kind),
                }
            }

            self.scope.end_frame();

            let mut missing = funcs
                .iter()
                .enumerate()
                .filter_map(|(i, maybe)| maybe.is_none().then_some(i))
                .peekable();

            if missing.peek().is_some() {
                let suggestions = missing
                    .map(|i| self.typec.func_of_impl(r#impl, i))
                    .map(|func| &self.interner[func.name])
                    .collect::<Vec<_>>()
                    .join(", ");

                let loc = self
                    .typec
                    .loc_of(self.typec.impls[r#impl].bound, self.interner);
                self.workspace.push(diag! {
                    (ast.span, self.current_file) => "some of the methods are not implemented",
                    (none) => "missing methods: {}" { suggestions },
                    (exp loc) => "related bound type",
                });
                continue;
            }

            self.typec.impls[r#impl].funcs = self
                .typec
                .def_lists
                .bump(funcs.into_iter().map(Maybe::unwrap));
        }
    }

    fn bound_impl_func(
        &mut self,
        item: AstEnt,
        r#impl: Impl,
        funcs: &mut Vec<Maybe<Def>>,
    ) -> errors::Result {
        let [cc, generics, ast_name, ref args @ .., ret, body] = self.ast_data[item.children] else {
            unreachable!();
        };

        self.scope.start_frame();
        let params = ty_parser!(self, self.current_file).bounded_generics(generics)?;
        let sig = ty_parser!(self, self.current_file).sig(cc, args, ret)?;

        let name = span_str!(self, ast_name.span);
        let name_id = self.interner.intern_str(name);
        let impl_id = self.typec.impls[r#impl].id;
        let id = self.interner.intern(scoped_ident!(impl_id, name_id));

        let Some(index) = self.typec.func_index_of_impl(r#impl, name_id) else {
            let loc = self.typec.loc_of(self.typec.impls[r#impl].bound, self.interner);
            self.workspace.push(diag! {
                (ast_name.span, self.current_file) => "function with this name does not exist for the bound",
                (exp loc) => "related bound defined here"
            });
            return Err(());
        };

        if let Err(err) =
            bound_checker!(self).compare_bound_signatures(index, default(), sig, r#impl)
        {
            self.report_signature_mismatch(err, r#impl, ast_name.span);
            return Err(());
        }

        if let Some(already) = funcs[index].expand() {
            let span = self.typec.defs[already].loc.expand(self.interner).span;
            self.workspace.push(diag! {
                (ast_name.span, self.current_file) => "function with this name is already implemented",
                (span, self.current_file) => "previously defined here"
            });
            return Ok(());
        }

        let def_ent = DefEnt {
            params,
            flags: FuncFlags::GENERIC & params.is_some(),
            loc: Loc::new(
                Some(ast_name.span.start),
                self.current_file,
                name,
                self.interner,
            ),
            sig,
            ..default()
        };
        let def = self.typec.defs.insert_unique(id, def_ent);

        funcs[index] = Maybe::some(def);

        self.tir_data.clear();
        self.func_parser_ctx.current_fn = def.into();
        let body = self.r#fn(body, generics, args, self.typec.defs[def].sig.ret.expand())?;
        self.typec.defs[def].tir_data = self.tir_data.clone();
        self.typec.defs[def].body = body.into();

        Ok(())
    }

    pub fn report_signature_mismatch(&mut self, err: SignatureError, r#impl: Impl, span: Span) {
        let loc = self
            .typec
            .loc_of(self.typec.impls[r#impl].bound, self.interner);
        let diag = match err {
            SignatureError::CallConv(expected, got) => diag!(
                (span, self.current_file) => "call convention mismatch, expected {} but got {}" {
                    expected.expand().map_or("\"default\"", |cc| &self.interner[cc]),
                    got.expand().map_or("\"default\"", |cc| &self.interner[cc]),
                },
                (exp loc) => "related bound defined here",
            ),
            SignatureError::ArgCount(expected, got) => diag!(
                (span, self.current_file) => "argument count mismatch, expected {} but got {}" {
                    expected,
                    got,
                },
                (exp loc) => "related bound defined here",
            ),
            SignatureError::Arg(pos, expected, got) => diag!(
                (span, self.current_file) => "argument {} mismatch, expected {} but got {}" {
                    pos,
                    &self.interner[self.typec.types.id(expected)],
                    &self.interner[self.typec.types.id(got)],
                },
                (exp loc) => "related bound defined here",
            ),
            SignatureError::Ret(expected, got) => diag!(
                (span, self.current_file) => "return type mismatch, expected {} but got {}" {
                    expected.expand().map_or("nothing", |expected| &self.interner[self.typec.types.id(expected)]),
                    got.expand().map_or("nothing", |got| &self.interner[self.typec.types.id(got)]),
                },
                (exp loc) => "related bound defined here",
            ),
        };
        self.workspace.push(diag);
    }

    pub fn funcs(&mut self, funcs: impl IntoIterator<Item = (AstEnt, Def)>) {
        for (ast, def) in funcs {
            let [_cc, generics, _name, ref args @ .., _ret, body] = self.ast_data[ast.children] else {
                unreachable!();
            };

            self.tir_data.clear();
            self.func_parser_ctx.current_fn = def.into();
            let Ok(body) = self.r#fn(body, generics, args, self.typec.defs[def].sig.ret.expand()) else {
                continue;
            };
            self.typec.defs[def].tir_data = self.tir_data.clone();
            self.typec.defs[def].body = body.into();
        }
    }

    fn r#fn(
        &mut self,
        body: AstEnt,
        generics: AstEnt,
        args: &[AstEnt],
        ret: Expected,
    ) -> errors::Result<Maybe<TirList>> {
        self.scope.start_frame();

        let parsed_generics = self
            .typec
            .params_of_def(self.func_parser_ctx.current_fn.unwrap());

        self.push_generics(generics, parsed_generics);
        self.push_args(args);

        let TirKind::Block { stmts } = self.block(body, ret).kind else {
            unreachable!();
        };

        self.scope.end_frame();
        Ok(stmts)
    }

    fn block(&mut self, body: AstEnt, expected: Expected) -> TirEnt {
        let stmts = &self.ast_data[body.children];

        let mut last = TirEnt::default();

        let mut reserved = self.tir_data.reserve(stmts.len());
        for (i, &item) in stmts.iter().rev().enumerate().rev() {
            let expected = (i == 0).then_some(expected).flatten();
            last = self.expr(item, expected).unwrap_or_default();
            self.tir_data.push_to_reserved(&mut reserved, last);
            if last.terminating() {
                break;
            }
        }
        let stmts = self
            .tir_data
            .fill_reserved(reserved, TirEnt::new(TirKind::Unreachable));

        TirEnt {
            kind: TirKind::Block { stmts },
            ..last
        }
    }

    fn expr(&mut self, ast: AstEnt, expected: Expected) -> errors::Result<TirEnt> {
        let expr = match ast.kind {
            AstKind::Binary => self.binary(ast)?,
            AstKind::Return => self.r#return(ast)?,

            AstKind::String => TirEnt::new(TirKind::String)
                .span(ast.span)
                .ty(BuiltinTypes::STR),
            AstKind::Int => self.int(ast, expected),

            AstKind::Ident | AstKind::IdentChain => self.ident(ast)?,

            AstKind::Struct { .. }
            | AstKind::StructBody
            | AstKind::StructField { .. }
            | AstKind::Func { .. }
            | AstKind::FuncArg { .. }
            | AstKind::FuncBody
            | AstKind::Generics
            | AstKind::GenericParam
            | AstKind::TyInstance
            | AstKind::PtrTy { .. }
            | AstKind::ManifestSection
            | AstKind::ManifestImports
            | AstKind::ManifestImport { .. }
            | AstKind::ManifestField
            | AstKind::Imports
            | AstKind::Import
            | AstKind::Operator
            | AstKind::OperatorWithModule
            | AstKind::ImplBody
            | AstKind::BoundBody
            | AstKind::Bound { .. }
            | AstKind::BoundType { .. }
            | AstKind::BoundImpl { .. }
            | AstKind::Impl { .. }
            | AstKind::ImplType
            | AstKind::FuncSignature { .. }
            | AstKind::ImplUse
            | AstKind::FieldTy
            | AstKind::None => unreachable!(),
        };

        self.expect_type(expr, expected);

        Ok(expr)
    }

    fn binary(&mut self, ast: AstEnt) -> errors::Result<TirEnt> {
        let [lhs, op, rhs] = self.ast_data[ast.children] else {
            unreachable!();
        };

        let op_id = self.op_id(op, true);
        let def: Def = self.get_from_scope_concrete(op_id, op.span, "binary operator not found")?;

        let sig = self.typec.defs[def].sig;
        let [left_ty, right_ty] = self.typec.ty_lists[sig.args] else {
            unreachable!();
        };

        let lhs = self.expr(lhs, Some(left_ty));
        let rhs = self.expr(rhs, Some(right_ty));
        let (lhs, rhs) = (lhs?, rhs?);
        let args = self.tir_data.bump([lhs, rhs]);

        Ok(TirEnt::new(TirKind::Call {
            def,
            params: Maybe::none(),
            args,
        })
        .ty(sig.ret)
        .span(ast.span))
    }

    fn int(&mut self, ast: AstEnt, expected: Expected) -> TirEnt {
        let ty = {
            let mapping = [
                (BuiltinTypes::I8, "i8"),
                (BuiltinTypes::I16, "i16"),
                (BuiltinTypes::I32, "i32"),
                (BuiltinTypes::I64, "i64"),
                (BuiltinTypes::U8, "u8"),
                (BuiltinTypes::U16, "u16"),
                (BuiltinTypes::U32, "u32"),
                (BuiltinTypes::U64, "u64"),
                (BuiltinTypes::UINT, "u"),
            ];

            mapping
                .into_iter()
                .find_map(|(ty, str)| span_str!(self, ast.span).ends_with(str).then_some(ty))
                .or(expected)
                .unwrap_or(BuiltinTypes::INT)
        };

        TirEnt::new(TirKind::Int).ty(ty).span(ast.span)
    }

    fn ident(&mut self, ast: AstEnt) -> errors::Result<TirEnt> {
        let id = ty_parser!(self, self.current_file).ident_chain_id(ast);

        let something = self.get_from_scope(id, ast.span, "symbol not found")?;

        let result = if let Some(var) = something.try_read::<Tir>() {
            TirEnt::new(TirKind::Access { var })
                .span(ast.span)
                .ty(self.tir_data[var].ty)
        } else {
            unimplemented!();
        };

        Ok(result)
    }

    fn r#return(&mut self, ast: AstEnt) -> errors::Result<TirEnt> {
        let [r#return] = self.ast_data[ast.children] else {
            unreachable!();
        };

        let expr = if r#return.kind.is_none() {
            TirEnt::new(TirKind::Unreachable)
        } else {
            let expected = self.typec.defs[self.func_parser_ctx.current_fn.unwrap()]
                .sig
                .ret;
            self.expr(r#return, expected.expand())?
        };

        let (value, _) = self.tir_data.bump_push(expr);

        Ok(TirEnt::new(TirKind::Return {
            value: value.into(),
        })
        .span(ast.span))
    }

    fn op_id(&mut self, op: AstEnt, binary: bool) -> Ident {
        let kind = if binary { "binary" } else { "unary" };
        match op.kind {
            AstKind::Operator => {
                self.interner
                    .intern(ident!(kind, "(", span_str!(self, op.span), ")"))
            }
            AstKind::OperatorWithModule => {
                let [op, module] = self.ast_data[op.children] else {
                    unreachable!();
                };

                self.interner.intern(ident!(
                    span_str!(self, module.span),
                    "`",
                    kind,
                    "(",
                    span_str!(self, op.span),
                    ")"
                ))
            }
            kind => unimplemented!("{:?}", kind),
        }
    }

    fn expect_type(&mut self, tir: TirEnt, expected: Expected) {
        if let (Some(got), Some(expected)) = (tir.ty.expand(), expected)
            && got != expected
        {
            self.workspace.push(diag! {
                (tir.span, self.current_file) error => "type mismatch",
                (none) => "expected {} but got {}" {
                    &self.interner[self.typec.types.id(expected)],
                    &self.interner[self.typec.types.id(got)],
                },
            })
        }
    }

    fn push_args(&mut self, args: &[AstEnt]) {
        let types = self.typec.args_of(self.func_parser_ctx.current_fn.unwrap());
        let mut reserved = self.tir_data.reserve(args.len());
        for (i, (arg, &ty)) in args
            .iter()
            .zip(self.typec.ty_lists[types].iter())
            .enumerate()
        {
            let [name, ..] = self.ast_data[arg.children] else {
                unreachable!();
            };

            let id = self.interner.intern_str(span_str!(self, name.span));
            let argument = TirEnt::new(TirKind::Argument(i as u8))
                .ty(ty)
                .span(name.span);
            let arg = self.tir_data.push_to_reserved(&mut reserved, argument);
            self.scope
                .push(ScopeItem::new(id, arg, name.span, self.current_file));
        }
        self.tir_data.finish_reserved(reserved);
    }

    fn push_generics(&mut self, generics: AstEnt, parsed_generics: Maybe<TyList>) {
        for (&ast_param, &param) in self.ast_data[generics.children]
            .iter()
            .zip(self.typec.ty_lists[parsed_generics].iter())
        {
            let [name, ..] = self.ast_data[ast_param.children] else {
                unreachable!();
            };

            let id = self.interner.intern_str(span_str!(self, name.span));
            self.scope
                .push(ScopeItem::new(id, param, name.span, self.current_file));
        }
    }

    scope_error_handler!();
}

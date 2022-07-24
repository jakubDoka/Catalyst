use diags::*;
use packaging_t::*;
use parsing_t::*;
use scope::*;
use storage::*;
use type_checking_t::*;

use crate::*;

type Expected = Option<Ty>;

impl FnParser<'_> {
    pub fn fns(&mut self, fns: &mut Vec<(AstEnt, Def)>) -> errors::Result {
        for (ast, def) in fns.drain(..) {
            let [_cc, generics, _name, ref args @  .., _ret, body] = self.ast_data[ast.children] else {
                unreachable!();
            };

            self.tir_data.clear();
            self.fn_parser_ctx.current_fn = def.into();
            let Ok(body) = self.r#fn(body, generics, args, self.fns.defs[def].sig.ret.expand()) else {
                continue;
            };
            self.fns.defs[def].tir_data = self.tir_data.clone();
            self.fns.defs[def].body = body.into();
        }

        Ok(())
    }

    fn r#fn(
        &mut self,
        body: AstEnt,
        generics: AstEnt,
        args: &[AstEnt],
        ret: Expected,
    ) -> errors::Result<Maybe<TirList>> {
        self.scope.start_frame();

        self.push_generics(generics);
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
            | AstKind::Fn { .. }
            | AstKind::FnArg { .. }
            | AstKind::FnBody
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
        let def = self
            .scope
            .get_concrete::<Def>(op_id)
            .map_err(scope_error_handler!(
                self,
                op.span,
                op_id,
                "binary operator not found"
            ))?;

        let sig = self.fns.defs[def].sig;
        let [left_ty, right_ty] = self.types.slices[sig.args] else {
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
        let id = ident_chain_id!(self, ast);

        let something = self.scope.get(id).map_err(scope_error_handler!(
            self,
            ast.span,
            id,
            "symbol not found"
        ))?;

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
            let expected = self.fns.defs[self.fn_parser_ctx.current_fn.unwrap()]
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
                    &self.interner[self.types.ents.id(expected)],
                    &self.interner[self.types.ents.id(got)],
                },
            })
        }
    }

    fn push_args(&mut self, args: &[AstEnt]) {
        let types = self.fns.args_of(self.fn_parser_ctx.current_fn.unwrap());
        let mut reserved = self.tir_data.reserve(args.len());
        for (i, (arg, &ty)) in args.iter().zip(self.types.slices[types].iter()).enumerate() {
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

    fn push_generics(&mut self, generics: AstEnt) {
        let params = self
            .fns
            .params_of_def(self.fn_parser_ctx.current_fn.unwrap());
        for (&ast_param, &param) in self.ast_data[generics.children]
            .iter()
            .zip(self.types.slices[params].iter())
        {
            let [name, ..] = self.ast_data[ast_param.children] else {
                unreachable!();
            };

            let id = self.interner.intern_str(span_str!(self, name.span));
            self.scope
                .push(ScopeItem::new(id, param, name.span, self.current_file));
        }
    }
}

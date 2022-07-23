use packaging_t::*;
use parsing_t::*;
use scope::ScopeItem;
use storage::Maybe;
use type_checking_t::*;

use crate::*;

impl FnParser<'_> {
    pub fn fns(&mut self, fns: &mut Vec<(AstEnt, Def)>) -> errors::Result {
        for (ast, def) in fns.drain(..) {
            let [_cc, generics, _name, ref args @  .., _ret, body] = self.ast_data[ast.children] else {
                unreachable!();
            };

            self.tir_data.clear();
            self.fn_parser_ctx.current_fn = def.into();
            let Ok(body) = self.r#fn(body, generics, args) else {
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
    ) -> errors::Result<Maybe<TirList>> {
        self.scope.start_frame();

        self.push_generics(generics);
        self.push_args(args);

        let TirKind::Block { stmts } = self.block(body).kind else {
            unreachable!();
        };

        self.scope.end_frame();
        Ok(stmts)
    }

    fn block(&mut self, body: AstEnt) -> TirEnt {
        let stmts = &self.ast_data[body.children];

        let mut last = TirEnt::default();

        let mut reserved = self.tir_data.reserve(stmts.len());
        for &item in stmts {
            last = self.expr(item).unwrap_or_default();
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

    fn expr(&mut self, ast: AstEnt) -> errors::Result<TirEnt> {
        match ast.kind {
            AstKind::Return => self.r#return(ast),

            AstKind::String => Ok(self.capture(ast, TirKind::String)),
            AstKind::Int => Ok(self.capture(ast, TirKind::Int)),

            AstKind::Ident | AstKind::IdentChain => self.ident(ast),

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
            | AstKind::None => unreachable!(),
        }
    }

    fn ident(&mut self, ast: AstEnt) -> errors::Result<TirEnt> {
        let id = ident_chain_id!(self, ast);

        let something = self.scope.get(id).map_err(scope_error_handler!(
            self,
            ast.span,
            id,
            "symbol not found"
        ))?;

        if let Some(var) = something.try_read::<Tir>() {
            return Ok(TirEnt::new(TirKind::Access { var }).span(ast.span));
        }

        unimplemented!();
    }

    fn r#return(&mut self, ast: AstEnt) -> errors::Result<TirEnt> {
        let [r#return] = self.ast_data[ast.children] else {
            unreachable!();
        };

        let expr = if r#return.kind.is_none() {
            TirEnt::new(TirKind::Unreachable)
        } else {
            self.expr(r#return)?
        };

        let (value, _) = self.tir_data.bump_push(expr);

        Ok(TirEnt::new(TirKind::Return {
            value: value.into(),
        })
        .span(ast.span))
    }

    fn capture(&mut self, ast: AstEnt, kind: TirKind) -> TirEnt {
        TirEnt {
            kind,
            span: ast.span.into(),
            ..TirEnt::default()
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

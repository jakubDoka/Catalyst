use crate::*;
use ast::*;
use lexer::*;
use storage::*;
use typec_types::*;

impl TyParser<'_> {
    pub fn parse_type(&mut self, ty: Ast) -> errors::Result<Ty> {
        let Some(res) = self.parse_type_optional(ty)? else {
            let span = self.ast_data.nodes[ty].span;
            self.diagnostics
                .push(TyError::ExpectedConcreteType { loc: span });
            return Err(());
        };
        Ok(res)
    }

    /// parse a type just like `parse_type` but can return `Ty::reserved_value` in case the ty is '_'.
    pub fn parse_type_optional(&mut self, ty: Ast) -> errors::Result<Option<Ty>> {
        let ast::AstEnt { kind, span, .. } = self.ast_data.nodes[ty];
        match kind {
            AstKind::Ident => return self.parse_ident_type(span),
            AstKind::Instantiation => self.parse_instance_type(ty),
            AstKind::Ref(mutable) => self.parse_ptr_type(ty, mutable),
            AstKind::FuncPtr => self.parse_func_ptr_type(ty),
            _ => {
                self.diagnostics
                    .push(TyError::InvalidTypeExpression { loc: span });
                return Err(());
            }
        }
        .map(Some)
    }

    pub fn parse_func_ptr_type(&mut self, ty: Ast) -> errors::Result<Ty> {
        let children = self.ast_data.children(ty);
        let &[cc, .., ret] = children else {
            unreachable!();
        };

        let mut generic = false;

        let cc = parse_call_conv(cc, self.sources, self.ast_data, self.diagnostics);

        let args = {
            let mut args = self.vec_pool.with_capacity(children.len() - 2);
            for &param in &children[1..children.len() - 1] {
                let ty = self.parse_type(param)?;
                generic |= self.types[ty].flags.contains(TyFlags::GENERIC);
                args.push(TyCompEnt {
                    ty,
                    ..Default::default()
                });
            }

            self.ty_comps.push(&args)
        };

        let ret = if ret.is_reserved_value() {
            self.builtin_types.nothing
        } else {
            self.parse_type(ret)?
        };
        generic |= self.types[ret].flags.contains(TyFlags::GENERIC);
        let sig = Sig { args, ret, cc };

        Ok(ty_factory!(self).func_pointer_of(sig, generic))
    }

    pub fn parse_ident_type(&mut self, span: Span) -> errors::Result<Option<Ty>> {
        let str = self.sources.display(span);

        if str == "_" {
            return Ok(None);
        }

        let matcher = matcher!(Func = "function");
        let handler = scope_error_handler(
            self.diagnostics,
            not_found_handler(span),
            span,
            "type",
            matcher,
        );
        self.scope
            .get_concrete::<Ty>(str)
            .map_err(handler)
            .map(Some)
    }

    fn parse_instance_type(&mut self, ty: Ast) -> errors::Result<Ty> {
        let children = self.ast_data.children(ty);
        let header = self.parse_type(children[0])?;

        self.ty_lists.mark_frame();

        for &param in &children[1..] {
            let param = self.parse_type(param)?;
            self.ty_lists.push_one(param);
        }

        let mut new_instances = self.vec_pool.get();
        Ok(ty_factory!(self).parse_instance_type(
            header,
            self.ast_data.nodes[ty].span,
            &mut new_instances,
        ))
    }

    pub fn parse_ptr_type(&mut self, ty: Ast, mutable: bool) -> errors::Result<Ty> {
        let inner_ty = {
            let inner = self.ast_data.children(ty)[0];
            self.parse_type(inner)?
        };

        Ok(ty_factory!(self).pointer_of(inner_ty, mutable))
    }

    pub fn parse_composite_bound(&mut self, asts: &[Ast], span: Span) -> Ty {
        self.ty_lists.mark_frame();
        for &bound in asts {
            let Ok(ty) = self.parse_type(bound) else {
                continue;
            };
            self.ty_lists.push_one(ty);
        }
        self.ty_lists.top_mut().sort_by_key(|ty| ty.0);
        let duplicates = self.ty_lists.top().windows(2).any(|w| w[0] == w[1]);

        if duplicates {
            self.diagnostics.push(TyError::DuplicateBound { loc: span });
        }
        ty_factory!(self).parse_composite_bound_low(span)
    }
}

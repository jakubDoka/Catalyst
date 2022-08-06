use crate::*;
use packaging_t::*;
use parsing_t::*;
use scope::*;
use storage::*;
use type_checking_t::*;

impl TyBuilder<'_> {
    pub fn types(&mut self, types: &mut Vec<(AstEnt, Ty)>) {
        for (ast, ty) in types.drain(..) {
            match ast.kind {
                AstKind::Struct { .. } => drop(self.r#struct(ast, ty)),
                AstKind::Bound { .. } => drop(self.bound(ast, ty)),
                _ => unimplemented!(),
            }
        }
    }

    fn bound(&mut self, ast: AstEnt, ty: Ty) -> errors::Result {
        let &[generics, name, .., body] = &self.ast_data[ast.children] else {
            unreachable!();
        };

        self.scope.start_frame();
        let ty_id = self.interner.intern_str(span_str!(self, name.span));
        self.scope.self_alias = ty_id.into();
        ty_parser!(self, self.current_file).generics(generics);
        let res_funcs = self.bound_funcs(body, ty);
        let TyKind::Bound { ref mut funcs, .. } = self.typec.types[ty].kind else {
            unreachable!();
        };
        *funcs = res_funcs;

        self.scope.self_alias.take();
        self.scope.end_frame();

        Ok(())
    }

    fn bound_funcs(&mut self, ast: AstEnt, ty: Ty) -> Maybe<BoundFuncList> {
        let assoc_type_count = self.typec.assoc_ty_count(ty);
        let func_count = self.ast_data[ast.children].len() - assoc_type_count;

        let mut funcs = self.typec.funcs.reserve(func_count);

        for &item in self.ast_data[ast.children].iter() {
            let AstKind::FuncSignature { vis } = item.kind else {
                continue;
            };

            let Ok(item) = self.func_signature(item, ty, &mut funcs, vis) else {
                continue;
            };

            insert_scope_item!(self, item);
        }

        self.typec.funcs.fill_reserved(funcs, Default::default())
    }

    fn func_signature(
        &mut self,
        ast: AstEnt,
        ty: Ty,
        funcs: &mut Reserved<BoundFuncList>,
        vis: Vis,
    ) -> errors::Result<ModItem> {
        let [cc, generics, name, ref args @ .., ret] = self.ast_data[ast.children] else {
            unreachable!();
        };

        let local_id = self.interner.intern(scoped_ident!(
            span_str!(self, self.typec.types[ty].span.unwrap()),
            span_str!(self, name.span)
        ));
        let id = intern_scoped_ident!(self, local_id);
        self.visibility[id] = vis;

        if let Some(already) = self.typec.types.get(id) {
            duplicate_definition!(self, ast.span, already.span);
            return Err(());
        }

        self.scope.start_frame();
        let params = ty_parser!(self, self.current_file).bounded_generics(generics)?;
        let sig = ty_parser!(self, self.current_file).sig(cc, args, ret)?;
        self.scope.end_frame();

        let bound_func_ent = BoundFuncEnt {
            sig,
            params,
            span: name.span.into(),
        };
        let bound_func = self.typec.funcs.push_to_reserved(funcs, bound_func_ent);

        Ok(ModItem::new(local_id, bound_func, name.span))
    }

    fn r#struct(&mut self, ast: AstEnt, ty: Ty) -> errors::Result {
        let &[generics, .., body] = &self.ast_data[ast.children] else {
            unreachable!();
        };

        self.scope.start_frame();

        ty_parser!(self, self.current_file).generics(generics);
        let fields = self.struct_fields(body, ty);
        let kind = TyKind::Struct { fields };
        self.typec.types[ty].kind = kind;

        self.scope.end_frame();

        Ok(())
    }

    fn struct_fields(&mut self, body: AstEnt, ty: Ty) -> Maybe<FieldList> {
        for field in &self.ast_data[body.children] {
            let struct_id = self.typec.types.id(ty);

            let &[ast_name, ast_ty] = &self.ast_data[field.children] else {
                unreachable!();
            };
            let AstKind::StructField { vis, mutable, exported } = field.kind else {
                unreachable!();
            };

            let Ok(ty) = ty_parser!(self, self.current_file).parse(ast_ty) else {
                continue;
            };
            let id = self
                .interner
                .intern(field_ident!(struct_id, span_str!(self, ast_name.span)));
            self.visibility[id] = vis;

            let ent = FieldEnt {
                ty,
                name: ast_name.span.into(),
                mutable,
                exported,
            };
            let field = self.typec.fields.push(ent);

            // struct field acts as any other scope item
            let item = ModItem {
                id,
                ptr: ScopePtr::new(field),
                span: ast_name.span,
            };
            insert_scope_item!(self, item);
        }
        self.typec.fields.bump_pushed()
    }
}

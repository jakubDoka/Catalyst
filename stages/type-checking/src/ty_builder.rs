use crate::*;
use packaging_t::*;
use parsing_t::*;
use scope::*;
use storage::*;
use type_checking_t::*;

impl TyBuilder<'_> {
    pub fn types(&mut self, types: &mut Vec<(AstEnt, Ty)>) -> errors::Result {
        for (ast, ty) in types.drain(..) {
            match ast.kind {
                AstKind::Struct { .. } => drop(self.r#struct(ast, ty)),
                AstKind::Bound { .. } => drop(self.bound(ast, ty)),
                _ => unimplemented!(),
            }
        }
        Ok(())
    }

    fn bound(&mut self, ast: AstEnt, ty: Ty) -> errors::Result {
        let &[generics, .., body] = &self.ast_data[ast.children] else {
            unreachable!();
        };

        self.scope.start_frame();

        self.generics(generics);
        let (assoc_types, funcs) = self.bound_items(body, ty);
        let kind = TyKind::Bound {
            assoc_types,
            funcs,
            inherits: Maybe::none(),
        };
        self.types.ents[ty].kind = kind;

        self.scope.end_frame();

        Ok(())
    }

    fn bound_items(&mut self, ast: AstEnt, ty: Ty) -> (Maybe<TyList>, Maybe<BoundFuncList>) {
        let assoc_type_count = self.ast_data[ast.children]
            .iter()
            .filter(|item| matches!(item.kind, AstKind::BoundType { .. }))
            .count();
        let func_count = self.ast_data[ast.children].len() - assoc_type_count;

        let (mut assoc_types, mut funcs) = (
            self.types.slices.reserve(assoc_type_count),
            self.types.funcs.reserve(func_count),
        );

        for &item in self.ast_data[ast.children].iter() {
            let res = match item.kind {
                AstKind::BoundType { vis } => self.assoc_type(item, ty, &mut assoc_types, vis),
                AstKind::FuncSignature { vis } => self.func_signature(item, ty, &mut funcs, vis),
                kind => unimplemented!("{:?}", kind),
            };

            let Ok(item) = res else {
                continue;
            };

            insert_scope_item!(self, item);
        }

        (
            self.types
                .slices
                .fill_reserved(assoc_types, BuiltinTypes::ANY),
            self.types.funcs.fill_reserved(funcs, Default::default()),
        )
    }

    fn assoc_type(
        &mut self,
        ast: AstEnt,
        ty: Ty,
        assoc_types: &mut Reserved<TyList>,
        vis: Vis,
    ) -> errors::Result<ModItem> {
        let &[generics, name] = &self.ast_data[ast.children] else {
            unreachable!("{:?}", &self.ast_data[ast.children]);
        };

        let local_id = self.interner.intern(scoped_ident!(
            self.types.ents.id(ty),
            span_str!(self, name.span)
        ));
        let id = intern_scoped_ident!(self, local_id);
        self.visibility[id] = vis;

        if let Some(prev) = self.types.ents.get(id) {
            duplicate_definition!(self, ast.span, prev.span);
            return Err(());
        }

        let ty_ent = TyEnt {
            kind: TyKind::AssocType {
                index: self.types.slices.reserve_len(&assoc_types) as u32,
            },
            flags: TyFlags::GENERIC & generics.children.is_some(),
            param_count: self.ast_data[generics.children].len() as u8,
            file: self.current_file.into(),
            span: name.span.into(),
        };
        let ty = self.types.ents.insert_unique(id, ty_ent);
        self.types.slices.push_to_reserved(assoc_types, ty);

        Ok(ModItem::new(local_id, ty, name.span))
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
            self.types.ents.id(ty),
            span_str!(self, name.span)
        ));
        let id = intern_scoped_ident!(self, local_id);
        self.visibility[id] = vis;

        if let Some(already) = self.types.ents.get(id) {
            duplicate_definition!(self, ast.span, already.span);
            return Err(());
        }

        self.scope.start_frame();
        let params = ty_parser!(self, self.current_file).generics(generics)?;
        let sig = ty_parser!(self, self.current_file).sig(cc, args, ret)?;
        self.scope.end_frame();

        let bound_func_ent = BoundFuncEnt {
            sig,
            params,
            span: name.span.into(),
        };
        let bound_func = self.types.funcs.push_to_reserved(funcs, bound_func_ent);

        Ok(ModItem::new(local_id, bound_func, name.span))
    }

    fn r#struct(&mut self, ast: AstEnt, ty: Ty) -> errors::Result {
        let &[generics, .., body] = &self.ast_data[ast.children] else {
            unreachable!();
        };

        self.scope.start_frame();

        self.generics(generics);
        let fields = self.struct_fields(body, ty);
        let kind = TyKind::Struct { fields };
        self.types.ents[ty].kind = kind;

        self.scope.end_frame();

        Ok(())
    }

    fn struct_fields(&mut self, body: AstEnt, ty: Ty) -> Maybe<FieldList> {
        for field in &self.ast_data[body.children] {
            let struct_id = self.types.ents.id(ty);

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
            let field = self.types.fields.push(ent);

            // struct field acts as any other scope item
            let item = ModItem {
                id,
                ptr: ScopePtr::new(field),
                span: ast_name.span,
            };
            insert_scope_item!(self, item);
        }
        self.types.fields.bump_pushed()
    }

    fn generics(&mut self, generics: AstEnt) {
        let mut param = BuiltinTypes::TY_ANY;
        for ast_param in &self.ast_data[generics.children] {
            let id = self.interner.intern_str(span_str!(self, ast_param.span));
            let item = ScopeItem {
                id,
                ptr: ScopePtr::new(param),
                span: ast_param.span,
                module: self.current_file,
            };
            self.scope.push(item);
            param = ty_factory!(self).next_param_of(param);
        }
    }
}

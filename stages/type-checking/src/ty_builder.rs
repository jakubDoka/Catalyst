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
                _ => unimplemented!(),
            }
        }
        Ok(())
    }

    fn r#struct(&mut self, ast: AstEnt, ty: Ty) -> errors::Result {
        let &[generics, .., body] = &self.ast_data[ast.children] else {
            unreachable!();
        };

        self.scope.start_frame();

        self.generics(generics);
        let fields = self.struct_fields(body, ty);
        let kind = TyKind::Struct {
            fields,
            param_count: self.ast_data[generics.children].len() as u32,
        };
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

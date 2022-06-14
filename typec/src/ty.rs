use ast::*;
use lexer::*;
use module_types::*;
use storage::*;
use typec_types::*;

use crate::*;

impl TyBuilder<'_> {
    pub fn build(&mut self) {
        let TyEnt { id, .. } = self.types[self.ty];
        let ast = self.scope_context.type_ast[self.ty];
        if ast.is_reserved_value() {
            return;
        }
        let ast::AstEnt { kind, span, .. } = self.ast_data.nodes[ast];

        self.ty_graph.add_vertex(self.ty);

        match kind {
            AstKind::Struct => self.build_struct(id, ast),
            AstKind::Bound => self.build_bound(id, ast),
            AstKind::Enum => self.build_enum(id, ast),
            _ => unimplemented!(
                "Unhandled type decl {:?}: {}",
                kind,
                self.sources.display(span)
            ),
        }
    }

    fn build_enum(&mut self, id: ID, ast: Ast) {
        let &[generics, .., body] = self.ast_data.children(ast) else {
            unreachable!();
        };

        self.scope.mark_frame();

        self.build_generics(generics);

        let (discriminant_ty, variants) = self.build_variants(id, body);
        self.types[self.ty].kind = TyKind::Enum(discriminant_ty, variants);

        self.scope.pop_frame();
    }

    fn build_variants(&mut self, id: ID, ast: Ast) -> (Ty, TyCompList) {
        let discriminant_ty = match self.ast_data.children(ast).len() {
            const { u16::MAX as usize }.. => panic!("Are you being serious?"),
            256.. => self.builtin_types.u16,
            1.. => self.builtin_types.u8,
            0 => self.builtin_types.nothing,
            _ => unreachable!(),
        };

        // we need to allocate as build_variant also pushes to ty_comps,
        // if this becomes a bottleneck, we will store long lived vec in context
        let mut variants = self
            .vec_pool
            .with_capacity(self.ast_data.children(ast).len() + 1);
        let ent = TyCompEnt {
            ty: discriminant_ty,
            index: 0,
            name: self.builtin_types.discriminant,
        };
        variants.push(ent);

        for (i, &variant) in self.ast_data.children(ast).iter().enumerate() {
            let index = i as u32 + 1;
            let &[name, ty_expr] = self.ast_data.children(variant) else {
                unreachable!();
            };
            let name = self.ast_data.nodes[name].span;

            let Ok(ty) = ty_parser!(self).parse_type(ty_expr) else {
                continue
            };

            self.ty_graph.add_edge(self.ty, ty);

            let ent = TyCompEnt { ty, index, name };

            variants.push(ent);
        }

        for comp in variants.drain(..) {
            let id = ID::owned(id, self.sources.id_of(comp.name));
            let comp_id = self.ty_comps.push_one(comp);
            let module_item = ModuleItem::new(id, comp_id, comp.name);
            self.scope.insert_current(self.diagnostics, module_item);
            self.modules[comp.name.source()].items.push(module_item);
        }

        (discriminant_ty, self.ty_comps.close_frame())
    }

    fn build_bound(&mut self, _id: ID, _ast: Ast) {
        // do absolutely nothing
    }

    fn build_struct(&mut self, id: ID, ast: Ast) {
        let &[generics, .., body] = self.ast_data.children(ast) else {
            unreachable!();
        };

        self.scope.mark_frame();

        self.build_generics(generics);

        let fields = self.build_fields(id, body);
        self.types[self.ty].kind = TyKind::Struct(fields);

        self.scope.pop_frame();
    }

    fn build_fields(&mut self, id: ID, body: Ast) -> TyCompList {
        for (i, &field_ast) in self.ast_data.children(body).iter().enumerate() {
            let &[name, field_ty_ast] = self.ast_data.children(field_ast) else {
                unreachable!();
            };

            let Ok(field_ty) = ty_parser!(self).parse_type(field_ty_ast) else {
                continue;
            };

            let span = self.ast_data.nodes[name].span;
            self.ty_graph.add_edge(self.ty, field_ty);

            let id = {
                let name = self.sources.id_of(span);
                ID::owned(id, name)
            };

            let field = {
                let field = TyCompEnt {
                    name: span,
                    ty: field_ty,
                    index: i as u32,
                };
                self.ty_comps.push_one(field)
            };

            let module_item = ModuleItem::new(id, field, span);
            self.scope.insert_current(self.diagnostics, module_item);
            self.modules[span.source()].items.push(module_item);
        }
        self.ty_comps.close_frame()
    }

    fn build_generics(&mut self, generics: Ast) {
        if generics.is_reserved_value() {
            return;
        }

        let generics = self.ast_data.children(generics);

        // type params have no associated bound
        // so we can use the empty one
        {
            let mut current = self.builtin_types.ty_any;
            for &ident in generics {
                let span = self.ast_data.nodes[ident].span;
                let id = self.sources.id_of(span);
                self.scope.push_item(id, ScopeItem::new(current, span));
                current = get_param(current, self.types);
            }
        }
    }
}

pub fn get_param(ty: Ty, types: &mut Types) -> Ty {
    let next_ty = types.next_key();
    let TyKind::Param(index, .., next) = &mut types[ty].kind else {
        unreachable!();
    };
    let index = *index as u8;

    if let Some(next) = next.expand() {
        return next;
    }

    *next = next_ty.into();

    let mut copy = types[ty];
    // this is only useful for parameters on types
    // but it bring minimal overhead
    let TyKind::Param(other_index, ..) = &mut copy.kind else {
        unreachable!();
    };
    *other_index = index + 1;
    types.push(copy)
}

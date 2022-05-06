use ast::Ast;
use lexer_types::*;
use module_types::{*, scope::Scope, tree::GenericGraph, module::Modules};
use typec_types::*;
use storage::*;

use crate::scope::ScopeContext;
use crate::*;

pub struct TyBuilder<'a> {
    pub scope: &'a mut Scope,
    pub types: &'a mut Types,
    pub sources: &'a Sources,
    pub ast: &'a AstData,
    pub ctx: &'a mut ScopeContext,
    pub graph: &'a mut GenericGraph,
    pub modules: &'a mut Modules,
    pub ty: Ty,
    pub diagnostics: &'a mut errors::Diagnostics,
}

impl<'a> TyBuilder<'a> {
    pub fn build(&mut self) -> errors::Result {
        let TyEnt { id, .. } = self.types.ents[self.ty];
        let ast = self.ctx.type_ast[self.ty];
        if ast.is_reserved_value() {
            return Ok(());
        }
        let ast::AstEnt { kind, span, .. } = self.ast.nodes[ast];

        match kind {
            ast::AstKind::Struct => self.build_struct(id, ast)?,
            ast::AstKind::Bound => self.build_bound(id, ast)?,
            _ => todo!(
                "Unhandled type decl {:?}: {}",
                kind,
                self.sources.display(span)
            ),
        }

        Ok(())
    }

    pub fn build_bound(&mut self, _id: ID, _ast: Ast) -> errors::Result {
        Ok(())
    }

    pub fn build_struct(&mut self, id: ID, ast: Ast) -> errors::Result {
        let &[generics, .., body] = self.ast.children(ast) else {
            unreachable!();
        };

        self.scope.mark_frame();

        if !generics.is_reserved_value() {
            for (&ident, &param) in self
                .ast
                .children(generics)
                .iter()
                .zip(self.types.ty_params())
            {
                let span = self.ast.nodes[ident].span;
                let id = self.sources.id(span);
                self.scope.push_item(id, ScopeItem::new(param, span))
            }
        }

        // fields are inserted into centralized hash map for faster lookup
        // and memory efficiency, though we still need field ordering when
        // calculating offsets
        let fields = {
            for (i, &field_ast) in self.ast.children(body).iter().enumerate() {
                let &[name, field_ty_ast] = self.ast.children(field_ast) else {
                    unreachable!();
                };
                let field_ty = parse_type!(self, field_ty_ast)?;

                let span = self.ast.nodes[name].span;

                let id = {
                    let name = self.sources.id(span);
                    ID::field(id, name)
                };

                let field = {
                    let field = SFieldEnt {
                        span,
                        ty: field_ty,
                        index: i as u32,
                    };
                    self.types.sfields.push_one(field)
                };

                assert!(self
                    .types
                    .sfield_lookup
                    .insert(id, SFieldRef::new(field))
                    .map(|f| f.next.is_some())
                    .unwrap_or(true));

                let field_ty = if let TyKind::Instance(header, ..) = self.types.ents[field_ty].kind {
                    header
                } else {
                    field_ty
                };

                self.graph.add_edge(field_ty.as_u32());
            }
            self.types.sfields.close_frame()
        };

        self.types.ents[self.ty].kind = TyKind::Struct(fields);
        self.graph.close_node();

        self.scope.pop_frame();

        Ok(())
    }

   
}

impl AstIDExt for TyBuilder<'_> {
    fn state(&self) -> (&AstData, &Sources) {
        (self.ast, self.sources)
    }
}
use ast::Ast;
use lexer_types::*;
use module_types::*;
use typec_types::*;
use storage::*;

use crate::scope::ScopeContext;
use crate::*;

pub struct TyBuilder<'a> {
    pub scope: &'a mut Scope,
    pub types: &'a mut Types,
    pub ty_lists: &'a mut TyLists,
    pub sfields: &'a mut SFields,
    pub sfield_lookup: &'a mut SFieldLookup,
    pub builtin_types: &'a BuiltinTypes,
    pub instances: &'a mut Instances,
    pub bound_impls: &'a mut BoundImpls,
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
        let TyEnt { id, .. } = self.types[self.ty];
        let ast = self.ctx.type_ast[self.ty];
        if ast.is_reserved_value() {
            return Ok(());
        }
        let ast::AstEnt { kind, span, .. } = self.ast.nodes[ast];

        match kind {
            AstKind::Struct => self.build_struct(id, ast)?,
            AstKind::Bound => self.build_bound(id, ast)?,
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

        self.build_generics(generics);
        
        let fields = self.build_fields(id, body);
        self.types[self.ty].kind = TyKind::Struct(fields);
        
        self.graph.close_node();

        self.scope.pop_frame();

        Ok(())
    }

    pub fn build_fields(&mut self, id: ID, body: Ast) -> SFieldList {
        for (i, &field_ast) in self.ast.children(body).iter().enumerate() {
            let &[name, field_ty_ast] = self.ast.children(field_ast) else {
                unreachable!();
            };
            
            let Ok(field_ty) = ty_parser!(self).parse_type(field_ty_ast) else {
                continue;
            };

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
                self.sfields.push_one(field)
            };

            assert!(self
                .sfield_lookup
                .insert(id, SFieldRef::new(field))
                .map(|f| f.next.is_some())
                .unwrap_or(true));

            let field_ty = if let TyKind::Instance(header, ..) = self.types[field_ty].kind {
                header
            } else {
                field_ty
            };

            self.graph.add_edge(field_ty.as_u32());
        }
        self.sfields.close_frame()
    }

    pub fn build_generics(&mut self, generics: Ast) {
        if generics.is_reserved_value() {
            return;
        }
        
        let generics = self.ast.children(generics);
        
        // type params have no associated bound
        // so we can use the empty one
        {
            let mut current = self.builtin_types.ty_any;
            for &ident in generics {
                let span = self.ast.nodes[ident].span;
                let id = self.sources.id(span);
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
use std::sync::atomic::{AtomicUsize, AtomicIsize, Ordering};

use ast::Ast;
use lexer_types::*;
use module_types::*;
use storage::*;
use typec_types::*;

use crate::scope::ScopeContext;
use crate::*;

pub struct TyBuilder<'a> {
    pub scope: &'a mut Scope,
    pub types: &'a mut Types,
    pub ty_lists: &'a mut TyLists,
    pub ty_comps: &'a mut TyComps,
    pub ty_comp_lookup: &'a mut TyCompLookup,
    pub builtin_types: &'a BuiltinTypes,
    pub instances: &'a mut Instances,
    pub bound_impls: &'a mut BoundImpls,
    pub sources: &'a Sources,
    pub ast: &'a AstData,
    pub ctx: &'a mut ScopeContext,
    pub ty_graph: &'a mut Graph<Ty>,
    pub modules: &'a mut Modules,
    pub ty: Ty,
    pub diagnostics: &'a mut errors::Diagnostics,
}

#[macro_export]
macro_rules! ty_builder {
    ($self:expr, $ty:expr) => {
        TyBuilder::new(
            &mut $self.scope,
            &mut $self.types,
            &mut $self.ty_lists,
            &mut $self.ty_comps,
            &mut $self.ty_comp_lookup,
            &$self.builtin_types,
            &mut $self.instances,
            &mut $self.bound_impls,
            &$self.sources,
            &$self.ast,
            &mut $self.scope_context,
            &mut $self.ty_graph,
            &mut $self.modules,
            $ty,
            &mut $self.diagnostics,
        )
    };
}

impl<'a> TyBuilder<'a> {
    pub fn new(
        scope: &'a mut Scope,
        types: &'a mut Types,
        ty_lists: &'a mut TyLists,
        ty_comps: &'a mut TyComps,
        ty_comp_lookup: &'a mut TyCompLookup,
        builtin_types: &'a BuiltinTypes,
        instances: &'a mut Instances,
        bound_impls: &'a mut BoundImpls,
        sources: &'a Sources,
        ast: &'a AstData,
        ctx: &'a mut ScopeContext,
        graph: &'a mut Graph<Ty>,
        modules: &'a mut Modules,
        ty: Ty,
        diagnostics: &'a mut errors::Diagnostics,
    ) -> Self {
        Self {
            scope,
            types,
            ty_lists,
            ty_comps,
            ty_comp_lookup,
            builtin_types,
            instances,
            bound_impls,
            sources,
            ast,
            ctx,
            ty_graph: graph,
            modules,
            ty,
            diagnostics,
        }
    }

    pub fn build(&mut self) {
        let TyEnt { id, .. } = self.types[self.ty];
        let ast = self.ctx.type_ast[self.ty];
        if ast.is_reserved_value() {
            return;
        }
        let ast::AstEnt { kind, span, .. } = self.ast.nodes[ast];

        self.ty_graph.add_vertex(self.ty);

        match kind {
            AstKind::Struct => self.build_struct(id, ast),
            AstKind::Bound => self.build_bound(id, ast),
            AstKind::Enum => self.build_enum(id, ast),
            _ => todo!(
                "Unhandled type decl {:?}: {}",
                kind,
                self.sources.display(span)
            ),
        }
    }

    fn build_enum(&mut self, id: ID, ast: Ast) {
        let &[generics, .., body] = self.ast.children(ast) else {
            unreachable!();
        };

        self.scope.mark_frame();

        self.build_generics(generics);

        let (discriminant_ty, variants) = self.build_variants(id, body);
        self.types[self.ty].kind = TyKind::Enum(discriminant_ty, variants);

        self.scope.pop_frame();
    }

    fn build_variants(&mut self, id: ID, ast: Ast) -> (Ty, TyCompList) {
        let discriminant_ty = match self.ast.children(ast).len() {
            const { u16::MAX as usize }.. => panic!("Are you being serious?"),
            256.. => self.builtin_types.i16,
            1.. => self.builtin_types.i8,
            0 => self.builtin_types.nothing,
            _ => unreachable!(),
        };

        // we need to allocate as build_variant also pushes to ty_comps,
        // if this becomes a bottleneck, we will store long lived vec in context
        let mut variants = Vec::with_capacity(self.ast.children(ast).len());
        for (i, &variant) in self.ast.children(ast).iter().enumerate() {
            let index = i as u32 + 1; 
            let &[name, ty_expr] = self.ast.children(variant) else {
                unreachable!();
            };
            let name = self.ast.nodes[name].span;
            let variant_id = ID::field(id, self.sources.id_of(name));
        
            let Ok(ty) = ty_parser!(self).parse_type(ty_expr) else {
                continue
            };

            self.ty_graph.add_edge(self.ty, ty);

            let ent = TyCompEnt {
                ty,
                index,
                span: self.ast.nodes[variant].span,
            };

            variants.push((ent, variant_id));
        }

        for (comp, variant_id) in variants {
            let comp = self.ty_comps.push_one(comp);
            self.ty_comp_lookup.insert(variant_id, comp);
        }

        (discriminant_ty, self.ty_comps.close_frame())
    }

    fn build_bound(&mut self, _id: ID, _ast: Ast) {
        // do absolutely nothing
    }

    fn build_struct(&mut self, id: ID, ast: Ast) {
        let &[generics, .., body] = self.ast.children(ast) else {
            unreachable!();
        };

        self.scope.mark_frame();

        self.build_generics(generics);

        let fields = self.build_fields(id, body);
        self.types[self.ty].kind = TyKind::Struct(fields);

        self.scope.pop_frame();
    }

    fn build_fields(&mut self, id: ID, body: Ast) -> TyCompList {
        for (i, &field_ast) in self.ast.children(body).iter().enumerate() {
            let &[name, field_ty_ast] = self.ast.children(field_ast) else {
                unreachable!();
            };

            let Ok(field_ty) = ty_parser!(self).parse_type(field_ty_ast) else {
                continue;
            };

            self.ty_graph.add_edge(self.ty, field_ty);

            let span = self.ast.nodes[name].span;

            let id = {
                let name = self.sources.id_of(span);
                ID::field(id, name)
            };

            let field = {
                let field = TyCompEnt {
                    span,
                    ty: field_ty,
                    index: i as u32,
                };
                self.ty_comps.push_one(field)
            };

            assert!(self
                .ty_comp_lookup
                .insert(id, field)
                .is_none());
        }
        self.ty_comps.close_frame()
    }

    fn build_generics(&mut self, generics: Ast) {
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
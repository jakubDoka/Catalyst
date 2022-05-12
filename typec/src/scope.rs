
use std::vec;

use incr::Incr;
use module_types::{scope::{Scope, self}, module::{Modules, self}};
use lexer_types::*;
use storage::*;
use typec_types::*;
use crate::{*, ty::get_param};
use ast::*;

pub struct ScopeBuilder<'a> {
    pub scope: &'a mut Scope,
    pub funcs: &'a mut Funcs,
    pub types: &'a mut Types,
    pub ty_lists: &'a mut TyLists,
    pub sfields: &'a mut SFields,
    pub sfield_lookup: &'a mut SFieldLookup,
    pub builtin_types: &'a BuiltinTypes,
    pub tfunc_lists: &'a mut TFuncLists,
    pub instances: &'a mut Instances,
    pub bound_impls: &'a mut BoundImpls,
    pub modules: &'a mut Modules,
    pub sources: &'a Sources,
    pub ast: &'a AstData,
    pub diagnostics: &'a mut errors::Diagnostics,
    pub ctx: &'a mut ScopeContext,
    pub module: Source,
    pub to_compile: &'a mut ToCompile,
    pub incr: &'a mut Incr,
}

impl<'a> ScopeBuilder<'a> {
    pub fn collect_items<'f>(
        &mut self,
        elements: impl Iterator<Item = (Ast, &'f ast::AstEnt)> + Clone,
    ) -> errors::Result {
        for (ast, &ast::AstEnt { kind, span, .. }) in elements.clone() {
            if kind == AstKind::Tag {
                self.ctx.tags.push(ast);
                continue;
            }

            match kind {
                AstKind::Function(..) | AstKind::Impl => (),
                AstKind::Struct => drop(self.collect_struct(ast)),
                AstKind::Bound => drop(self.collect_bound(ast)), // for now
                _ => (todo!("Unhandled top-level item:\n{}", self.sources.display(span))),
            }

            self.ctx.tags.clear();
        }

        for (ast, &ast::AstEnt { kind, span, .. }) in elements {
            if kind == AstKind::Tag {
                self.ctx.tags.push(ast);
                continue;
            }

            match kind {
                AstKind::Function(..) => drop(self.collect_function(None, None, ast)),
                AstKind::Impl => drop(self.collect_impl(ast)),
                AstKind::Struct | AstKind::Bound => (),
                _ => todo!("Unhandled top-level item:\n{}", self.sources.display(span)),
            }

            self.ctx.tags.clear();
        }

        // due to the unique nested structure of bounds, (being types and also functions),
        // we have to defer the scope insertion after all types have been inserted.
        {
            self.scope.mark_frame();

            let mut prev = None;
            while let Some(func) = self.ctx.bound_funcs.pop() {
                let ast = self.ctx.func_ast[func];
                let TFuncKind::Bound(bound, ..) = self.funcs[func].kind else {
                    unreachable!();
                };

                if Some(bound) != prev {
                    if prev.is_some() {
                        self.scope.pop_item();
                    }
                    let span = self.types[bound].name;
                    self.scope.push_item("Self", scope::ScopeItem::new(bound, span));
                    prev = Some(bound);
                }

                drop(self.collect_function(Some(func), None, ast));
            }

            self.scope.pop_frame();
        }

        Ok(())
    }

    fn collect_impl(&mut self, ast: Ast) -> errors::Result {
        let &[ty, dest, body] = self.ast.children(ast) else {
			unreachable!();
		};

        let span = self.ast.nodes[ty].span;
        let ty = ty_parser!(self).parse_type(ty)?;

        if !dest.is_reserved_value() {
            let dest = ty_parser!(self).parse_type(dest)?;
            let id = {
                let dest_id = self.types[dest].id;
                let bound_id = self.types[ty].id;
                ID::bound_impl(bound_id, dest_id)
            };

            if let Some(collision) = self.bound_impls.insert(id, BoundImpl::new(span)) {
                self.diagnostics.push(TyError::DuplicateBoundImpl {
                    loc: self.ast.nodes[ast].span,
                    because: collision.span,
                });
                return Err(());
            }

            if !body.is_reserved_value() {
                self.scope.mark_frame();
                self.scope.push_item("Self", scope::ScopeItem::new(dest, span));

                // TODO: we can avoid inserting funcs into the scope all together
                for &func in self.ast.children(body) {
                    if !matches!(self.ast.nodes[func].kind, AstKind::Function(..)) {
                        continue;
                    }

                    let reserved = {
                        let ent = TFuncEnt {
                            kind: TFuncKind::Owned(dest),
                            ..Default::default()
                        };
                        self.funcs.push(ent)
                    };
                    let id = self.types[ty].id;
                    self.collect_function(Some(reserved), Some(id), func)?;
                }

                self.scope.pop_frame();
            }

            self.ctx.bounds_to_verify.push((dest, ty, ast));
        } else if !body.is_reserved_value() {
            self.scope.mark_frame();
            self.scope.push_item("Self", scope::ScopeItem::new(ty, span));

            for &func in self.ast.children(body) {
                let reserved = {
                    let ent = TFuncEnt {
                        kind: TFuncKind::Owned(ty),
                        ..Default::default()
                    };
                    self.funcs.push(ent)
                };
                self.collect_function(Some(reserved), None, func)?;
            }

            self.scope.pop_frame();
        }

        Ok(())
    }

    fn collect_bound(&mut self, ast: Ast) -> errors::Result {
        let source = self.ast.nodes[ast].span.source();
        let &[name, body] = self.ast.children(ast) else {
            unreachable!();
        };

        let name = self.ast.nodes[name].span;
        let scope_id = self.sources.id(name);
        let id = self.modules[self.module].id + scope_id;

        let slot = self.types.push(TyEnt {
            id,
            name,
            kind: TyKind::Unresolved,
            flags: TyFlags::GENERIC,
        });

        let funcs = {
            for (i, &func) in self.ast.children(body).iter().enumerate() {
                let func_id = self.funcs.push(TFuncEnt {
                    kind: TFuncKind::Bound(slot, i as u32),
                    ..Default::default()
                });
                self.ctx.func_ast[func_id] = func;
                self.ctx.bound_funcs.push(func_id);
                self.tfunc_lists.push_one(func_id);
            }
            self.tfunc_lists.close_frame()
        };

        // bound implements it self
        {
            let id = ID::bound_impl(id, id);
            let bound = BoundImpl { span: name, funcs };
            self.bound_impls.insert_unique(id, bound);
        }

        self.types[slot].kind = TyKind::Bound(funcs);
        self.ctx.type_ast[slot] = ast;

        {
            let item = module::ModuleItem::new(scope_id, slot, name);
            drop(
                self.scope
                    .insert(self.diagnostics, source, scope_id, item.to_scope_item()),
            );
            self.modules[self.module].items.push(item);
        }

        Ok(())
    }

    fn collect_struct(&mut self, ast: Ast) -> errors::Result {
        let source = self.ast.nodes[ast].span.source();
        let &[generics, name, ..] = self.ast.children(ast) else {
            unreachable!();
        };

        // dbg!(generics, self.sources.display(self.ast.nodes[name].span));

        let span = self.ast.nodes[name].span;
        let scope_id = self.sources.id(span);
        let id = self.modules[self.module].id + scope_id;
        let flags = if generics.is_reserved_value() {
            TyFlags::empty()
        } else {
            TyFlags::GENERIC.add_param_count(self.ast.children(generics).len())
        };

        let ent = TyEnt {
            id,
            kind: TyKind::Unresolved,
            name: span,
            flags,
        };
        let ty = self.types.push(ent);
        self.ctx.type_ast[ty] = ast;

        {
            let item = module::ModuleItem::new(scope_id, ty, span);
            drop(
                self.scope
                    .insert(self.diagnostics, source, scope_id, item.to_scope_item()),
            );
            self.modules[self.module].items.push(item);
        }

        Ok(())
    }

    fn collect_function(
        &mut self,
        prepared: Option<Func>,
        bound: Option<ID>,
        ast: Ast,
    ) -> errors::Result<Func> {
        let children = self.ast.children(ast);
        let ast::AstEnt { span: current_span, kind: AstKind::Function(external), .. } = self.ast.nodes[ast] else {
            unreachable!();
        };
        let &[generics, call_conv, name, .., return_type, _body] = children else {
            unreachable!();
        };

        let sig = {
            self.scope.mark_frame();

            let params = self.handle_generics(generics, prepared);

            let args = {
                self.ty_lists.mark_frame();

                for &ast in
                    &children[ast::FUNCTION_ARG_START..children.len() - ast::FUNCTION_ARG_END]
                {
                    let children = self.ast.children(ast);
                    let amount = children.len() - 1;
                    let ty = children[amount];
                    let Ok(ty) = ty_parser!(self).parse_type(ty) else {
                        continue;
                    };

                    for _ in 0..amount {
                        self.ty_lists.push_one(ty);
                    }
                }

                self.ty_lists.pop_frame()
            };

            let ret = if return_type.is_reserved_value() {
                self.builtin_types.nothing
            } else {
                ty_parser!(self).parse_type(return_type)?
            };

            self.scope.pop_frame();

            let call_conv = if call_conv.is_reserved_value() {
                Span::default()
            } else {
                self.ast.nodes[call_conv].span
            };

            // println!("{:?}", self.ty_lists.get(params));

            Sig {
                call_conv,
                params,
                args,
                ret,
            }
        };

        let func = prepared.unwrap_or_else(|| self.funcs.push(Default::default()));

        let scope_id = {
            let span = self.ast.nodes[name].span;
            let str = self.sources.display(span);
            let id = ID::new(str);
            if let TFuncKind::Owned(owner) | TFuncKind::Bound(owner, ..) = self.funcs[func].kind {
                if let Some(bound) = bound {
                    let implementor = self.types[owner].id;
                    ID::bound_impl_owned_func(bound, implementor, id)
                } else {
                    let owner = self.types[owner].id;
                    ID::owned_func(owner, id)
                }
            } else {
                id
            }
        };
        
        let id = self.modules[self.module].id + scope_id;

        if sig.params.is_reserved_value() {
            let mod_id = self.modules[self.module].id;
            self.incr.modules.get_mut(mod_id).unwrap().owned_functions.insert(id, ());
            self.to_compile.push(FuncRef::Func(func));
        }

        {
            let is_entry = self.find_simple_tag("entry");
            let is_inline = self.find_simple_tag("inline");
            let flags = {
                (TFuncFlags::EXTERNAL & external)
                    | (TFuncFlags::INLINE & is_inline.is_some())
                    | (TFuncFlags::ENTRY & is_entry.is_some())
                    | (TFuncFlags::GENERIC & !sig.params.is_reserved_value())
            };

            if flags.contains(TFuncFlags::ENTRY | TFuncFlags::GENERIC) {
                self.diagnostics.push(TyError::GenericEntry {
                    tag: is_entry.unwrap(),
                    generics: self.ast.nodes[generics].span,
                    loc: self.ast.nodes[name].span,
                })
            }

            let ent = TFuncEnt {
                sig,
                id,
                name: self.ast.nodes[name].span,
                flags,
                ..self.funcs[func]
            };
            self.funcs[func] = ent;
            self.ctx.func_ast[func] = ast;
        }

        {
            let module_item = module::ModuleItem::new(scope_id, func, current_span);
            drop(self.scope.insert(
                self.diagnostics,
                current_span.source(),
                scope_id,
                module_item.to_scope_item(),
            ));
            self.modules[self.module].items.push(module_item);
        }

        Ok(func)
    }

    pub fn handle_generics(&mut self, generics: Ast, prepared: Option<Func>) -> TyList {
        
        let mut used = EntitySet::new();
        let mut used_list = vec![];

        self.ty_lists.mark_frame();

        if !generics.is_reserved_value() {    
            for &ast in self.ast.children(generics).iter() {
                let children = self.ast.children(ast);
                let name = children[0];
                let span = self.ast.nodes[name].span;
                
                let bound = ty_parser!(self).parse_composite_bound(&children[1..], span);
                let bound = if !used.insert(bound) {
                    // we allocate copy of the bound, this is important
                    // because we will need same bounds with distinct state 
                    // in next stages
                    let next_bound = get_param(bound, self.types);
                    let id = self.types[bound].id;
                    assert!(self.instances.insert(id, bound) == Some(bound));
                    next_bound
                } else {
                    used_list.push(bound);
                    bound
                };
                
                let str = self.sources.display(span);
                self.scope.push_item(str, scope::ScopeItem::new(bound, span));
                self.ty_lists.push_one(bound);
            }
        }   

        if let Some(func) = prepared
            && let TFuncKind::Bound(owner, ..) = self.funcs[func].kind
        {
            self.ty_lists.mark_frame();
            self.ty_lists.push_one(owner);
            let span = self.types[owner].name;
            
            let bound = ty_parser!(self).parse_composite_bound_low(span);
            let bound = if used.contains(bound) {
                get_param(bound, self.types)
            } else {
                bound
            };
            
            self.scope.push_item("Self", scope::ScopeItem::new(bound, span));
            self.ty_lists.push_one(bound);
        }

        for bound in used_list {
            let id = self.types[bound].id;
            assert!(self.instances.insert(id, bound).is_some());
        }

        self.ty_lists.pop_frame()       
    }

    pub fn find_simple_tag(&self, name: &str) -> Option<Span> {
        self.ctx
            .tags
            .iter()
            .rev()
            .map(|&tag| self.ast.nodes[tag].span)
            .find(|&span| self.sources.display(span)[1..].trim() == name)
    }
}

pub struct ScopeContext {
    pub bound_funcs: Vec<Func>,
    pub tags: Vec<Ast>,
    /// (implementor, bound, impl block)
    pub bounds_to_verify: Vec<(Ty, Ty, Ast)>,
    pub type_ast: SecondaryMap<Ty, Ast>,
    pub func_ast: SecondaryMap<Func, Ast>,
    pub used_types: Vec<Ty>,
    pub used_types_set: EntitySet<Ty>,
}

impl ScopeContext {
    pub fn new() -> Self {
        Self {
            bound_funcs: Vec::new(),
            tags: Vec::new(),
            bounds_to_verify: Vec::new(),
            type_ast: SecondaryMap::new(),
            func_ast: SecondaryMap::new(),
            used_types: Vec::new(),
            used_types_set: EntitySet::new(),
        }
    }

    pub fn use_type(&mut self, ty: Ty, types: &Types) {
        if self.used_types_set.insert(ty) && types[ty].flags.contains(TyFlags::GENERIC) {
            self.used_types.push(ty);
        }
    }
}

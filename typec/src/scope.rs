use std::{str::FromStr, vec};

use crate::{ty::get_param, *};
use ast::*;
use cranelift_codegen::isa::CallConv;
use lexer::*;
use module_types::*;
use storage::*;
use typec_types::{
    jit::{Macro, Stage},
    *,
};

impl<'a> ScopeBuilder<'a> {
    pub fn collect_items<'f>(
        &mut self,
        elements: impl Iterator<Item = (Ast, &'f ast::AstEnt)> + Clone,
    ) {
        for (ast, &ast::AstEnt { kind, span, .. }) in elements.clone() {
            if kind == AstKind::Tag {
                self.scope_context.tags.push(ast);
                continue;
            }

            match kind {
                AstKind::Function(..) | AstKind::Impl | AstKind::Variable(..) => (),
                AstKind::Struct => drop(self.collect_struct(ast)),
                AstKind::Bound => drop(self.collect_bound(ast)),
                AstKind::Enum => drop(self.collect_struct(ast)), // there is no difference at this level
                _ => (unimplemented!("Unhandled top-level item:\n{}", self.sources.display(span))),
            }

            self.scope_context.tags.clear();
        }

        for (ast, &ast::AstEnt { kind, span, .. }) in elements {
            if kind == AstKind::Tag {
                self.scope_context.tags.push(ast);
                continue;
            }

            match kind {
                AstKind::Variable(..) => drop(self.collect_global(ast)),
                AstKind::Function(..) => drop(self.collect_function(None, None, ast)),
                AstKind::Impl => drop(self.collect_impl(ast)),
                AstKind::Struct | AstKind::Bound | AstKind::Enum => (),
                _ => todo!("Unhandled top-level item:\n{}", self.sources.display(span)),
            }

            self.scope_context.tags.clear();
        }

        // due to the unique nested structure of bounds, (being types and also functions),
        // we have to defer the scope insertion after all types have been inserted.
        {
            self.scope.mark_frame();

            let mut prev = None;
            while let Some(func) = self.scope_context.bound_funcs.pop() {
                let ast = self.scope_context.func_ast[func];
                let FuncKind::Bound(bound, ..) = self.func_meta[func].kind else {
                    unreachable!();
                };

                if Some(bound) != prev {
                    if prev.is_some() {
                        self.scope.pop_item();
                    }
                    let span = self.types[bound].name;
                    self.scope
                        .push_item("Self", ScopeItem::new(bound, span));
                    prev = Some(bound);
                }

                drop(self.collect_function(Some(func), None, ast));
            }


            self.scope.pop_frame();
        }
    }

    fn collect_global(&mut self, ast: Ast) {
        let AstEnt { kind, .. } = self.ast_data.nodes[ast];
        let mutable = kind == AstKind::Variable(true);
        
        let &[name, value] = self.ast_data.children(ast) else {
            unreachable!();
        };
        let span = self.ast_data.nodes[name].span;
        let scope_id = self.sources.id_of(span);
        let id = self.modules[self.source].id + scope_id;

        let global_ent = GlobalEnt {
            id,
            name: span, 
            mutable,
            
            ..Default::default()
        };
        let global = self.globals.push(global_ent);

        self.scope_context.global_ast[global] = value;
        
        let item = ModuleItem::new(scope_id, global, span);
        drop(self.scope.insert(self.diagnostics, self.source, scope_id, item.to_scope_item()));
        self.modules[self.source].items.push(item);
    }

    fn collect_impl(&mut self, ast: Ast) -> errors::Result {
        let &[ty, dest, body] = self.ast_data.children(ast) else {
			unreachable!();
		};

        let span = self.ast_data.nodes[ty].span;
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
                    loc: self.ast_data.nodes[ast].span,
                    because: collision.span,
                });
                return Err(());
            }

            if !body.is_reserved_value() {
                self.scope.mark_frame();
                self.scope
                    .push_item("Self", ScopeItem::new(dest, span));

                // TODO: we can avoid inserting funcs into the scope all together
                for &func in self.ast_data.children(body) {
                    if !matches!(self.ast_data.nodes[func].kind, AstKind::Function(..)) {
                        continue;
                    }

                    let reserved = self.funcs.push(FuncEnt::default());
                    self.func_meta[reserved] = FuncMetaData {
                        kind: FuncKind::Owned(dest),
                        ..Default::default()
                    };
                    let id = self.types[ty].id;
                    self.collect_function(Some(reserved), Some(id), func)?;
                }

                self.scope.pop_frame();
            }

            self.scope_context.bounds_to_verify.push((dest, ty, ast));
        } else if !body.is_reserved_value() {
            self.scope.mark_frame();
            self.scope
                .push_item("Self", ScopeItem::new(ty, span));

            for &func in self.ast_data.children(body) {
                let reserved = self.funcs.push(FuncEnt::default());
                self.func_meta[reserved] = FuncMetaData {
                    kind: FuncKind::Owned(ty),
                    ..Default::default()
                };
                self.collect_function(Some(reserved), None, func)?;
            }

            self.scope.pop_frame();
        }

        Ok(())
    }

    fn collect_bound(&mut self, ast: Ast) -> errors::Result {
        let source = self.ast_data.nodes[ast].span.source();
        let &[name, body] = self.ast_data.children(ast) else {
            unreachable!();
        };

        let name = self.ast_data.nodes[name].span;
        let scope_id = self.sources.id_of(name);
        let id = self.modules[self.source].id + scope_id;

        let slot = self.types.push(TyEnt {
            id,
            name,
            kind: TyKind::Unresolved,
            flags: TyFlags::GENERIC,
        });

        let funcs = {
            for (i, &func) in self.ast_data.children(body).iter().enumerate() {
                let reserved = self.funcs.push(FuncEnt::default());
                self.func_meta[reserved] = FuncMetaData {
                    kind: FuncKind::Bound(slot, i as u32),
                    ..Default::default()
                };
                self.scope_context.func_ast[reserved] = func;
                self.scope_context.bound_funcs.push(reserved);
                self.func_lists.push_one(reserved);
            }
            self.func_lists.close_frame()
        };

        // bound implements it self
        {
            let id = ID::bound_impl(id, id);
            let bound = BoundImpl { span: name, funcs };
            self.bound_impls.insert_unique(id, bound);
        }

        self.types[slot].kind = TyKind::Bound(funcs);
        self.scope_context.type_ast[slot] = ast;

        {
            let item = module::ModuleItem::new(scope_id, slot, name);
            drop(
                self.scope
                    .insert(self.diagnostics, source, scope_id, item.to_scope_item()),
            );
            self.modules[self.source].items.push(item);
        }

        Ok(())
    }

    fn collect_struct(&mut self, ast: Ast) -> errors::Result {
        let source = self.ast_data.nodes[ast].span.source();
        let &[generics, name, _body] = self.ast_data.children(ast) else {
            unreachable!();
        };

        let span = self.ast_data.nodes[name].span;
        let scope_id = self.sources.id_of(span);
        let id = self.modules[self.source].id + scope_id;
        let flags = if generics.is_reserved_value() {
            TyFlags::empty()
        } else {
            TyFlags::GENERIC.add_param_count(self.ast_data.children(generics).len())
        };

        let ent = TyEnt {
            id,
            kind: TyKind::Unresolved,
            name: span,
            flags,
        };
        let ty = self.types.push(ent);
        self.scope_context.type_ast[ty] = ast;

        {
            let item = module::ModuleItem::new(scope_id, ty, span);
            drop(
                self.scope
                    .insert(self.diagnostics, source, scope_id, item.to_scope_item()),
            );
            self.modules[self.source].items.push(item);
        }

        Ok(())
    }

    fn collect_function(
        &mut self,
        prepared: Option<Func>,
        bound: Option<ID>,
        ast: Ast,
    ) -> errors::Result<Func> {
        let children = self.ast_data.children(ast);
        let ast::AstEnt { span: current_span, kind: AstKind::Function(external), .. } = self.ast_data.nodes[ast] else {
            unreachable!();
        };

        let &[generics, call_conv, name, .., return_type, _body] = children else {
            unreachable!();
        };

        let call_conv = if call_conv.is_reserved_value() {
            Some(CallConv::Fast)
        } else {
            let span = self.ast_data.nodes[call_conv].span.strip_sides();
            let str = self.sources.display(span);
            if str == "default" {
                None
            } else {
                CallConv::from_str(str)
                    .map_err(|_| {
                        self.diagnostics
                            .push(TyError::InvalidCallConv { loc: span })
                    })
                    .ok()
            }
        };

        let sig = {
            self.scope.mark_frame();

            let params = self.handle_generics(generics, prepared);

            let args = {
                self.ty_lists.mark_frame();

                for &ast in
                    &children[ast::FUNCTION_ARG_START..children.len() - ast::FUNCTION_ARG_END]
                {
                    let children = self.ast_data.children(ast);
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

            Sig { params, args, ret }
        };

        let func = prepared.unwrap_or_else(|| self.funcs.push(Default::default()));

        let scope_id = {
            let span = self.ast_data.nodes[name].span;
            let str = self.sources.display(span);
            let id = ID::new(str);
            if let FuncKind::Owned(owner) | FuncKind::Bound(owner, ..) = self.func_meta[func].kind {
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

        let id = if external {
            scope_id
        } else {
            self.modules[self.source].id + scope_id
        };

        assert!(self.func_instances.insert(id, func).is_none());

        if sig.params.is_reserved_value() && !external {
            self.to_compile.push((func, TyList::reserved_value()));
        } else if external {
            self.to_link.push(func);
        }

        if let Some(macro_tag) = self.parse_macro_tag() {
            self.macros.push((func, macro_tag));
        };

        {
            let is_entry = self.find_simple_tag("entry");
            let is_inline = self.find_simple_tag("inline");

            let flags = {
                (FuncFlags::EXTERNAL & external)
                    | (FuncFlags::INLINE & is_inline.is_some())
                    | (FuncFlags::ENTRY & is_entry.is_some())
                    | (FuncFlags::GENERIC & !sig.params.is_reserved_value())
                    | call_conv
            };

            if is_entry.is_some() {
                self.initializers.push((func, None.into()));
            }

            if flags.contains(FuncFlags::ENTRY | FuncFlags::GENERIC) {
                self.diagnostics.push(TyError::GenericEntry {
                    tag: self.ast_data.nodes[is_entry.unwrap()].span,
                    generics: self.ast_data.nodes[generics].span,
                    loc: self.ast_data.nodes[name].span,
                })
            }

            let ent = FuncEnt {
                id,
                flags,
                parent: None.into(),
            };
            self.funcs[func] = ent;

            let meta = FuncMetaData {
                sig,
                name: self.ast_data.nodes[name].span,
                ..self.func_meta[func]
            };
            self.func_meta[func] = meta;

            self.scope_context.func_ast[func] = ast;
        }

        {
            let module_item = module::ModuleItem::new(scope_id, func, current_span);
            drop(self.scope.insert(
                self.diagnostics,
                current_span.source(),
                scope_id,
                module_item.to_scope_item(),
            ));
            self.modules[self.source].items.push(module_item);
        }

        Ok(func)
    }

    pub fn handle_generics(&mut self, generics: Ast, prepared: Option<Func>) -> TyList {
        let mut used = EntitySet::new();
        let mut used_list = vec![];

        self.ty_lists.mark_frame();

        if !generics.is_reserved_value() {
            for &ast in self.ast_data.children(generics).iter() {
                let children = self.ast_data.children(ast);
                let name = children[0];
                let span = self.ast_data.nodes[name].span;

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
                self.scope
                    .push_item(str, ScopeItem::new(bound, span));
                self.ty_lists.push_one(bound);
            }
        }

        if let Some(func) = prepared
            && let FuncKind::Bound(owner, ..) = self.func_meta[func].kind
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

            self.scope.push_item("Self", ScopeItem::new(bound, span));
            self.ty_lists.push_one(bound);
        }

        for bound in used_list {
            let id = self.types[bound].id;
            assert!(self.instances.insert(id, bound).is_some());
        }

        self.ty_lists.pop_frame()
    }

    pub fn parse_macro_tag(&self) -> Option<Macro> {
        let raw_tag = self.find_simple_tag("macro")?;
        let raw_tag = self.ast_data.children(raw_tag)[0]; 

        // TODO: emit error
        assert_eq!(self.ast_data.nodes[raw_tag].kind, AstKind::Call);

        let &[_, from, to] = self.ast_data.children(raw_tag) else {
            // TODO: emit error
            unreachable!();
        };

        let from = {
            let span = self.ast_data.nodes[from].span;
            let str = self.sources.display(span);
            Stage::from_str(str).map_err(|_| todo!()).ok()?
        };

        let to = {
            let span = self.ast_data.nodes[to].span;
            let str = self.sources.display(span);
            Stage::from_str(str).map_err(|_| todo!()).ok()?
        };

        // TODO: emit error
        assert!(from.can_bridge_to(to));

        Some(Macro { from, to })
    }

    pub fn find_simple_tag(&self, name: &str) -> Option<Ast> {
        self.scope_context
            .tags
            .iter()
            .rev()
            .find(|&&tag| {
                self.sources.display(self.ast_data.nodes[tag].span)[1..]
                    .trim()
                    .starts_with(name)
            })
            .copied()
    }
}

pub struct ScopeContext {
    pub bound_funcs: Vec<Func>,
    pub tags: Vec<Ast>,
    /// (implementor, bound, impl block)
    pub bounds_to_verify: Vec<(Ty, Ty, Ast)>,
    pub type_ast: SecondaryMap<Ty, Ast>,
    pub func_ast: SecondaryMap<Func, Ast>,
    pub global_ast: SecondaryMap<Global, Ast>,
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
            global_ast: SecondaryMap::new(),
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
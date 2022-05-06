
use module_types::{scope::{Scope, self}, modules::{Modules, self}};
use lexer_types::*;
use storage::*;
use typec_types::*;
use crate::*;
use ast::*;

pub struct Collector<'a> {
    pub scope: &'a mut Scope,
    pub funcs: &'a mut Funcs,
    pub types: &'a mut Types,
    pub modules: &'a mut Modules,
    pub sources: &'a Sources,
    pub ast: &'a ast::Data,
    pub diagnostics: &'a mut errors::Diagnostics,
    pub ctx: &'a mut Context,
    pub module: Source,
}

impl<'a> Collector<'a> {
    pub fn collect_items<'f>(
        &mut self,
        elements: impl Iterator<Item = (Ast, &'f ast::Ent)> + Clone,
    ) -> errors::Result {
        for (ast, &ast::Ent { kind, span, .. }) in elements.clone() {
            if kind == ast::Kind::Tag {
                self.ctx.tags.push(ast);
                continue;
            }

            match kind {
                ast::Kind::Function(..) | ast::Kind::Impl => (),
                ast::Kind::Struct => drop(self.collect_struct(ast)),
                ast::Kind::Bound => drop(self.collect_bound(ast)), // for now
                _ => (todo!("Unhandled top-level item:\n{}", self.sources.display(span))),
            }

            self.ctx.tags.clear();
        }

        for (ast, &ast::Ent { kind, span, .. }) in elements {
            if kind == ast::Kind::Tag {
                self.ctx.tags.push(ast);
                continue;
            }

            match kind {
                ast::Kind::Function(..) => drop(self.collect_function(None, None, ast)),
                ast::Kind::Impl => drop(self.collect_impl(ast)),
                ast::Kind::Struct | ast::Kind::Bound => (),
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
                let func::Kind::Bound(bound, ..) = self.funcs[func].kind else {
                    unreachable!();
                };

                if Some(bound) != prev {
                    if prev.is_some() {
                        self.scope.pop_item();
                    }
                    let span = self.types.ents[bound].name;
                    self.scope.push_item("Self", scope::Item::new(bound, span));
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
        let ty = parse_type!(self, ty)?;

        if !dest.is_reserved_value() {
            let dest = parse_type!(self, dest)?;
            let id = {
                let dest_id = self.types.ents[dest].id;
                let bound_id = self.types.ents[ty].id;
                ID::bound_impl(bound_id, dest_id)
            };

            if let Some(collision) = self.types.bound_cons.insert(id, BoundImpl::new(span)) {
                self.diagnostics.push(Error::DuplicateBoundImpl {
                    loc: self.ast.nodes[ast].span,
                    because: collision.span,
                });
                return Err(());
            }

            if !body.is_reserved_value() {
                self.scope.mark_frame();
                self.scope.push_item("Self", scope::Item::new(dest, span));

                // TODO: we can avoid inserting funcs into the scope all together
                for &func in self.ast.children(body) {
                    if let ast::Kind::UseBoundFunc = self.ast.nodes[func].kind {
                        continue;
                    }

                    let reserved = {
                        let ent = func::Ent {
                            kind: func::Kind::Owned(dest),
                            ..Default::default()
                        };
                        self.funcs.push(ent)
                    };
                    let id = self.types.ents[ty].id;
                    self.collect_function(Some(reserved), Some(id), func)?;
                }

                self.scope.pop_frame();
            }

            self.ctx.bounds_to_verify.push((dest, ty, ast));
        } else if !body.is_reserved_value() {
            self.scope.mark_frame();
            self.scope.push_item("Self", scope::Item::new(ty, span));

            for &func in self.ast.children(body) {
                let reserved = {
                    let ent = func::Ent {
                        kind: func::Kind::Owned(ty),
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

        let slot = self.types.ents.push(ty::Ent {
            id,
            name,
            kind: ty::Kind::Unresolved,
            flags: ty::Flags::GENERIC,
        });

        let funcs = {
            for (i, &func) in self.ast.children(body).iter().enumerate() {
                let func_id = self.funcs.push(func::Ent {
                    kind: func::Kind::Bound(slot, i as u32),
                    ..Default::default()
                });
                self.ctx.func_ast[func_id] = func;
                self.ctx.bound_funcs.push(func_id);
                self.types.funcs.push_one(func_id);
            }
            self.types.funcs.close_frame()
        };

        // bound implements it self
        {
            let id = ID::bound_impl(id, id);
            assert!(self
                .types
                .bound_cons
                .insert(id, BoundImpl { span: name, funcs })
                .is_none());
        }

        self.types.ents[slot].kind = ty::Kind::Bound(funcs);
        self.ctx.type_ast[slot] = ast;

        {
            let item = modules::Item::new(scope_id, slot, name);
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
        let ent = ty::Ent {
            id,
            kind: ty::Kind::Unresolved,
            name: span,
            flags: ty::Flags::GENERIC & !generics.is_reserved_value(),
        };
        let ty = self.types.ents.push(ent);
        self.ctx.type_ast[ty] = ast;

        {
            let item = modules::Item::new(scope_id, ty, span);
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
        let ast::Ent { span: current_span, kind: ast::Kind::Function(external), .. } = self.ast.nodes[ast] else {
            unreachable!();
        };
        let &[generics, call_conv, name, .., return_type, _body] = children else {
            unreachable!();
        };

        let sig = {
            self.scope.mark_frame();

            let params = {
                // push the generic parameters
                if !generics.is_reserved_value() {
                    let ast = self.ast.children(generics);
                    for (i, &ast) in ast.iter().enumerate() {
                        let children = self.ast.children(ast);
                        let name = children[0];
                        let span = self.ast.nodes[name].span;
                        let Ok(bound) = parse_composite_bound!(self, &children[1..], span) else {
                            continue;
                        };

                        let str = self.sources.display(span);
                        let params = self.types.fn_params();
                        self.scope.push_item(str, scope::Item::new(params[i], span));
                        self.types.args.push_one(bound);
                    }
                }

                // if this is a bound owned function, push the self as well
                if let Some(func) = prepared
                    && let
                        func::Kind::Bound(owner, ..)
                        | func::Kind::Owned(owner) = self.funcs[func].kind
                    && let ty::Kind::Bound(..) = self.types.ents[owner].kind
                {
                    let params = self.types.fn_params();
                    let param = params[self.types.args.top().len()];
                    let span = self.types.ents[owner].name;
                    self.scope.push_item("Self", scope::Item::new(param, span));
                    self.types.args.push_one(owner);
                }

                self.types.args.close_frame()
            };

            let args = {
                for &ast in
                    &children[ast::FUNCTION_ARG_START..children.len() - ast::FUNCTION_ARG_END]
                {
                    let children = self.ast.children(ast);
                    let amount = children.len() - 1;
                    let ty = children[amount];
                    let Ok(ty) = parse_type!(self, ty) else {
                        continue;
                    };

                    for _ in 0..amount {
                        self.types.args.push_one(ty);
                    }
                }

                self.types.args.close_frame()
            };

            let ret = if return_type.is_reserved_value() {
                self.types.builtin.nothing
            } else {
                parse_type!(self, return_type)?
            };

            self.scope.pop_frame();

            let call_conv = if call_conv.is_reserved_value() {
                Span::default()
            } else {
                self.ast.nodes[call_conv].span
            };

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
            if let func::Kind::Owned(owner) | func::Kind::Bound(owner, ..) = self.funcs[func].kind {
                if let Some(bound) = bound {
                    let implementor = self.types.ents[owner].id;
                    ID::bound_impl_owned_func(bound, implementor, id)
                } else {
                    let owner = self.types.ents[owner].id;
                    ID::owned_func(owner, id)
                }
            } else {
                id
            }
        };

        {
            let is_entry = self.find_simple_tag("entry");
            let is_inline = self.find_simple_tag("inline");
            let flags = {
                (func::Flags::EXTERNAL & external)
                    | (func::Flags::INLINE & is_inline.is_some())
                    | (func::Flags::ENTRY & is_entry.is_some())
                    | (func::Flags::GENERIC & !sig.params.is_reserved_value())
            };

            if flags.contains(func::Flags::ENTRY | func::Flags::GENERIC) {
                self.diagnostics.push(Error::GenericEntry {
                    tag: is_entry.unwrap(),
                    generics: self.ast.nodes[generics].span,
                    loc: self.ast.nodes[name].span,
                })
            }

            let ent = func::Ent {
                sig,
                name: self.ast.nodes[name].span,
                flags,
                ..self.funcs[func]
            };
            self.funcs[func] = ent;
            self.ctx.func_ast[func] = ast;
        }

        {
            let module_item = modules::Item::new(scope_id, func, current_span);
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

    pub fn find_simple_tag(&self, name: &str) -> Option<Span> {
        self.ctx
            .tags
            .iter()
            .rev()
            .map(|&tag| self.ast.nodes[tag].span)
            .find(|&span| self.sources.display(span)[1..].trim() == name)
    }
}

pub struct Context {
    pub bound_funcs: Vec<Func>,
    pub tags: Vec<Ast>,
    /// (implementor, bound, impl block)
    pub bounds_to_verify: Vec<(Ty, Ty, Ast)>,
    pub type_ast: SecondaryMap<Ty, Ast>,
    pub func_ast: SecondaryMap<Func, Ast>,
    pub used_types: Vec<Ty>,
    pub used_types_set: EntitySet<Ty>,
}

impl Context {
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
        if !self.used_types_set.contains(ty) && types.ents[ty].flags.contains(ty::Flags::GENERIC) {
            self.used_types.push(ty);
            self.used_types_set.insert(ty);
        }
    }
}

impl AstIDExt for Collector<'_> {
    fn state(&self) -> (&ast::Data, &Sources) {
        (self.ast, self.sources)
    }
}

#![feature(let_else)]
#![feature(explicit_generic_args_with_impl_trait)]
#![feature(let_chains)]
#![feature(bool_to_option)]
#![feature(if_let_guard)]

pub mod collector;
pub mod error;
pub mod func;
pub mod tir;
pub mod ty;

pub use collector::*;
use cranelift_entity::packed_option::ReservedValue;
pub use error::Error;
pub use func::*;
pub use tir::*;
pub use ty::*;

use lexer::*;
use modules::*;
use parser::*;

pub trait TypeParser {
    fn state(
        &mut self,
    ) -> (
        &mut Scope,
        &mut Types,
        &Sources,
        &mut Modules,
        &ast::Data,
        &mut errors::Diagnostics,
    );

    fn instantiate(&mut self, target: Ty, params: &[Ty]) -> errors::Result<Ty> {
        let (_scope, types, _sources, _modules, _data, _diag) = self.state();
        let ty::Ent { kind, generic, .. } = types.ents[target];

        if !generic {
            return Ok(target);
        }
        
        match kind {
            ty::Kind::Param(i, ..) => return Ok(params[i as usize]),
            ty::Kind::Ptr(ty, ..) => {
                let ty = self.instantiate(ty, params)?;
                return Ok(self.pointer_of(ty));
            }
            _ => todo!(),
        }
    }

    fn infer_parameters(
        &mut self,
        reference: Ty,
        parametrized: Ty,
        params: &mut [Ty],
        home_params: TyList,
        foreign_params: TyList,
    ) -> errors::Result {

        
        let (_scope, types, _sources, _modules, _data, _diag) = self.state();
        let mut frontier = vec![(reference, parametrized)];

        while let Some((reference, parametrized)) = frontier.pop() {
            let ty::Ent { kind, generic, .. } = types.ents[parametrized];
            if !generic {
                continue;
            }

            match (kind, types.ents[reference].kind) {
                (ty::Kind::Param(index), _) => {
                    let other = params[index as usize];
                    if !other.is_reserved_value() {
                        if types.base_of_low(parametrized, foreign_params) != types.base_of_low(other, home_params) {
                            todo!()
                        }
                    } else {
                        if !types.compatible(reference, parametrized, home_params, foreign_params) {
                            todo!("{} {}",
                                ty::Display::new(types, _sources, reference), 
                                ty::Display::new(types, _sources, parametrized)
                            );
                        }
    
                        params[index as usize] = reference;
                    }

                }
                (ty::Kind::Ptr(ty, depth), ty::Kind::Ptr(ref_ty, ref_depth))
                    if depth == ref_depth =>
                {
                    frontier.push((ref_ty, ty));
                }
                (a, b) if a == b => {}
                _ => {
                    todo!("{} {}", 
                        ty::Display::new(types, _sources, reference), 
                        ty::Display::new(types, _sources, parametrized),
                    );
                }
            }
        }

        Ok(())
    }

    fn parse_type(&mut self, ty: Ast) -> errors::Result<Ty> {
        let (scope, _types, sources, _modules, ast, diagnostics) = self.state();
        let ast::Ent { kind, span, .. } = ast.nodes[ty];
        match kind {
            ast::Kind::Ident => {
                let str = sources.display(span);
                scope.get(diagnostics, str, span)
            }
            ast::Kind::Pointer => self.parse_ptr_type(ty),
            _ => todo!("Unhandled type expr {:?}: {}", kind, span.log(sources)),
        }
    }

    fn parse_ptr_type(&mut self, ty: Ast) -> errors::Result<Ty> {
        let ast = self.state().4;
        let inner_ty = {
            let inner = ast.children(ty)[0];
            self.parse_type(inner)?
        };

        Ok(self.pointer_of(inner_ty))
    }

    fn pointer_of(&mut self, ty: Ty) -> Ty {
        let (scope, types, _sources, modules, _ast, diagnostics) = self.state();
        let ty::Ent {
            kind,
            id,
            name,
            generic,
            ..
        } = types.ents[ty];
        let id = Self::pointer_id(id);

        if let Some(ptr) = scope.weak_get::<Ty>(id) {
            return ptr;
        }

        let depth = if let ty::Kind::Ptr(.., depth) = kind {
            depth
        } else {
            0
        };

        let ent = ty::Ent {
            id,
            name,
            kind: ty::Kind::Ptr(ty, depth + 1),
            generic,
        };
        let ptr = types.ents.push(ent);
        let item = modules::Item::new(id, ptr, name);

        drop(scope.insert(diagnostics, name.source(), id, item.to_scope_item()));
        
        modules[name.source()].items.push(item);

        ptr
    }

    fn parse_composite_bound(&mut self, asts: &[Ast], span: Span) -> errors::Result<Ty> {
        for &bound in asts {
            let Ok(ty) = self.parse_type(bound) else {
                continue;
            };
            self.state().1.args.push_one(ty);
        }

        let (scope, types, .., diagnostics) = self.state();

        types.args.top_mut().sort_by_key(|ty| ty.0);
        let duplicates = types.args.top().windows(2).any(|w| w[0] == w[1]);

        if duplicates {
            diagnostics.push(Error::DuplicateBound { loc: span });
        }

        let id = ID::new("<bound_combo>")
            + types
                .args
                .top()
                .iter()
                .map(|&ty| types.ents[ty].id)
                .fold(None, |acc, id| acc.map(|acc| acc + id).or(Some(id)))
                .unwrap_or_else(|| types.ents[types.builtin.any].id);

        let item = if let Some(item) = scope.weak_get::<Ty>(id) {
            types.args.discard();
            item
        } else {
            // make bound combo implement all contained bounds
            for &ty in types.args.top() {
                let ty::Kind::Bound(funcs) = types.ents[ty].kind else {
                    unreachable!();
                };
                let bound = types.ents[ty].id;
                let id = Collector::bound_impl_id(bound, id);
                assert!(types
                    .bound_cons
                    .insert(
                        id,
                        BoundImpl {
                            span: Default::default(),
                            funcs
                        }
                    )
                    .is_none());
            }

            let bounds = types.args.close_frame();
            let ent = ty::Ent {
                id,
                name: span,
                kind: ty::Kind::BoundCombo(bounds),
                generic: true,
            };
            types.ents.push(ent)
        };

        Ok(item)
    }

    fn pointer_id(id: ID) -> ID {
        ID::new("*") + id
    }
}

pub fn create_builtin_items(
    types: &mut Types,
    funcs: &mut Funcs,
    sources: &mut Sources,
    builtin_source: &mut BuiltinSource,
    target: &mut Vec<module::Item>,
) {
    let comparison_operators = "== != < > <= >=";
    let math_operators = "+ - * / %";
    let integer_binary_operators = format!("{} {}", comparison_operators, math_operators);
    let math_unary_operators = "-";
    let integer_unary_operators = format!("{}", math_unary_operators);

    for ty in types.builtin.all() {
        let ent = &types.ents[ty];
        target.push(modules::Item::new(ent.id, ty, ent.name));
    }

    for op in integer_binary_operators.split(' ') {
        for ty in types.builtin.integers() {
            let id = {
                let id = types.ents[ty].id;
                func::Builder::binary_id(id, ID::new(op))
            };
            let ret = (comparison_operators.contains(op))
                .then_some(types.builtin.bool)
                .unwrap_or(ty);
            create_func(
                op,
                &[ty, ty],
                ret,
                id,
                types,
                funcs,
                sources,
                builtin_source,
                target,
            );
        }
    }

    for op in integer_unary_operators.split(' ') {
        for ty in types.builtin.integers() {
            let id = {
                let id = types.ents[ty].id;
                func::Builder::unary_id(id, ID::new(op))
            };
            create_func(
                op,
                &[ty],
                ty,
                id,
                types,
                funcs,
                sources,
                builtin_source,
                target,
            );
        }
    }
}

fn create_func(
    name: &str,
    args: &[Ty],
    ret: Ty,
    id: ID,
    types: &mut Types,
    funcs: &mut Funcs,
    sources: &mut Sources,
    builtin_source: &mut BuiltinSource,
    dest: &mut Vec<module::Item>,
) {
    let span = builtin_source.make_span(sources, name);
    let sig = Sig {
        args: types.args.push(args),
        ret,
        ..Default::default()
    };
    let func = {
        let ent = func::Ent {
            sig,
            name: span,
            kind: func::Kind::Builtin,
            ..Default::default()
        };
        funcs.push(ent)
    };
    let item = module::Item::new(id, func, span);
    dest.push(item);
}

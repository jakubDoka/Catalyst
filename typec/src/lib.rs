#![feature(let_else)]
#![feature(explicit_generic_args_with_impl_trait)]
#![feature(let_chains)]
#![feature(bool_to_option)]
#![feature(if_let_guard)]

pub mod error;
pub mod scope;
pub mod tir;
pub mod ty;

pub use scope::{ScopeBuilder, ScopeContext};
pub use tir::TirBuilder;
pub use ty::TyBuilder;

use ast::*;
use errors::*;
use lexer_types::*;
use module_types::{scope::Scope, *};
use storage::*;
use typec_types::{TyError, *};

#[macro_export]
macro_rules! ident_hasher {
    ($self:expr) => {
        IdentHasher::new(
            $self.sources,
            $self.ast,
            $self.scope,
            $self.diagnostics,
            $self.types,
        )
    };
}

pub fn infer_parameters(
    reference: Ty,
    parametrized: Ty,
    params: &mut [Ty],
    span: Span,
    types: &Types,
    ty_lists: &TyLists,
    bound_impls: &mut BoundImpls,
    diagnostics: &mut Diagnostics,
) -> errors::Result {
    // TODO: user preallocated vec if needed
    let mut frontier = vec![(reference, parametrized)];

    while let Some((reference, parametrized)) = frontier.pop() {
        let TyEnt { kind, flags, .. } = types[parametrized];
        if !flags.contains(TyFlags::GENERIC) {
            continue;
        }

        match (kind, types[reference].kind) {
            (TyKind::Param(index, ..), _) => {
                // we use `base_of_low` because we want to compare parameter
                // backing bounds correctly
                let other = params[index as usize];

                if !other.is_reserved_value() && other != reference {
                    diagnostics.push(TyError::GenericTypeMismatch {
                        expected: other,
                        found: reference,
                        loc: span,
                    });
                } else {
                    let base_ref = types.base_of(parametrized);
                    drop(implements(
                        base_ref,
                        parametrized,
                        span,
                        types,
                        ty_lists,
                        bound_impls,
                        diagnostics,
                    ));
                    params[index as usize] = reference;
                }
            }
            (TyKind::Ptr(ty, depth), TyKind::Ptr(ref_ty, ref_depth)) if depth == ref_depth => {
                frontier.push((ref_ty, ty));
            }
            (TyKind::Instance(base, params), TyKind::Instance(ref_base, ref_params))
                if base == ref_base =>
            {
                let params = ty_lists.get(params);
                let ref_params = ty_lists.get(ref_params);
                for (&ref_param, &param) in ref_params.iter().zip(params) {
                    frontier.push((ref_param, param));
                }
            }
            (a, b) if a == b => {}
            _ => diagnostics.push(TyError::GenericTypeMismatch {
                found: reference,
                expected: parametrized,
                loc: span,
            }),
        }
    }

    Ok(())
}

pub struct TypeParser<'a> {
    types: &'a mut Types,
    ast: &'a AstData,
    diagnostics: &'a mut Diagnostics,
    sfields: &'a SFields,
    scope: &'a mut Scope,
    ty_lists: &'a mut TyLists,
    instances: &'a mut Instances,
    sources: &'a Sources,
    bound_impls: &'a mut BoundImpls,
    builtin_types: &'a BuiltinTypes,
}

#[macro_export]
macro_rules! ty_parser {
    ($self:expr) => {
        TypeParser::new(
            $self.types,
            $self.ast,
            $self.diagnostics,
            $self.sfields,
            $self.scope,
            $self.ty_lists,
            $self.instances,
            $self.sources,
            $self.bound_impls,
            $self.builtin_types,
        )
    };
}

impl<'a> TypeParser<'a> {
    pub fn new(
        types: &'a mut Types,
        ast: &'a AstData,
        diagnostics: &'a mut Diagnostics,
        sfields: &'a SFields,
        scope: &'a mut Scope,
        ty_lists: &'a mut TyLists,
        instances: &'a mut Instances,
        sources: &'a Sources,
        bound_impls: &'a mut BoundImpls,
        builtin_types: &'a BuiltinTypes,
    ) -> Self {
        Self {
            types,
            ast,
            diagnostics,
            sfields,
            scope,
            ty_lists,
            instances,
            sources,
            bound_impls,
            builtin_types,
        }
    }

    pub fn instantiate(&mut self, target: Ty, params: &[Ty]) -> Ty {
        let TyEnt {
            kind, flags, name, ..
        } = self.types[target];

        // println!("instantiate: {}", ty_display!(self, target));
        // for &param in params {
        // println!(" param: {}", ty_display!(self, param));
        // }

        if !flags.contains(TyFlags::GENERIC) {
            return target;
        }

        let result = match kind {
            TyKind::Param(i, ..) => params[i as usize],
            TyKind::Ptr(ty, ..) => {
                let ty = self.instantiate(ty, params);
                pointer_of(ty, self.types, self.instances)
            }
            TyKind::Instance(base, i_params) => {
                self.ty_lists.mark_frame();
                for param in self.ty_lists.get(i_params).to_vec() {
                    // TODO: optimize if needed
                    let ty = self.instantiate(param, params);
                    self.ty_lists.push_one(ty);
                }

                let result = self.parse_instance_type_low(base, name);

                let TyEnt { kind, flags, .. } = self.types[result];
                if !flags.contains(TyFlags::GENERIC) {
                    let TyKind::Instance(base, params) = kind else {
                        unreachable!();
                    };

                    let kind = self.types[base].kind;
                    match kind {
                        TyKind::Struct(sfields) => {
                            let params = self.ty_lists.get(params).to_vec(); // TODO: optimize if needed
                            for field in self.sfields.get(sfields) {
                                self.instantiate(field.ty, &params);
                            }
                        }
                        kind => todo!("{kind:?}"),
                    }
                }

                result
            }
            _ => todo!(),
        };

        result
    }

    pub fn parse_type(&mut self, ty: Ast) -> errors::Result<Ty> {
        let res = self.parse_type_optional(ty)?;
        if res.is_reserved_value() {
            let span = self.ast.nodes[ty].span;
            self.diagnostics
                .push(TyError::ExpectedConcreteType { loc: span });
            return Err(());
        }
        Ok(res)
    }

    /// parse a type just like `parse_type` but can return `Ty::reserved_value` in case the ty is '_'.
    pub fn parse_type_optional(&mut self, ty: Ast) -> errors::Result<Ty> {
        let ast::AstEnt { kind, span, .. } = self.ast.nodes[ty];
        match kind {
            AstKind::Ident => self.parse_ident_type(span),
            AstKind::Instantiation => self.parse_instance_type(ty),
            AstKind::Pointer => self.parse_ptr_type(ty),
            _ => {
                self.diagnostics
                    .push(TyError::InvalidTypeExpression { loc: span });
                return Err(());
            }
        }
    }

    pub fn parse_ident_type(&mut self, span: Span) -> errors::Result<Ty> {
        let str = self.sources.display(span);

        if str == "_" {
            return Ok(Ty::reserved_value());
        }

        self.scope.get(self.diagnostics, str, span)
    }

    fn parse_instance_type(&mut self, ty: Ast) -> errors::Result<Ty> {
        let children = self.ast.children(ty);
        let header = self.parse_type(children[0])?;

        self.ty_lists.mark_frame();

        for &param in &children[1..] {
            let param = self.parse_type(param)?;
            self.ty_lists.push_one(param);
        }

        Ok(self.parse_instance_type_low(header, self.ast.nodes[ty].span))
    }

    /// further specification of [`parse_type`], it expects the `ty` to be of [`ast::Kind::Instantiation`], if instance already exists, it is reused.
    pub fn parse_instance_type_low(&mut self, header: Ty, span: Span) -> Ty {
        let mut id = ID::new("<instance>") + self.types[header].id;
        let mut generic = false;

        for &param in self.ty_lists.top() {
            id = id + self.types[param].id;
            generic |= self.types[param].flags.contains(TyFlags::GENERIC);
        }

        if let Some(&already) = self.instances.get(id) {
            self.ty_lists.discard();
            return already;
        }

        let params = self.ty_lists.pop_frame();

        let result = {
            let ent = TyEnt {
                id,
                name: span,
                kind: TyKind::Instance(header, params),
                flags: TyFlags::GENERIC & generic,
            };
            self.types.push(ent)
        };

        self.instances.insert_unique(id, result);

        result
    }

    pub fn parse_ptr_type(&mut self, ty: Ast) -> errors::Result<Ty> {
        let inner_ty = {
            let inner = self.ast.children(ty)[0];
            self.parse_type(inner)?
        };

        Ok(pointer_of(inner_ty, self.types, self.instances))
    }

    pub fn parse_composite_bound(&mut self, asts: &[Ast], span: Span) -> Ty {
        self.ty_lists.mark_frame();
        for &bound in asts {
            let Ok(ty) = self.parse_type(bound) else {
                continue;
            };
            self.ty_lists.push_one(ty);
        }
        self.parse_composite_bound_low(span)
    }

    pub fn parse_composite_bound_low(&mut self, span: Span) -> Ty {
        self.ty_lists.top_mut().sort_by_key(|ty| ty.0);
        let duplicates = self.ty_lists.top().windows(2).any(|w| w[0] == w[1]);

        if duplicates {
            self.diagnostics.push(TyError::DuplicateBound { loc: span });
        }

        let base_id = self.types[self.builtin_types.any].id;

        let id = self
            .ty_lists
            .top()
            .iter()
            .map(|&ty| self.types[ty].id)
            .fold(base_id, |acc, id| acc + id);

        if let Some(&already) = self.instances.get(id) {
            self.ty_lists.discard();
            return already;
        }

        // make bound combo implement all contained bounds
        for &ty in self.ty_lists.top() {
            let TyKind::Bound(funcs) = self.types[ty].kind else {
                unreachable!();
            };
            let bound = self.types[ty].id;
            let id = ID::bound_impl(bound, id);
            let bound = BoundImpl {
                span: Default::default(),
                funcs,
            };
            self.bound_impls.insert_unique(id, bound);
        }

        let combo = {
            let bounds = self.ty_lists.pop_frame();
            let ent = TyEnt {
                id,
                name: span,
                kind: TyKind::Param(0, bounds, None.into()),
                flags: TyFlags::GENERIC,
            };
            self.types.push(ent)
        };

        self.instances.insert_unique(id, combo);

        combo
    }
}

pub fn prepare_params(params: &[Ty], types: &mut Types) {
    for (i, &param) in params.iter().enumerate() {
        let TyKind::Param(index, ..) = &mut types[param].kind else {
            unreachable!("{:?}", types[param].kind);
        };
        *index = i as u8;
    }
}

/// creates a pointer of `ty`, already instantiated entities will be reused.
pub fn pointer_of(ty: Ty, types: &mut Types, instances: &mut Instances) -> Ty {
    let TyEnt {
        kind,
        id,
        name,
        flags,
        ..
    } = types[ty];
    let id = ID::pointer(id);

    if let Some(&already) = instances.get(id) {
        return already;
    }

    let depth = if let TyKind::Ptr(.., depth) = kind {
        depth
    } else {
        0
    };

    let ent = TyEnt {
        id,
        name,
        kind: TyKind::Ptr(ty, depth + 1),
        flags,
    };
    let ptr = types.push(ent);

    assert!(instances.insert(id, ptr).is_none());

    ptr
}

pub fn implements(
    input: Ty,
    bound: Ty,
    span: Span,
    types: &Types,
    ty_lists: &TyLists,
    bound_impls: &mut BoundImpls,
    diagnostics: &mut Diagnostics,
) -> errors::Result {
    // bound can be pain Bound or BoundCombo
    let TyKind::Param(_, bounds, ..) = types[bound].kind else {
        unreachable!();
    };

    let input_id = types[input].id;

    let mut result = Ok(());

    for &bound in ty_lists.get(bounds) {
        let bound_id = types[bound].id;
        let id = ID::bound_impl(bound_id, input_id);
        if bound_impls.get(id).is_none() {
            diagnostics.push(TyError::MissingBound {
                input,
                bound,
                loc: span,
            });
            result = Err(());
        }
    }

    result
}

/// instantiates all builtin items like types, operators and functions.
pub fn create_builtin_items(
    types: &mut Types,
    ty_lists: &mut TyLists,
    builtin: &BuiltinTypes,
    funcs: &mut Funcs,
    sources: &mut Sources,
    builtin_source: &mut BuiltinSource,
    target: &mut Vec<module::ModuleItem>,
) {
    let comparison_operators = "== != < > <= >=";
    let math_operators = "+ - * / %";
    let integer_binary_operators = format!("{} {}", comparison_operators, math_operators);
    let math_unary_operators = "-";
    let integer_unary_operators = format!("{}", math_unary_operators);

    for ty in builtin.all() {
        let ent = &types[ty];
        target.push(module::ModuleItem::new(ent.id, ty, ent.name));
    }

    for op in integer_binary_operators.split(' ') {
        for ty in builtin.integers() {
            let id = {
                let id = types[ty].id;
                ID::binary(id, ID::new(op))
            };
            let ret = (comparison_operators.contains(op))
                .then_some(builtin.bool)
                .unwrap_or(ty);
            create_func(
                op,
                &[ty, ty],
                ret,
                id,
                ty_lists,
                funcs,
                sources,
                builtin_source,
                target,
            );
        }
    }

    for op in integer_unary_operators.split(' ') {
        for ty in builtin.integers() {
            let id = {
                let id = types[ty].id;
                ID::unary(id, ID::new(op))
            };
            create_func(
                op,
                &[ty],
                ty,
                id,
                ty_lists,
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
    ty_lists: &mut TyLists,
    funcs: &mut Funcs,
    sources: &mut Sources,
    builtin_source: &mut BuiltinSource,
    dest: &mut Vec<module::ModuleItem>,
) {
    let span = builtin_source.make_span(sources, name);
    let sig = Sig {
        args: ty_lists.push(args),
        ret,
        ..Default::default()
    };
    let func = {
        let ent = TFuncEnt {
            sig,
            name: span,
            kind: TFuncKind::Builtin,
            ..Default::default()
        };
        funcs.push(ent)
    };
    let item = module::ModuleItem::new(id, func, span);
    dest.push(item);
}

#![feature(let_else)]
#![feature(explicit_generic_args_with_impl_trait)]
#![feature(let_chains)]
#![feature(bool_to_option)]
#![feature(if_let_guard)]

pub mod collector;
pub mod error;
pub mod func_builder;
pub mod ty_builder;

use errors::*;
use lexer_types::*;
use storage::*;
use module_types::{*, scope::Scope};
use typec_types::{*, Error};
use ast::*;

pub trait TyDump {
    fn add(&mut self, ty: Ty);
}

impl TyDump for () {
    fn add(&mut self, _ty: Ty) {}
}

impl TyDump for Vec<Ty> {
    fn add(&mut self, ty: Ty) {
        self.push(ty);
    }
}

/// use at instantiation stage where all parameters are inferred but
/// we have to catch all falsely marked generic types
pub fn late_instantiate(target: Ty, types: &mut Types, new_ty_dump: &mut Vec<Ty>) -> Ty {
    instantiate_low(target, &[], types, new_ty_dump).unwrap()
}

/// use at type checking stage when changing context of a type, this is needed for example when
/// generic function call return a value
pub fn instantiate(target: Ty, params: &[Ty], types: &mut Types) -> errors::Result<Ty> {
    instantiate_low(target, params, types, &mut ())
}

/// instantiates new type, call this only when clearly necessary, function can add new entities to `types`
/// so keep the target abstract for as long as possible, if you want to create instance of a generic type,
/// wrap the existing generic type ins [`ty::Kind::Instance`] and provide parameters. Instances will get
/// resolved later, similarly as generic functions do.
pub fn instantiate_low(
    target: Ty,
    params: &[Ty],
    types: &mut Types,
    new_type_dump: &mut impl TyDump,
) -> errors::Result<Ty> {
    let ty::Ent { kind, flags, .. } = types.ents[target];

    if !flags.contains(ty::Flags::GENERIC) {
        return Ok(target);
    }

    let result = match kind {
        ty::Kind::Param(i, ..) => params[i as usize],
        ty::Kind::Ptr(ty, ..) => {
            let ty = instantiate_low(ty, params, types, new_type_dump)?;
            pointer_of(ty, types)
        }
        _ => todo!(),
    };

    if result != target {
        new_type_dump.add(result);
    }

    Ok(result)
}

/// performs pattern matching on `parametrized` and `reference`,
/// matched parameters are placed into `params`, `home_params` are backing
/// parameters of reference and `foreign_params` are backing parameters for `parametrized`,
/// this function is mainly used for inferring parameters on generic calls but also when pattern
/// matching two possibly generic function signatures.
pub fn infer_parameters(
    reference: Ty,
    parametrized: Ty,
    params: &mut [Ty],
    home_params: TyList,
    foreign_params: TyList,
    span: Span,
    types: &Types,
    diagnostics: &mut Diagnostics,
) -> errors::Result {
    // TODO: user preallocated vec if needed
    let mut frontier = vec![(reference, parametrized)];

    while let Some((reference, parametrized)) = frontier.pop() {
        let ty::Ent { kind, flags, .. } = types.ents[parametrized];
        if !flags.contains(ty::Flags::GENERIC) {
            continue;
        }

        match (kind, types.ents[reference].kind) {
            (ty::Kind::Param(index), _) => {
                // we use `base_of_low` because we want to compare parameter
                // backing bounds correctly

                let other = params[index as usize];
                let (parametrized, parametrized_id) =
                    types.base_of_low(parametrized, foreign_params);

                if !other.is_reserved_value() {
                    // parameter is already inferred so we just check for equality
                    let (other, other_id) = types.base_of_low(other, home_params);
                    if other_id != parametrized_id || other != parametrized {
                        todo!()
                    }
                } else {
                    let base_ref = types.base_of(parametrized, home_params);
                    drop(implements(types, base_ref, parametrized, diagnostics, span));
                    params[index as usize] = reference;
                }
            }
            (ty::Kind::Ptr(ty, depth), ty::Kind::Ptr(ref_ty, ref_depth)) if depth == ref_depth => {
                frontier.push((ref_ty, ty));
            }
            (a, b) if a == b => {}
            _ => diagnostics.push(Error::GenericTypeMismatch {
                found: reference,
                expected: parametrized,
                loc: span,
            }),
        }
    }

    Ok(())
}

/// parse type parses and ast representation of a type into `ty::Ent`, saves it into `types` ins case new
/// instance was created and returns the id to the type.
pub fn parse_type(
    ty: Ast,
    sources: &Sources,
    ast: &ast::Data,
    scope: &Scope,
    types: &mut Types,
    diagnostics: &mut Diagnostics,
) -> errors::Result<Ty> {
    let res = parse_type_optional(ty, sources, ast, scope, types, diagnostics)?;
    if res.is_reserved_value() {
        let span = ast.nodes[ty].span;
        diagnostics.push(Error::ExpectedConcreteType { loc: span });
        return Err(());
    }
    Ok(res)
}

#[macro_export]
macro_rules! parse_type {
    ($self:expr, $ty:expr) => {
        parse_type(
            $ty,
            $self.sources,
            $self.ast,
            $self.scope,
            $self.types,
            $self.diagnostics,
        )
    };
}

/// parse a type just like `parse_type` but can return `Ty::reserved_value` in case the ty is '_'.
pub fn parse_type_optional(
    ty: Ast,
    sources: &Sources,
    ast: &ast::Data,
    scope: &Scope,
    types: &mut Types,
    diagnostics: &mut Diagnostics,
) -> errors::Result<Ty> {
    let ast::Ent { kind, span, .. } = ast.nodes[ty];
    match kind {
        ast::Kind::Ident => {
            let str = sources.display(span);

            if str == "_" {
                return Ok(Ty::reserved_value());
            }

            scope.get(diagnostics, str, span)
        }
        ast::Kind::Instantiation => {
            parse_instance_type(ty, sources, ast, scope, types, diagnostics)
        }
        ast::Kind::Pointer => parse_ptr_type(ty, sources, ast, scope, types, diagnostics),
        _ => {
            diagnostics.push(Error::InvalidTypeExpression { loc: span });
            return Err(());
        }
    }
}

/// further specification of [`parse_type`], it expects the `ty` to be of [`ast::Kind::Instantiation`], if instance already exists, it is reused.
pub fn parse_instance_type(
    ty: Ast,
    sources: &Sources,
    ast: &ast::Data,
    scope: &Scope,
    types: &mut Types,
    diagnostics: &mut Diagnostics,
) -> errors::Result<Ty> {
    let children = ast.children(ty);
    let header = parse_type(children[0], sources, ast, scope, types, diagnostics)?;

    let mut id = ID::new("<instance>") + types.ents[header].id;
    let mut generic = false;

    for &param in &children[1..] {
        let param = parse_type(param, sources, ast, scope, types, diagnostics)?;
        id = id + types.ents[param].id;
        generic |= types.ents[param].flags.contains(ty::Flags::GENERIC);
        types.args.push_one(param);
    }

    if let Some(&already) = types.instances.get(id) {
        types.args.discard();
        return Ok(already);
    }

    let params = types.args.close_frame();

    let result = {
        let ent = ty::Ent {
            id,
            name: ast.nodes[ty].span,
            kind: ty::Kind::Instance(header, params),
            flags: ty::Flags::GENERIC & generic,
        };
        types.ents.push(ent)
    };

    types.instances.insert(id, result);

    Ok(result)
}

/// further specification of [`parse_type`], it already expects that ty is a [`ast::Kind::Pointer`]
pub fn parse_ptr_type(
    ty: Ast,
    sources: &Sources,
    ast: &ast::Data,
    scope: &Scope,
    types: &mut Types,
    diagnostics: &mut Diagnostics,
) -> errors::Result<Ty> {
    let inner_ty = {
        let inner = ast.children(ty)[0];
        parse_type(inner, sources, ast, scope, types, diagnostics)?
    };

    Ok(pointer_of(inner_ty, types))
}

/// creates a pointer of `ty`, already instantiated entities will be reused.
pub fn pointer_of(ty: Ty, types: &mut Types) -> Ty {
    let ty::Ent {
        kind,
        id,
        name,
        flags,
        ..
    } = types.ents[ty];
    let id = ID::pointer(id);

    if let Some(&already) = types.instances.get(id) {
        return already;
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
        flags,
    };
    let ptr = types.ents.push(ent);

    assert!(types.instances.insert(id, ptr).is_none());

    ptr
}

/// parses an composite bound that can be found after type parameters on generic functions,
/// if such bound already exists no instantiation is performed.
pub fn parse_composite_bound(
    asts: &[Ast],
    span: Span,
    sources: &Sources,
    ast: &ast::Data,
    scope: &Scope,
    types: &mut Types,
    diagnostics: &mut Diagnostics,
) -> errors::Result<Ty> {
    for &bound in asts {
        let Ok(ty) = parse_type(bound, sources, ast, scope, types, diagnostics) else {
            continue;
        };
        types.args.push_one(ty);
    }

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
            .fold(types.ents[types.builtin.any].id, |acc, id| acc + id);

    if let Some(&already) = types.instances.get(id) {
        types.args.discard();
        return Ok(already);
    }

    // make bound combo implement all contained bounds
    for &ty in types.args.top() {
        let ty::Kind::Bound(funcs) = types.ents[ty].kind else {
            unreachable!();
        };
        let bound = types.ents[ty].id;
        let id = ID::bound_impl(bound, id);
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

    let combo = {
        let bounds = types.args.close_frame();
        let ent = ty::Ent {
            id,
            name: span,
            kind: ty::Kind::BoundCombo(bounds),
            flags: ty::Flags::GENERIC,
        };
        types.ents.push(ent)
    };

    assert!(types.instances.insert(id, combo).is_none());

    Ok(combo)
}

pub fn implements(
    types: &Types,
    input: Ty,
    bound: Ty,
    diagnostics: &mut Diagnostics,
    span: Span,
) -> errors::Result {
    // bound can be pain Bound or BoundCombo
    let a = &[bound];
    let bounds = match types.ents[bound].kind {
        ty::Kind::Bound(..) => a,
        ty::Kind::BoundCombo(combo) => types.args.get(combo),
        _ => unreachable!("{:?}", types.ents[bound].kind),
    };

    let input_id = types.ents[input].id;

    let mut result = Ok(());

    for &bound in bounds {
        let bound_id = types.ents[bound].id;
        let id = ID::bound_impl(bound_id, input_id);
        if types.bound_cons.get(id).is_none() {
            diagnostics.push(Error::MissingBound {
                input,
                bound,
                loc: span,
            });
            result = Err(());
        }
    }

    result
}

#[macro_export]
macro_rules! parse_composite_bound {
    ($self:expr, $asts:expr, $span:expr) => {
        parse_composite_bound(
            $asts,
            $span,
            $self.sources,
            $self.ast,
            $self.scope,
            $self.types,
            $self.diagnostics,
        )
    };
}

/// instantiates all builtin items like types, operators and functions.
pub fn create_builtin_items(
    types: &mut Types,
    funcs: &mut Funcs,
    sources: &mut Sources,
    builtin_source: &mut BuiltinSource,
    target: &mut Vec<modules::Item>,
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
                ID::binary(id, ID::new(op))
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
                ID::unary(id, ID::new(op))
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
    dest: &mut Vec<modules::Item>,
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
    let item = modules::Item::new(id, func, span);
    dest.push(item);
}

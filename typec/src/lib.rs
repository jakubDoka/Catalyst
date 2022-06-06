#![feature(let_else)]
#![feature(explicit_generic_args_with_impl_trait)]
#![feature(let_chains)]
#![feature(if_let_guard)]
#![feature(inline_const_pat)]
#![allow(incomplete_features)]
#![feature(atomic_mut_ptr)]

pub mod bound_verifier;
pub mod error;
pub mod global;
pub mod ident_hasher;
pub mod scope;
pub mod state;
pub mod tir;
pub mod ty;
pub mod ty_parser;

use std::str::FromStr;

use cranelift_codegen::isa::CallConv;
pub use scope::ScopeContext;
pub use state::{
    BoundVerifier, GlobalBuilder, IdentHasher, ScopeBuilder, TirBuilder, TyBuilder, TyParser,
};

use ast::*;
use errors::*;
use lexer::*;
use module_types::*;
use storage::*;
use typec_types::{TyError, *};

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

pub fn prepare_params(params: &[Ty], types: &mut Types) {
    for (i, &param) in params.iter().enumerate() {
        let TyKind::Param(index, ..) = &mut types[param].kind else {
            unreachable!("{:?}", types[param].kind);
        };
        *index = i as u8;
    }
}

/// creates a pointer of `ty`, already instantiated entities will be reused.
pub fn pointer_of(ty: Ty, mutable: bool, types: &mut Types, ty_instances: &mut TyInstances) -> Ty {
    let TyEnt {
        kind,
        id,
        name,
        flags,
        ..
    } = types[ty];
    let id = ID::pointer(id, mutable);

    if let Some(&already) = ty_instances.get(id) {
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
        flags: flags & !TyFlags::BUILTIN | TyFlags::MUTABLE & mutable,
    };
    let ptr = types.push(ent);

    assert!(ty_instances.insert(id, ptr).is_none());

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
    for from in builtin.numbers() {
        for to in builtin.numbers() {
            let name = types[to].name;
            let id = {
                let ty = types[from].id;
                let name = sources.id_of(name);
                ID::owned(ty, name)
            };
            create_func(name, &[from], to, id, ty_lists, funcs, target);
        }
    }

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
                builtin_source.make_span(sources, op),
                &[ty, ty],
                ret,
                id,
                ty_lists,
                funcs,
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
                builtin_source.make_span(sources, op),
                &[ty],
                ty,
                id,
                ty_lists,
                funcs,
                target,
            );
        }
    }
}

fn create_func(
    span: Span,
    args: &[Ty],
    ret: Ty,
    id: ID,
    ty_lists: &mut TyLists,
    funcs: &mut Funcs,
    target: &mut Vec<module::ModuleItem>,
) {
    let sig = Sig {
        args: ty_lists.push(args),
        ret,
        ..Default::default()
    };
    let func = {
        let ent = FuncEnt {
            id,
            ..Default::default()
        };
        let meta = FuncMeta {
            sig,
            name: span,
            kind: FuncKind::Builtin,
            ..Default::default()
        };
        funcs.push(ent, meta)
    };

    let item = module::ModuleItem::new(id, func, span);
    target.push(item);
}

pub fn int_value(sources: &Sources, span: Span, signed: bool) -> u128 {
    let mut chars = sources.display(span).chars();
    let mut value = 0;
    while let Some(c @ '0'..='9') = chars.next() {
        value = value * 10 + (c as u128 - '0' as u128);
    }

    if signed {
        value = value.wrapping_sub(i128::MIN as u128);
    }

    value
}

#[derive(Debug, Clone, Copy)]
pub enum CharError {
    ExtraCharacters,
    NoCharacter,
}

pub fn char_value(sources: &Sources, span: Span) -> std::result::Result<char, CharError> {
    let mut chars = sources.display(span.strip_sides()).chars();
    let char = chars.next().ok_or(CharError::NoCharacter)?;
    if chars.next().is_some() {
        return Err(CharError::ExtraCharacters);
    }
    Ok(char)
}

pub fn parse_call_conv(call_conv: Ast, sources: &Sources, ast_data: &AstData, diagnostics: &mut Diagnostics) -> Option<CallConv> {
    if call_conv.is_reserved_value() {
        Some(CallConv::Fast)
    } else {
        let span = ast_data.nodes[call_conv].span.strip_sides();
        let str = sources.display(span);
        if str == "default" {
            None
        } else {
            CallConv::from_str(str)
                .map_err(|_| {
                    diagnostics
                        .push(TyError::InvalidCallConv { loc: span })
                })
                .ok()
        }
    }
}
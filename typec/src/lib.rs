#![feature(let_else)]
#![feature(explicit_generic_args_with_impl_trait)]
#![feature(let_chains)]
#![feature(bool_to_option)]

pub mod collector;
pub mod func;
pub mod tir;
pub mod ty;
pub mod error;

pub use collector::*;
pub use func::*;
pub use tir::*;
pub use ty::*;
pub use error::Error;

use lexer::*;
use modules::*;
use parser::*;

pub trait TypeParser {

    fn state(&mut self) -> (&mut Scope, &mut Types, &Sources, &ast::Data, &mut errors::Diagnostics);

    fn parse_type(&mut self, ty: Ast) -> errors::Result<Ty> {
        let (scope, _types, sources, ast, diagnostics) = self.state();
        let ast::Ent { kind, span, .. } = ast.nodes[ty];
        match kind {
            ast::Kind::Ident => {
                let str = sources.display(span);
                return scope.get(diagnostics, str, span)
            }
            _ => todo!("Unhandled type expr {:?}: {}", kind, sources.display(span)),
        }
    }
}

pub fn create_builtin_items(
    types: &mut Types,
    funcs: &mut Funcs,
    sources: &mut Sources,
    builtin_source: &mut BuiltinSource,
) -> Vec<module::Item> {
    let comparison_operators = "== != < > <= >=";
    let math_operators = "+ - * / %";
    let integer_binary_operators = format!("{} {}", comparison_operators, math_operators);
    let math_unary_operators = "-";
    let integer_unary_operators = format!("{}", math_unary_operators);

    let mut vec = vec![];

    for ty in types.builtin.all() {
        let ent = &types.ents[ty];
        vec.push(modules::Item::new(ent.id, ty, ent.name));
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
                &mut vec
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
                &mut vec
            );
        }
    }

    vec
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
    println!("{}", name);
    let sig = Sig {
        args: types.args.push(args),
        ret,
        ..Default::default()
    };
    let func = {
        let ent = func::Ent {
            sig,
            name: span,
            id,
            kind: func::Kind::Builtin,
            ..Default::default()
        };
        funcs.push(ent)
    };
    let item = module::Item::new(id, func, span);
    dest.push(item);
}
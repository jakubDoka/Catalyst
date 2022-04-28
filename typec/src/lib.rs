#![feature(let_else)]
#![feature(explicit_generic_args_with_impl_trait)]
#![feature(let_chains)]
#![feature(bool_to_option)]
#![feature(if_let_guard)]

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

    fn state(&mut self) -> (&mut Scope, &mut Types, &Sources, &mut Modules, &ast::Data, &mut errors::Diagnostics);

    fn parse_type(&mut self, ty: Ast) -> errors::Result<Ty> {
        let (scope, _types, sources, _modules, ast, diagnostics) = self.state();
        let ast::Ent { kind, span, .. } = ast.nodes[ty];
        match kind {
            ast::Kind::Ident => {
                let str = sources.display(span);
                scope.get(diagnostics, str, span)
            }
            ast::Kind::Pointer => self.parse_pointer_type(ty),
            _ => todo!("Unhandled type expr {:?}: {}", kind, span.log(sources)),
        }
    }

    fn parse_pointer_type(&mut self, ty: Ast) -> errors::Result<Ty> {
        let ast = self.state().4;
        let inner_ty = {
            let inner = ast.children(ty)[0];
            self.parse_type(inner)?
        };
        
        
        let (scope, types, _, modules, ast, diagnostics) = self.state();
        let source = types.ents[inner_ty].name.source();
        let id = {
            let ty = types.ents[inner_ty].id;
            Self::pointer_id(ty)
        };

        if let Some(ptr) = scope.weak_get::<Ty>(id) {
            return Ok(ptr);
        }

        let name = ast.nodes[ty].span; 
        let ent = ty::Ent {
            id,
            name,
            kind: ty::Kind::Pointer(inner_ty),
        };
        let ty = types.ents.push(ent);
        let item = modules::Item::new(id, ty, name);
        
        drop(scope.insert(diagnostics, source, id, item.to_scope_item()));
        modules[source].items.push(item);

        Ok(ty)
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
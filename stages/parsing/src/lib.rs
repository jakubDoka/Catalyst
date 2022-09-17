#![feature(default_free_fn)]
#![feature(let_else)]
#![feature(type_alias_impl_trait)]

#[macro_export]
macro_rules! token {
    (none) => {
        None
    };
    ($value:ident) => {
        Some(TokenKind::$value)
    };
}

#[macro_export]
macro_rules! branch {
    (
        $self:expr => {
            $($cond:ident$(($($value:pat = $default:expr),*))? => $res:expr,)*
            $(_ => $default_branch:expr,)?
        }
    ) => {
        match $self.state.current.kind {
            $(TokenKind::$cond$(($($value),*))? => $res,)*
            _ => {
                branch!(__default_branch__ $self, ($($cond$(($($value = $default),*))?),*) $($default_branch)?)
            },
        }
    };

    (__default_branch__ $self:expr, ($($cond:ident$(($($value:pat = $default:expr),*))?),*)) => {
        {
            let terminals: &[TokenPattern] = &[
                $(TokenPattern::Kind(TokenKind::$cond$(($($default),*))?)),*
            ];
            $self.expect_error(terminals);
            return Err(())
        }
    };


    (__default_branch__ $self:expr, ($($ignored:tt)*) $default_branch:expr) => {
        {
            $default_branch
        }
    };

    (
        str $self:expr => {
            $($str:literal => $res:expr,)*
        }
    ) => {
        match $self.current_token_str() {
            $($str => $res,)*
            _ => {
                let terminals = [
                    $($str),*
                ];
                $self.expect_str_error(&terminals);
                return Err(())
            },
        }
    };
}

mod parser;

pub use parser::{
    bound::BoundExprAst,
    expr::{BinaryExprAst, BlockAst, BlockMeta, ExprAst, ReturnExprAst, UnitExprAst},
    func::{FuncArgAst, FuncArgMeta, FuncArgsAst, FuncBodyAst, FuncDefAst, FuncSigAst},
    imports::{ImportAst, ImportsAst, ImportsMeta, UseAst},
    items::{ItemAst, ItemsAst, ItemsMeta},
    manifest::{
        DepsMeta, ManifestAst, ManifestDepAst, ManifestDepsAst, ManifestFieldAst, ManifestListAst,
        ManifestListMeta, ManifestObjectAst, ManifestObjectMeta, ManifestValueAst,
    },
    r#struct::{
        StructAst, StructBodyAst, StructBodyMeta, StructConstructorFieldAst, StructFieldAst,
    },
    ty::{
        MutabilityAst, TyAst, TyGenericsAst, TyGenericsMeta, TyInstanceAst, TyPointerAst,
        TyTupleAst,
    },
    BoundsMeta, GenericParamAst, GenericsAst, GenericsMeta, ParamBoundsAst, PathAst,
    PathSegmentAst, StructConstructorAst, StructConstructorMeta, TupleConstructorAst,
    TupleConstructorMeta,
};

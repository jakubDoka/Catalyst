#![feature(default_free_fn)]
#![feature(let_else)]
#![feature(type_alias_impl_trait)]
#![feature(let_chains)]

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
            let terminals: &[TokenPat] = &[
                $(TokenPat::Kind(TokenKind::$cond$(($($default),*))?)),*
            ];
            $self.expect_error(terminals);
            return None
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
                return None
            },
        }
    };
}

mod parser;

pub use parser::{
    expr::{
        BinaryExprAst, BlockAst, BlockMeta, CallArgsAst, CallArgsMeta, CallExprAst, ConstAst,
        DotExprAst, ElifAst, EnumCtorAst, EnumCtorPatAst, ExprAst, IfAst, IfBlockAst, MatchArmAst,
        MatchBodyAst, MatchExprAst, PatAst, PathInstanceAst, ReturnExprAst, StructCtorAst,
        StructCtorBodyAst, StructCtorPatAst, StructCtorPatBodyAst, StructCtorPatBodyMeta,
        StructCtorPatFieldAst, TypedPathAst, UnitExprAst,
    },
    func::{FuncArgAst, FuncArgMeta, FuncArgsAst, FuncBodyAst, FuncDefAst, FuncSigAst},
    imports::{ImportAst, ImportsAst, ImportsMeta, UseAst, UseAstSkip},
    items::{
        EnumAst, EnumBodyAst, EnumVariantAst, GroupedItemSlice, GroupedItemsAst, ImplAst,
        ImplBodyAst, ImplItemAst, ImplTarget, InlineModeAst, ItemAst, ItemBodyMeta, ItemsAst,
        ItemsMeta, SpecAst, SpecBodyAst, TopLevelAttributeAst, TopLevelAttributeKindAst,
    },
    manifest::{
        DepsMeta, ManifestAst, ManifestDepAst, ManifestDepsAst, ManifestFieldAst, ManifestListAst,
        ManifestListMeta, ManifestObjectAst, ManifestObjectMeta, ManifestValueAst,
    },
    r#struct::{StructAst, StructBodyAst, StructBodyMeta, StructCtorFieldAst, StructFieldAst},
    spec::SpecExprAst,
    ty::{
        MutabilityAst, TyAst, TyGenericsAst, TyGenericsMeta, TyInstanceAst, TyPointerAst,
        TyTupleAst,
    },
    BoundsMeta, GenericParamAst, GenericsAst, GenericsMeta, ParamSpecsAst, PathExprAst,
    TupleCtorAst, TupleCtorMeta,
};

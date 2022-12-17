#![feature(default_free_fn)]
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
            @$($fallback:tt)*
        }
    ) => {
        match $self.state.current.kind {
            $(TokenKind::$cond$(($($value),*))? => $res,)*
            _ => $crate::branch!(@fallback ($self, $($cond$(($($default),*))?)*) $($fallback)*),
        }
    };

    (
        @fallback $ignored:tt => $handler:expr
    ) => {
        $handler
    };

    (
        @fallback ($self:expr, $($cond:ident$(($($default:expr),*))?)*) $ast_name:literal
    ) => {
        const EXPECTED: &[&str] = &[
            $(
                TokenKind::$cond$(($($value),*))?.as_str(),
            )*
        ];
        $crate::parser::ExpectedStartOfAst {
            ast_name: $ast_name,
            expected: EXPECTED,
        }
    };

    (
        str $self:expr => {
            $($str:literal => $res:expr,)*
            @$($fallback:tt)*
        }
    ) => {
        match $self.current_token_str() {
            $($str => $res,)*
            _ => {
                $crate::branch!(@str_fallback ($self, $($str)*) $($fallback)*)
            },
        }
    };

    (
        @str_fallback $ignored:tt => $handler:expr,
    ) => {
        $handler
    };

    (
        @str_fallback ($self:expr, $($str:literal)*) $ast_name:literal,
    ) => {
        {
            const EXPECTED: &[&str] = &[$($str,)*];
            $self.workspace.push($crate::parser::ExpectedStartOfAst {
                ast_name: $ast_name,
                expected: EXPECTED,
                found: $self.state.current.kind,
                loc: $self.loc(),
            })?
        }
    };

    (
        @fallback $ignored:tt => $handler:expr,
    ) => {
        $handler
    };

    (
        @fallback ($self:expr, $($cond:ident$(($($default:expr),*))?)*) $ast_name:literal,
    ) => {
        {
            const EXPECTED: &[&str] = &[
                $(
                    TokenKind::$cond$(($($default),*))?.as_str(),
                )*
            ];
            $self.workspace.push($crate::parser::ExpectedStartOfAst {
                ast_name: $ast_name,
                expected: EXPECTED,
                found: $self.state.current.kind,
                loc: $self.loc(),
            })?
        }
    };
}

mod parser;

pub use parser::{
    expr::{
        control_flow::{
            BreakAst, ContinueAst, ElifAst, IfAst, IfBlockAst, LoopAst, MatchArmAst, MatchBodyAst,
            MatchExprAst, ReturnExprAst,
        },
        pat::{
            EnumCtorPatAst, PatAst, StructCtorPatAst, StructCtorPatBodyAst, StructCtorPatBodyMeta,
            StructCtorPatFieldAst,
        },
        BinaryExprAst, BlockAst, BlockMeta, CallArgsAst, CallArgsMeta, CallExprAst, DotExprAst,
        EnumCtorAst, ExprAst, LetAst, StructCtorAst, StructCtorBodyAst, UnitExprAst,
    },
    func::{FuncArgAst, FuncArgMeta, FuncArgsAst, FuncBodyAst, FuncDefAst, FuncSigAst},
    imports::{ImportAst, ImportsAst, ImportsMeta, UseAst, UseAstSkip},
    items::{
        EnumAst, EnumBodyAst, EnumVariantAst, GroupedItemSlice, GroupedItemsAst, ImplAst,
        ImplBodyAst, ImplItemAst, ImplTarget, InlineModeAst, ItemAst, ItemBodyMeta, ItemsAst,
        ItemsMeta, SpecAst, SpecBodyAst, TopLevelAttrKindAst, TopLevelAttributeAst,
    },
    manifest::{
        DepsMeta, ManifestAst, ManifestDepAst, ManifestDepsAst, ManifestFieldAst, ManifestListAst,
        ManifestListMeta, ManifestObjectAst, ManifestObjectMeta, ManifestValueAst,
    },
    r#struct::{StructAst, StructBodyAst, StructBodyMeta, StructCtorFieldAst, StructFieldAst},
    spec::SpecExprAst,
    ty::{MutabilityAst, TyAst, TyGenericsAst, TyGenericsMeta, TyPointerAst, TyTupleAst},
    BoundsMeta, GenericParamAst, GenericsAst, GenericsMeta, ParamSpecsAst, PathAst, PathItemAst,
    TupleCtorAst, TupleCtorMeta,
};

use std::{default::default, mem};

use crate::{ctx::*, TirBuilder};

use {ast::*, diags::*, span::*, std::iter, storage::*, types::*};

mod building;
mod collecting;

pub struct TypecParser<'arena, 'ctx> {
    arena: &'arena Arena,
    ctx: &'ctx mut TypecCtx,
    ext: TypecExternalCtx<'arena, 'ctx>,
    meta: TypecMeta,
}

impl<'arena, 'ctx> TypecParser<'arena, 'ctx> {
    pub fn new(
        arena: &'arena Arena,
        ctx: &'ctx mut TypecCtx,
        ext: TypecExternalCtx<'arena, 'ctx>,
        meta: TypecMeta,
    ) -> Self {
        Self {
            arena,
            ctx,
            ext,
            meta,
        }
    }

    pub fn execute(&mut self, items: GroupedItemsAst<'arena>) {
        self.ctx.clear();

        self.collect(items.specs, Self::collect_spec)
            .collect(items.structs, Self::collect_struct)
            .collect(items.enums, Self::collect_enum)
            .build(Self::build_spec)
            .build(Self::build_struct)
            .build(Self::build_enum)
            .collect(items.consts, Self::collect_const)
            .collect(items.funcs, Self::collect_func)
            .collect_impls(items.impls);

        self.ctx.detect_infinite_types(&mut self.ext, &self.meta);

        let funcs = mem::take(&mut self.ext.transfer.funcs);
        self.build_funcs(&funcs, TyParamIter::default());
        self.ext.transfer.funcs = funcs;

        self.build_impl_funcs();
    }

    fn span_str(&self, span: Span) -> &'ctx str {
        self.meta.span_str(span, self.ext.resources)
    }

    fn generics(
        &mut self,
        generic_ast: Option<ListAst<ParamAst>>,
        set: &mut SpecSet,
        mut params: impl AsMut<TyParamIter>,
    ) {
        let Some(generic_ast) = generic_ast else {return};

        for (&ParamAst { specs, .. }, i) in generic_ast.iter().zip(params.as_mut()) {
            let Some(ParamSpecsAst { first, rest, .. }) = specs else {continue};
            set.extend(
                i,
                i,
                default(),
                rest.iter()
                    .map(|(.., s)| s)
                    .chain(iter::once(&first))
                    .filter_map(|&b| self.spec(b)),
            )
        }
    }

    pub(crate) fn ty(&mut self, ty_ast: TyAst) -> Option<Ty> {
        match ty_ast {
            TyAst::Path(ident) => match self.ty_path(ident)? {
                (TyPathResult::Ty(ty), None) => Some(ty),
                (TyPathResult::Ty(ty), Some(params)) => {
                    let ty = ty.as_generic().or_else(|| todo!())?;
                    let args = params
                        .iter()
                        .map(|&p| self.ty(p))
                        .nsc_collect::<Option<BumpVec<_>>>()?;
                    Some(Ty::Instance(
                        self.ext.creator().instance(ty, args.as_slice()),
                    ))
                }
                (TyPathResult::Spec(..), ..) => todo!(),
            },
            TyAst::Pointer(&pointer) => self.pointer(pointer).map(Into::into),
            TyAst::Tuple(tuple) => self.tuple_ty(tuple),
            TyAst::Array(&array) => self.array_ty(array).map(Into::into),
            TyAst::Wildcard(..) => todo!(),
        }
    }

    fn array_ty(&mut self, array_ast: TyArrayAst) -> Option<Ty> {
        let ty = self.ty(array_ast.ty)?;
        let size = self
            .const_fold(Some(Ty::UINT), array_ast.size)?
            .0
            .as_array_size()?;
        Some(Ty::Array(self.ext.creator().array_of(ty, size)))
    }

    fn builder(&mut self, ty: impl Into<Option<Ty>>) -> TirBuilder<'arena, '_> {
        TirBuilder::new(ty, self.arena, self.ctx, self.ext.clone_borrow(), self.meta)
    }

    fn tuple_ty(&mut self, tuple_ast: ListAst<TyAst>) -> Option<Ty> {
        let types = tuple_ast
            .iter()
            .map(|&ty_ast| self.ty(ty_ast))
            .nsc_collect::<Option<BumpVec<_>>>()?;

        match (types.as_slice(), tuple_ast.elements) {
            ([], _) => Some(Ty::UNIT),
            _ => todo!(),
        }
    }

    fn spec_sum<'a>(
        &mut self,
        specs: impl Iterator<Item = &'a SpecExprAst<'a>>,
        spec_set: &mut SpecSet,
    ) -> Option<FragSlice<Spec>> {
        let specs = specs
            .map(|&ast| self.spec(ast))
            .nsc_collect::<Option<BumpVec<_>>>()?;
        for &spec in specs.iter() {
            self.ext.types.register_spec_generics(spec, spec_set)
        }
        Some(self.ext.creator().spec_sum(specs.iter().copied()))
    }

    fn spec(&mut self, spec_ast: SpecExprAst) -> Option<Spec> {
        match self.ty_path(spec_ast.path)? {
            (TyPathResult::Ty(..), ..) => todo!(),
            (TyPathResult::Spec(spec), None) => Some(Spec::Base(spec)),
            (TyPathResult::Spec(spec), Some(params)) => {
                let args = params
                    .iter()
                    .map(|&p| self.ty(p))
                    .nsc_collect::<Option<BumpVec<_>>>()?;
                Some(Spec::Instance(
                    self.ext.creator().spec_instance(spec, args.as_slice()),
                ))
            }
        }
    }

    fn pointer(&mut self, TyPointerAst { mutability, ty, .. }: TyPointerAst) -> Option<Pointer> {
        let base = self.ty(ty)?;
        let mutability = self.mutability(mutability)?;
        Some(self.ext.creator().pointer_to(mutability.as_param(), base))
    }

    pub(crate) fn mutability(
        &mut self,
        mutability_ast: Option<MutabilityAst>,
    ) -> Option<Mutability> {
        Some(match mutability_ast {
            Some(MutabilityAst::Mut(..)) => Mutability::Mutable,
            None => Mutability::Immutable,
            Some(MutabilityAst::Generic(
                ..,
                PathAst {
                    slash: None,
                    start: PathSegmentAst::Name(start),
                    segments: &[],
                },
            )) => match lookup!(Ty self, start.ident, start.span) {
                Ty::Param(param) => Mutability::Param(param.index),
                _ => todo!(),
            },
            Some(MutabilityAst::Generic(..)) => todo!(),
        })
    }

    pub(crate) fn ty_path<'a>(
        &mut self,
        path @ PathAst {
            start, segments, ..
        }: PathAst<'a>,
    ) -> Option<(TyPathResult, Option<ListAst<'a, TyAst<'a>>>)> {
        let PathSegmentAst::Name(start) = start else {
            todo!();
        };

        let (item, segments) = match self.lookup(start.ident, start.span, "type or module")? {
            ScopeItem::Module(module) => {
                let &[PathSegmentAst::Name(ty), ref segments @ ..] = segments else {
                    MissingIdentAfterMod {
                        loc: self.meta.loc(segments.first().map(|s| s.span()).unwrap_or(path.span())),
                    }.add(self.ext.workspace)?;
                };
                let id = self.ext.interner.intern_scoped(module.index(), ty.ident);
                (self.lookup(id, path.span(), "type or spec")?, segments)
            }
            item => (item, segments),
        };

        Some((
            match item {
                ScopeItem::Ty(ty) => TyPathResult::Ty(ty),
                ScopeItem::SpecBase(spec) => TyPathResult::Spec(spec),
                item => self.invalid_symbol_type(item, start.span, "types or spec")?,
            },
            match segments {
                [] => None,
                &[PathSegmentAst::Params(generics)] => Some(generics),
                _ => todo!(),
            },
        ))
    }

    pub(crate) fn lookup(
        &mut self,
        sym: Ident,
        span: Span,
        what: &'static str,
    ) -> Option<ScopeItem> {
        self.ctx.lookup(sym, span, what, &mut self.ext, &self.meta)
    }

    pub(crate) fn invalid_symbol_type(
        &mut self,
        item: ScopeItem,
        span: Span,
        what: &'static str,
    ) -> Option<!> {
        self.ctx
            .invalid_symbol_type(item, span, what, self.ext.workspace, &self.meta)
    }

    fn const_fold(&mut self, ty: Option<Ty>, value: ExprAst) -> Option<(FolderValue, Ty)> {
        let (body, ty) = self.builder(ty).expr_body(value, Inference::from(ty))?;
        if let Some(shortcut) = self.const_fold_shortcut(body) {
            return Some((shortcut, ty));
        }
        Some((self.ext.const_fold(ty, body), ty))
    }

    fn const_fold_shortcut(&mut self, body: TirNode) -> Option<FolderValue> {
        let TirKind::Return(Some(ret), ..) = body.kind else {
            return None;
        };

        Some(match ret.kind {
            TirKind::ConstAccess(c) => self.ext.types[c].value.clone(),
            TirKind::Int(i) => FolderValue::new_register(
                i.unwrap_or_else(|| self.span_str(ret.span).parse().unwrap()) as u64,
            ),
            // TODO: most common cases are covered, we can add others later
            _ => return None,
        })
    }
}

pub(crate) enum TyPathResult {
    Ty(Ty),
    Spec(FragRef<SpecBase>),
}

pub const MOD_HELP: &str =
    "syntax for specifying module (applies on methods as well): `<mod>\\<item>`";

ctl_errors! {
    #[err => "missing identifier after module"]
    #[info => "module is always followed by the name of an item"]
    #[help => MOD_HELP]
    error MissingIdentAfterMod: fatal {
        #[err loc]
        loc: SourceLoc,
    }
}

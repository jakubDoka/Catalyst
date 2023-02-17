use crate::{context::*, TirBuilder};

use {diags::*, lexing_t::*, parsing_t::*, std::iter, storage::*, typec_t::*};

mod building;
mod collecting;

pub struct TypecParser<'arena, 'ctx> {
    pub(crate) arena: &'arena Arena,
    pub(crate) ctx: &'ctx mut TypecCtx,
    pub(crate) ext: TypecExternalCtx<'arena, 'ctx>,
    pub(crate) meta: TypecMeta,
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

    pub fn span_str(&self, span: Span) -> &'ctx str {
        self.meta.span_str(span, self.ext.resources)
    }

    pub fn generics(
        &mut self,
        generic_ast: Option<ListAst<ParamAst>>,
        set: &mut SpecSet,
        offset: usize,
    ) {
        let Some(generic_ast) = generic_ast else {return};

        for (i, &ParamAst { specs, .. }) in generic_ast.iter().enumerate() {
            let Some(ParamSpecsAst { first, rest, .. }) = specs else {continue};
            set.extend(
                (i + offset) as u32,
                rest.iter()
                    .map(|(.., s)| s)
                    .chain(iter::once(&first))
                    .filter_map(|&b| self.spec(b)),
            )
        }
    }

    pub fn ty(&mut self, ty_ast: TyAst) -> Option<Ty> {
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
        let _ty = self.ty(array_ast.ty)?;
        let size = self
            .builder(Ty::UINT)
            .expr_body(array_ast.size, Inference::Strong(Ty::UINT))?;
        let _size_const = match size.kind {
            TirKind::ConstAccess(const_id) => const_id,
            _ => todo!(),
        };
        todo!()
    }

    pub fn builder(&mut self, ty: Ty) -> TirBuilder<'arena, '_> {
        TirBuilder::new(self.arena, ty, self.ctx, self.ext.clone_borrow(), self.meta)
    }

    pub fn tuple_ty(&mut self, tuple_ast: ListAst<TyAst>) -> Option<Ty> {
        let types = tuple_ast
            .iter()
            .map(|&ty_ast| self.ty(ty_ast))
            .nsc_collect::<Option<BumpVec<_>>>()?;

        match (types.as_slice(), tuple_ast.elements) {
            ([], _) => Some(Ty::UNIT),
            _ => todo!(),
        }
    }

    pub fn spec_sum<'a>(
        &mut self,
        specs: impl Iterator<Item = &'a SpecExprAst<'a>>,
        spec_set: &mut SpecSet,
    ) -> Option<FragSlice<Spec>> {
        let specs = specs
            .map(|&ast| self.spec(ast))
            .nsc_collect::<Option<BumpVec<_>>>()?;
        for &spec in specs.iter() {
            self.ext.typec.register_spec_generics(spec, spec_set)
        }
        Some(self.ext.creator().spec_sum(specs.iter().copied()))
    }

    pub fn spec(&mut self, spec_ast: SpecExprAst) -> Option<Spec> {
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
        Some(
            self.ext
                .creator()
                .pointer_to(RawMutability::new(mutability).expect("todo"), base),
        )
    }

    pub fn mutability(&mut self, mutability_ast: Option<MutabilityAst>) -> Option<Mutability> {
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
                Ty::Param(i) => Mutability::Param(i),
                _ => todo!(),
            },
            Some(MutabilityAst::Generic(..)) => todo!(),
        })
    }

    pub fn ty_path<'a>(
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
                item => self.invalid_symbol_type(item, start.span, "typec or spec")?,
            },
            match segments {
                [] => None,
                &[PathSegmentAst::Params(generics)] => Some(generics),
                _ => todo!(),
            },
        ))
    }

    pub fn lookup(&mut self, sym: Ident, span: Span, what: &'static str) -> Option<ScopeItem> {
        self.ctx.lookup(sym, span, what, &mut self.ext, &self.meta)
    }

    pub fn invalid_symbol_type(
        &mut self,
        item: ScopeItem,
        span: Span,
        what: &'static str,
    ) -> Option<!> {
        self.ctx
            .invalid_symbol_type(item, span, what, self.ext.workspace, &self.meta)
    }
}

pub enum TyPathResult {
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

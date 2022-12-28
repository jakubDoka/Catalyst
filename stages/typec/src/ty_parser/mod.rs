use std::iter;

use diags::*;
use lexing_t::Span;
use packaging_t::Source;
use parsing_t::*;

use storage::*;

use typec_t::*;

use crate::*;

impl TyChecker<'_> {
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

    pub fn insert_generics(
        &mut self,
        generics_ast: Option<ListAst<ParamAst>>,
        offset: usize,
    ) -> usize {
        let Some(generics_ast) = generics_ast else {return 0};

        for (i, &ParamAst { name, .. }) in generics_ast.iter().enumerate() {
            self.insert_param(offset + i, name)
        }

        generics_ast.len()
    }

    fn insert_param(&mut self, index: usize, name: NameAst) {
        self.scope
            .push(name.ident, Ty::Param(index as u8), name.span);
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
                    Some(Ty::Instance(self.typec.instance(
                        ty,
                        args.as_slice(),
                        self.interner,
                    )))
                }
                (TyPathResult::Spec(..), ..) => todo!(),
            },
            TyAst::Pointer(&pointer) => self.pointer(pointer).map(Into::into),
            TyAst::Tuple(tuple) => self.tuple(tuple),
            TyAst::Wildcard(..) => todo!(),
        }
    }

    pub fn tuple(&mut self, tuple_ast: ListAst<TyAst>) -> Option<Ty> {
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
            self.typec.register_spec_generics(spec, spec_set)
        }
        Some(self.typec.spec_sum(specs.iter().copied(), self.interner))
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
                Some(Spec::Instance(self.typec.spec_instance(
                    spec,
                    args.as_slice(),
                    self.interner,
                )))
            }
        }
    }

    fn pointer(
        &mut self,
        TyPointerAst { mutability, ty, .. }: TyPointerAst,
    ) -> Option<(FragRef<Pointer>, RawMutability)> {
        let base = self.ty(ty)?;
        let mutability = self.mutability(mutability)?;
        Some((
            self.typec.pointer_to(base, self.interner),
            RawMutability::new(mutability).expect("todo"),
        ))
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
                    self.workspace.push(MissingIdentAfterMod {
                        span: segments.first().map(|s| s.span()).unwrap_or(path.span()),
                        source: self.source,
                    })?;
                };
                let id = self.interner.intern_scoped(module.index(), ty.ident);
                (self.lookup(id, path.span(), "type or spec")?, segments)
            }
            item => (item, segments),
        };

        Some((
            match item {
                ScopeItem::Ty(ty) => TyPathResult::Ty(ty),
                ScopeItem::SpecBase(spec) => TyPathResult::Spec(spec),
                item => self.invalid_symbol_type(item, start.span, "type or spec")?,
            },
            match segments {
                [] => None,
                &[PathSegmentAst::Params(generics)] => Some(generics),
                _ => todo!(),
            },
        ))
    }

    pub fn lookup(&mut self, sym: Ident, span: Span, what: &'static str) -> Option<ScopeItem> {
        self.scope
            .get(sym)
            .map_err(|err| self.scope_error(err, sym, span, what))
            .ok()
    }

    pub fn insert_spec_functions(&mut self, generics: Generics, offset: usize) {
        let specs = self.typec[generics]
            .iter()
            .enumerate()
            .flat_map(|(i, &spec)| self.typec[spec].iter().map(move |&spec| (offset + i, spec)))
            .collect::<BumpVec<_>>();
        for (i, generic) in specs.into_iter() {
            self.insert_spec_functions_recur(i, generic);
        }
    }

    fn insert_spec_functions_recur(&mut self, index: usize, generic: Spec) {
        let spec_base = generic.base(self.typec);
        let functions = self.typec[spec_base].methods;

        for (key, &func) in functions.keys().zip(&self.typec[functions]) {
            let id = self
                .interner
                .intern_scoped(Ty::Param(index as u8), func.name);
            self.scope.push(id, key, func.span);
        }
    }

    pub fn scope_error(
        &mut self,
        err: ScopeError,
        sym: Ident,
        span: Span,
        expected: &'static str,
    ) -> Option<!> {
        match err {
            ScopeError::NotFound => self.workspace.push(ScopeItemNotFound {
                span,
                source: self.source,
                expected,
                queried: self.interner[sym].to_string(),
            }),
            ScopeError::Collision => {
                let suggestions = self.resources.module_deps
                    [self.resources.modules[self.module].deps]
                    .iter()
                    .filter(|dep| self.typec[dep.ptr].items.values().any(|i| i.id == sym))
                    .map(|dep| &self.interner[dep.name])
                    .intersperse(", ")
                    .collect::<String>();
                self.workspace.push(ScopeItemCollision {
                    suggestions,
                    span,
                    source: self.source,
                })
            }
            ScopeError::Inaccessible(pos, item_def) => self.workspace.push(InaccessibleScopeItem {
                span,
                source: self.source,
                item_def,
                pos,
            }),
        }
    }

    pub fn invalid_symbol_type(
        &mut self,
        item: ScopeItem,
        span: Span,
        expected: &'static str,
    ) -> Option<!> {
        self.workspace.push(InvalidScopeItemType {
            span,
            source: self.source,
            expected,
            actual: item.name(),
        })
    }
}

pub enum TyPathResult {
    Ty(Ty),
    Spec(FragRef<SpecBase>),
}

const MOD_HELP: &str = "syntax for specifying module (applies on methods as well): `<mod>\\<item>`";

ctl_errors! {
    #[err => "scope item not found"]
    #[help => "expected {expected}"]
    #[info => "debug: queried '{queried}'"]
    error ScopeItemNotFound: fatal {
        #[err source, span, "this does not exist or is not imported"]
        expected: &'static str,
        queried ref: String,
        span: Span,
        source: VRef<Source>,
    }

    #[err => "identifier is ambiguous"]
    #[info => "items from multiple modules match the identifier"]
    #[help => "you have to specify one of ({suggestions}) as a module"]
    #[help => MOD_HELP]
    error ScopeItemCollision: fatal {
        #[err source, span, "here"]
        suggestions ref: String,
        span: Span,
        source: VRef<Source>,
    }

    #[err => "invalid scope item type"]
    #[info => "expected {expected}, got {actual}"]
    error InvalidScopeItemType: fatal {
        #[err source, span, "here"]
        expected: &'static str,
        actual: &'static str,
        span: Span,
        source: VRef<Source>,
    }

    #[err => "inaccessible scope item"]
    #[info => ("item is defined in a different {}", match pos {
        ScopePosition::Module => "module and is private",
        ScopePosition::Package => "package and is not public",
    })]
    error InaccessibleScopeItem: fatal {
        #[info item_def, "item defined here"]
        #[err source, span, "here"]
        pos: ScopePosition,
        span: Span,
        item_def: SourceLoc,
        source: VRef<Source>,
    }

    #[err => "missing identifier after module"]
    #[info => "module is always followed by the name of an item"]
    #[help => MOD_HELP]
    error MissingIdentAfterMod: fatal {
        #[err source, span, "here"]
        span: Span,
        source: VRef<Source>,
    }
}

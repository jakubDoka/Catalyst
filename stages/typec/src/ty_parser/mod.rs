use std::ops::Deref;

use diags::*;
use lexing_t::Span;
use packaging_t::span_str;
use parsing::*;
use parsing_t::*;

use storage::*;

use typec_t::*;

use crate::*;

impl TyChecker<'_> {
    pub fn generics(&mut self, generic_ast: GenericsAst) -> Generics {
        let mut generics = bumpvec!(cap generic_ast.len());
        for &GenericParamAst { bounds, .. } in generic_ast.iter() {
            let bound = self.spec_sum(bounds.iter()).unwrap_or_default();
            generics.push(bound);
        }
        self.typec.params.bump(generics)
    }

    pub fn insert_generics(&mut self, generics_ast: GenericsAst, offset: usize) {
        for (i, &GenericParamAst { name, .. }) in generics_ast.iter().enumerate() {
            self.insert_param(offset + i, name)
        }
    }

    fn insert_param(&mut self, index: usize, name: NameAst) {
        self.scope
            .push(name.ident, Ty::Param(index as u16), name.span);
    }

    pub fn ty(&mut self, ty_ast: TyAst) -> Option<Ty> {
        match ty_ast {
            TyAst::Path(ident) => match self.ty_path(ident)? {
                TyPathResult::Ty(ty) => Some(ty),
                TyPathResult::Spec(..) => todo!(),
            },
            TyAst::Instance(instance) => self.instance(instance).map(Ty::Instance),
            TyAst::Pointer(&pointer) => self.pointer(pointer).map(Ty::Pointer),
            TyAst::Tuple(tuple) => self.tuple(tuple),
            TyAst::Wildcard(..) => todo!(),
        }
    }

    pub fn tuple(&mut self, tuple_ast: TyTupleAst) -> Option<Ty> {
        let types = tuple_ast
            .iter()
            .map(|&ty_ast| self.ty(ty_ast))
            .nsc_collect::<Option<BumpVec<_>>>()?;

        match (types.as_slice(), tuple_ast.deref()) {
            ([], _) => Some(Ty::UNIT),
            (&[ty], [ty_ast]) if ty_ast.after_delim.is_none() => Some(ty),
            _ => todo!(),
        }
    }

    pub fn spec_sum<'a>(
        &mut self,
        specs: impl Iterator<Item = &'a SpecExprAst<'a>>,
    ) -> Option<VSlice<Spec>> {
        let specs = specs
            .map(|&ast| self.spec(ast))
            .nsc_collect::<Option<BumpVec<_>>>()?;
        Some(self.typec.spec_sums.bump(specs))
    }

    pub fn spec(&mut self, spec_ast: SpecExprAst) -> Option<Spec> {
        match spec_ast {
            SpecExprAst::Path(ident) => match self.ty_path(ident)? {
                TyPathResult::Ty(..) => todo!(),
                TyPathResult::Spec(spec) => Some(Spec::Base(spec)),
            },
            SpecExprAst::Instance(instance) => self.spec_instance(instance).map(Spec::Instance),
        }
    }

    fn spec_instance(
        &mut self,
        TyInstanceAst { path, params }: TyInstanceAst,
    ) -> Option<VRef<SpecInstance>> {
        let base = match self.ty_path(path)? {
            TyPathResult::Ty(..) => todo!(),
            TyPathResult::Spec(base) => base,
        };

        let args = params
            .iter()
            .map(|&p| self.ty(p))
            .nsc_collect::<Option<BumpVec<_>>>()?;

        Some(
            self.typec
                .spec_instance(base, args.as_slice(), self.interner),
        )
    }

    fn pointer(
        &mut self,
        TyPointerAst { mutability, ty, .. }: TyPointerAst,
    ) -> Option<VRef<Pointer>> {
        let base = self.ty(ty)?;
        let mutability = self.mutability(mutability)?;
        Some(self.typec.pointer_to(mutability, base, self.interner))
    }

    fn mutability(&mut self, mutability_ast: MutabilityAst) -> Option<Mutability> {
        Some(match mutability_ast {
            MutabilityAst::Mut(..) => Mutability::Mutable,
            MutabilityAst::None => Mutability::Immutable,
            MutabilityAst::Generic(..) => todo!(),
        })
    }

    fn instance(
        &mut self,
        TyInstanceAst {
            path: ident,
            params,
        }: TyInstanceAst,
    ) -> Option<VRef<Instance>> {
        let base = match self.ty_path(ident)? {
            TyPathResult::Ty(base) => base
                .as_generic()
                .or_else(|| todo!("{} {base:?}", span_str!(self, ident.span())))?,
            TyPathResult::Spec(..) => todo!(),
        };

        let args = params
            .iter()
            .map(|&p| self.ty(p))
            .nsc_collect::<Option<BumpVec<_>>>()?;

        Some(self.typec.instance(base, args.as_slice(), self.interner))
    }

    pub fn ty_path(
        &mut self,
        path @ PathExprAst {
            start, segments, ..
        }: PathExprAst,
    ) -> Option<TyPathResult> {
        let item = match self.lookup(start.ident, start.span, TY_OR_MOD)? {
            ScopeItem::Module(module) => {
                let Some(ty) = segments.first() else {
                    self.invalid_ty_path(path);
                    return None;
                };
                let id = self.interner.intern_scoped(module.index(), ty.ident);
                self.lookup(id, path.span(), TY_OR_MOD)?
            }
            item => item,
        };

        Some(match item {
            ScopeItem::Ty(ty) => TyPathResult::Ty(ty),
            ScopeItem::SpecBase(spec) => TyPathResult::Spec(spec),
            item => self.invalid_symbol_type(item, start.span, TY_OR_MOD)?,
        })
    }

    pub fn lookup(&mut self, sym: VRef<str>, span: Span, what: &str) -> Option<ScopeItem> {
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

        for (key, &func) in self.typec.spec_funcs.indexed(functions) {
            let id = self
                .interner
                .intern_scoped(Ty::Param(index as u16), func.name);
            self.scope.push(id, key, func.span.unwrap_or_default());
        }
    }

    gen_error_fns! {
        push invalid_ty_path(self, segment: PathExprAst) {
            err: "invalid type path composition";
            info: "valid forms: `Ty` | `mod\\Ty`";
            (segment.span(), self.source) {
                info[segment.span()]: "malformed path encountered here";
            }
        }

        push invalid_symbol_type(self, item: ScopeItem, span: Span, what: &str) {
            err: "invalid symbol type";
            info: ("expected {} but got {}", what, item.what());
            (span, self.source) {
                info[span]: "symbol used here";
            }
        }
    }

    pub fn scope_error(
        &mut self,
        err: ScopeError,
        sym: VRef<str>,
        span: Span,
        what: &str,
    ) -> Option<!> {
        self.workspace.push(match err {
            ScopeError::NotFound => snippet! {
                err: ("{} not found", what);
                info: ("queried '{}'", &self.interner[sym]);
                (span, self.source) {
                    err[span]: "this does not exist";
                }
            },
            ScopeError::Collision => {
                let suggestions = self.resources.module_deps
                    [self.resources.modules[self.module].deps]
                    .iter()
                    .filter(|dep| {
                        self.typec.module_items[dep.ptr]
                            .values()
                            .any(|i| i.id == sym)
                    })
                    .map(|dep| &self.interner[dep.name])
                    .intersperse(", ")
                    .collect::<String>();

                snippet! {
                    err: ("'{}' is ambiguous", &self.interner[sym]);
                    help: ("try to specify module from which the item is imported");
                    help: ("suggestions: {}", suggestions);
                    help: ("syntax for specifying module: `<mod>\\<item>`");
                    help: ("syntax for specifying method module: `<expr>.<mod>\\<item>`");
                    (span, self.source) {
                        err[span]: "this is ambiguous";
                    }
                }
            }
        });

        None
    }
}

pub enum TyPathResult {
    Ty(Ty),
    Spec(VRef<SpecBase>),
}

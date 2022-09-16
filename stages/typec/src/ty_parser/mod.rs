use std::{
    any::{Any, TypeId},
    default::default,
};

use diags::*;
use lexing_t::Span;
use parsing::*;
use parsing_t::*;
use scope::*;
use storage::*;
use typec_shared::*;
use typec_t::*;

use crate::*;

impl TyParser<'_> {
    pub fn generics(&mut self, generic_ast: GenericsAst) -> VRefSlice<Bound> {
        let mut generics = bumpvec!(cap generic_ast.len());
        for &GenericParamAst { bounds, .. } in generic_ast.iter() {
            let bound = self.bound_sum(bounds.iter()).unwrap_or_default();
            generics.push(bound);
        }
        self.typec.bound_slices.bump(generics)
    }

    pub fn insert_generics(&mut self, generics_ast: GenericsAst, offset: usize, on_type: bool) {
        for (i, &GenericParamAst { name, .. }) in generics_ast.iter().enumerate() {
            self.insert_param(offset + i, name, on_type)
        }
    }

    fn insert_param(&mut self, index: usize, name: NameAst, on_type: bool) {
        let param = ty_utils!(self).nth_param(index, on_type);
        self.scope.push(ScopeItem::new(
            name.ident,
            param,
            name.span,
            name.span,
            self.current_file,
            Vis::Priv,
        ));
    }

    pub fn ty(&mut self, ty_ast: TyAst) -> errors::Result<VRef<Ty>> {
        match ty_ast {
            TyAst::Ident(ident) => self.ident::<TyLookup>(ident),
            TyAst::Instance(instance) => self.instance(instance),
            TyAst::Pointer(pointer) => self.pointer(*pointer),
        }
    }

    pub fn bound_sum<'a>(
        &mut self,
        bounds: impl Iterator<Item = &'a BoundExprAst<'a>>,
    ) -> errors::Result<VRef<Bound>> {
        let mut bounds = bounds
            .map(|&ast| self.bound(ast))
            .nsc_collect::<errors::Result<BumpVec<_>>>()?;
        bounds.sort_unstable_by_key(|b| b.index());

        let segments = self.typec.bound_sum_id(&bounds);
        let key = self.interner.intern(segments);
        let inherits = self.typec.bound_slices.bump(bounds);

        let fallback = |_: &mut Bounds| Bound {
            kind: BoundBase {
                inherits,
                ..default()
            }
            .into(),
            ..default()
        };

        Ok(self.typec.bounds.get_or_insert(key, fallback))
    }

    pub fn bound(&mut self, bound_ast: BoundExprAst) -> errors::Result<VRef<Bound>> {
        match bound_ast {
            BoundExprAst::Ident(ident) => self.ident::<BoundLookup>(ident),
        }
    }

    fn pointer(
        &mut self,
        TyPointerAst { mutability, ty, .. }: TyPointerAst,
    ) -> errors::Result<VRef<Ty>> {
        let base = self.ty(ty)?;
        let mutability = self.mutability(mutability)?;
        let depth = self
            .typec
            .types
            .try_inner::<TyPointer>(base)
            .map_or(0, |p| p.depth)
            + 1;
        let segments = self.typec.pointer_id(mutability, base);
        let id = self.interner.intern(segments);

        let fallback = |s: &mut Types| Ty {
            kind: TyPointer {
                base,
                mutability,
                depth: depth as u32,
            }
            .into(),
            flags: TyFlags::GENERIC & (s.is_generic(base) | s.is_generic(mutability)),
            loc: s.locate(base),
        };

        Ok(self.typec.types.get_or_insert(id, fallback))
    }

    fn mutability(&mut self, mutability_ast: MutabilityAst) -> errors::Result<VRef<Ty>> {
        Ok(match mutability_ast {
            MutabilityAst::Mut(..) => Ty::MUTABLE,
            MutabilityAst::None => Ty::IMMUTABLE,
            MutabilityAst::Ident(.., ident) => return self.ident::<TyLookup>(ident),
        })
    }

    fn ident<T: ScopeLookup>(
        &mut self,
        ident_ast: IdentChainAst,
    ) -> errors::Result<VRef<T::Output>> {
        let ident = self.intern_ident(ident_ast);
        self.lookup_typed::<T>(ident, ident_ast.span())
    }

    fn instance(
        &mut self,
        TyInstanceAst { ident, params }: TyInstanceAst,
    ) -> errors::Result<VRef<Ty>> {
        let base = self.ident::<TyLookup>(ident)?;

        let args = params
            .iter()
            .map(|&p| self.ty(p))
            .nsc_collect::<errors::Result<BumpVec<_>>>()?;

        let generic = args.iter().any(|&arg| self.typec.types.is_generic(arg));
        let segments = self.typec.instance_id(base, &args);
        let key = self.interner.intern(segments);

        let args = self.typec.ty_slices.bump(args);

        let fallback = |s: &mut Types| Ty {
            kind: TyInstance { base, args }.into(),
            flags: TyFlags::GENERIC & generic,
            loc: s.locate(base),
        };

        Ok(self.typec.types.get_or_insert(key, fallback))
    }

    fn intern_ident(&mut self, ident: IdentChainAst) -> Ident {
        todo!();
    }

    pub fn lookup_typed<T: ScopeLookup>(
        &mut self,
        sym: Ident,
        span: Span,
    ) -> errors::Result<VRef<T::Output>> {
        match self.scope.get_typed::<T::Output>(sym) {
            Ok((key, _)) => Ok(key),
            Err(err) => Err(self.handle_scope_error::<T>(err, sym, span)),
        }
    }

    pub fn lookup<T: ScopeLookup>(&mut self, sym: Ident, span: Span) -> errors::Result<ScopePtr> {
        match self.scope.get(sym) {
            Ok(key) => Ok(key.ptr),
            Err(err) => Err(self.handle_scope_error::<T>(err, sym, span)),
        }
    }

    pub fn handle_scope_error<T: ScopeLookup>(&mut self, err: ScopeError, sym: Ident, span: Span) {
        self.workspace.push(match err {
            ScopeError::NotFound => snippet! {
                err: ("{} not found", T::ITEM_NAME);
                info: ("queried '{}'", &self.interner[sym]);
                (span, self.current_file) {
                    err[span]: "this does not exist";
                }
            },
            ScopeError::Collision => {
                let suggestions = self
                    .packages
                    .modules
                    .get(&self.current_file)
                    .map(|m| &self.packages.conns[m.deps])
                    .unwrap()
                    .iter()
                    .filter_map(|dep| {
                        let ident = dep.name;
                        self.scope
                            .get_typed::<T::Output>(ident)
                            .ok()
                            .and(Some(ident))
                    })
                    .collect::<BumpVec<_>>()
                    .into_iter()
                    .map(|ident| &self.interner[ident])
                    .collect::<BumpVec<_>>()
                    .join(", ");

                snippet! {
                    err: ("'{}' is ambiguous", &self.interner[sym]);
                    help: ("try to specify module from which the item is imported");
                    help: ("suggestions: {}", suggestions);
                    (span, self.current_file) {
                        err[span]: "this is ambiguous";
                    }
                }
            }
            ScopeError::TypeMismatch(found) => snippet! {
                err: ("'{}' is not a {}", &self.interner[sym], T::ITEM_NAME);
                info: ("found type: {}", T::project(found).unwrap_or("todo: unknown"));
                (span, self.current_file) {
                    err[span]: "this is of incorrect type";
                }
            },
        });
    }
}

pub trait ScopeLookup {
    type Output: Any = ();

    const ITEM_NAME: &'static str;
    const TYPE_MISMATCH_MAPPING: &'static [(TypeId, &'static str)];

    fn project(id: TypeId) -> Option<&'static str> {
        Self::TYPE_MISMATCH_MAPPING
            .iter()
            .find_map(|&(oid, name)| (id == oid).then_some(name))
    }
}

gen_scope_lookup! {
    TyLookup<"type", Ty> {
        Bound => "bound",
    }
    BoundLookup<"bound", Bound> {
        Ty => "type",
    }
}

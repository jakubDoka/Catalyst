use std::{
    any::{Any, TypeId},
    default::default,
};

use diags::*;
use lexing_t::Span;
use packaging_t::span_str;
use parsing_t::*;
use scope::*;
use storage::*;
use typec_shared::*;
use typec_t::*;

use crate::*;

impl TyParser<'_> {
    pub fn generics(
        &mut self,
        generic_ast: Ast,
        offset: usize,
        on_type: bool,
        insert_params: bool,
    ) -> VRefSlice<Bound> {
        let mut generics = bumpvec!(cap self.ast_data[generic_ast.children].len());
        for (i, &ast) in self.ast_data[generic_ast.children].iter().enumerate() {
            let [ast_name, ref bounds @ ..] = self.ast_data[ast.children] else {
                unreachable!();
            };

            if insert_params {
                let name = span_str!(self, ast_name.span);
                let name_ident = self.interner.intern_str(name);
                let param = ty_utils!(self).nth_param(i + offset, on_type);
                self.scope.push(ScopeItem::new(
                    name_ident,
                    param,
                    ast_name.span,
                    ast_name.span,
                    self.current_file,
                    Vis::Priv,
                ));
            }

            let bound = self.bound_sum(bounds).unwrap_or_default();
            generics.push(bound);
        }

        self.typec.bound_slices.bump(generics)
    }

    pub fn ty(&mut self, ty_ast: Ast) -> errors::Result<VRef<Ty>> {
        match ty_ast.kind {
            AstKind::Ident | AstKind::IdentChain => self.ident::<TyLookup>(ty_ast),
            AstKind::TyInstance => self.instance(ty_ast),
            AstKind::PointerTy => self.pointer(ty_ast),
            kind => unimplemented!("{:?}", kind),
        }
    }

    pub fn bound_sum(&mut self, bounds: &[Ast]) -> errors::Result<VRef<Bound>> {
        let bounds = bounds
            .iter()
            .map(|&ast| self.bound(ast))
            .nsc_collect::<errors::Result<BumpVec<_>>>()?;

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

    pub fn bound(&mut self, bound_ast: Ast) -> errors::Result<VRef<Bound>> {
        match bound_ast.kind {
            AstKind::Ident | AstKind::IdentChain => self.ident::<BoundLookup>(bound_ast),
            kind => unimplemented!("{:?}", kind),
        }
    }

    fn pointer(&mut self, pointer_ast: Ast) -> errors::Result<VRef<Ty>> {
        let [mutability, base] = self.ast_data[pointer_ast.children] else {
            unreachable!();
        };

        let base = self.ty(base)?;
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

    fn mutability(&mut self, mutability_ast: Ast) -> errors::Result<VRef<Ty>> {
        Ok(match mutability_ast.kind {
            AstKind::PointerMut => Ty::MUTABLE,
            AstKind::None => Ty::IMMUTABLE,
            _ => return self.ty(mutability_ast),
        })
    }

    fn ident<T: ScopeLookup>(&mut self, ident_ast: Ast) -> errors::Result<VRef<T::Output>> {
        let ident = self.intern_ident(ident_ast);
        self.lookup_typed::<T>(ident, ident_ast.span)
    }

    fn instance(&mut self, instance_ast: Ast) -> errors::Result<VRef<Ty>> {
        let [base, ref args @ ..] = self.ast_data[instance_ast.children] else {
            unreachable!();
        };

        let base = self.ident::<TyLookup>(base)?;

        let args = args
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

    fn intern_ident(&mut self, ident: Ast) -> Ident {
        match ident.kind {
            AstKind::Ident => self
                .scope
                .project(span_str!(self, ident.span))
                .unwrap_or_else(|| self.interner.intern_str(span_str!(self, ident.span))),
            AstKind::IdentChain => self.interner.intern(ident_join(
                "`",
                self.ast_data[ident.children].iter().copied().map(|i| {
                    self.scope
                        .project(span_str!(self, i.span))
                        .map(InternedSegment::Ident)
                        .unwrap_or_else(|| InternedSegment::String(span_str!(self, i.span)))
                }),
            )),
            kind => unimplemented!("{:?}", kind),
        }
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
            ScopeError::NotFound => sippet! {
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
                        let name = self.interner.intern_str(span_str!(self, dep.name));
                        let ident = self.interner.intern(scoped_ident!(name, sym));
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

                sippet! {
                    err: ("'{}' is ambiguous", &self.interner[sym]);
                    help: ("try to specify module from which the item is imported");
                    help: ("suggestions: {}", suggestions);
                    (span, self.current_file) {
                        err[span]: "this is ambiguous";
                    }
                }
            }
            ScopeError::TypeMismatch(found) => sippet! {
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

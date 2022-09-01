mod utils;

use std::any::{Any, TypeId};

use diags::diag;
use lexing_t::Span;
use packaging_t::span_str;
use parsing_t::*;
use scope::*;
use storage::*;
use typec_t::*;

use crate::*;

impl TyParser<'_> {
    pub fn ty(&mut self, ty_ast: Ast) -> errors::Result<VRef<Ty>> {
        match ty_ast.kind {
            AstKind::Ident | AstKind::IdentChain => self.ident::<TyLookup>(ty_ast),
            AstKind::TyInstance => self.instance(ty_ast),
            AstKind::PointerTy => self.pointer(ty_ast),
            kind => unimplemented!("{:?}", kind),
        }
    }

    pub fn bound(&mut self, bound_ast: Ast) -> errors::Result<VRef<Bound>> {
        match bound_ast.kind {
            AstKind::Ident | AstKind::IdentChain => self.ident::<BoundLookup>(bound_ast),
            AstKind::BoundInstance | AstKind::TyInstance => self.bound_instance(bound_ast),
            kind => unimplemented!("{:?}", kind),
        }
    }

    fn bound_instance(&mut self, instance_ast: Ast) -> errors::Result<VRef<Bound>> {
        let [base, ref param_ast @ ..] = self.ast_data[instance_ast.children] else {
            unreachable!();
        };

        let base = self.ident::<BoundLookup>(base)?;

        let params = param_ast
            .iter()
            .map(|&param| self.ty(param))
            .nsc_collect::<errors::Result<BumpVec<_>>>()?;

        let generic = params
            .iter()
            .any(|&param| self.typec.types.is_generic(param));
        let params = self.typec.ty_slices.bump(params);

        todo!();
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
            ScopeError::NotFound => diag! {
                (span, self.current_file) => "{} not found (queried '{}')" {
                    T::ITEM_NAME,
                    &self.interner[sym],
                },
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
                    .collect::<Vec<_>>()
                    .into_iter()
                    .map(|ident| &self.interner[ident])
                    .collect::<Vec<_>>()
                    .join(", ");

                diag! {
                    (span, self.current_file) => "`{}` is ambiguous" { &self.interner[sym] },
                    (none) => "you have to specify module from which the item is imported",
                    (none) => "suggestions: {}" { suggestions },
                }
            }
            ScopeError::TypeMismatch(found) => diag! {
                (span, self.current_file) => "`{}` is not a {}" {
                    &self.interner[sym],
                    T::ITEM_NAME,
                },
                (none) => "found type: {}" { T::project(found).unwrap_or("todo: unknown") },
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

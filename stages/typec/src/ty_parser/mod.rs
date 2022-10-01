use std::{
    any::{Any, TypeId},
    default::default,
    ops::Deref,
};

use diags::*;
use lexing_t::Span;
use packaging_t::Mod;
use parsing::*;
use parsing_t::*;
use scope::*;
use storage::*;

use typec_t::*;

use crate::*;

impl TyChecker<'_> {
    pub fn type_diff(&self, pattern: VRef<Ty>, value: VRef<Ty>) -> String {
        let mut buffer = String::new();
        self.type_diff_recurse(pattern, value, &mut buffer);
        buffer
    }

    fn type_diff_recurse(&self, pattern: VRef<Ty>, value: VRef<Ty>, buffer: &mut String) {
        if pattern == value {
            buffer.push('_');
            return;
        }

        match (self.typec.types[pattern].kind, self.typec.types[value].kind) {
            (TyKind::Pointer(pattern), TyKind::Pointer(value)) => {
                buffer.push('^');
                self.type_diff_recurse(pattern.mutability, value.mutability, buffer);
                if pattern.mutability != Ty::IMMUTABLE {
                    buffer.push(' ');
                }
                self.type_diff_recurse(pattern.base, value.base, buffer);
            }
            (TyKind::Instance(pattern), TyKind::Instance(value)) => {
                self.type_diff_recurse(pattern.base, value.base, buffer);
                let Some((&pattern_first, pattern_others)) = self.typec.ty_slices[pattern.args].split_first() else {
                    return;
                };
                let Some((&value_first, value_others)) = self.typec.ty_slices[value.args].split_first() else {
                    return;
                };

                buffer.push('[');
                self.type_diff_recurse(pattern_first, value_first, buffer);
                for (&pattern, &value) in pattern_others.iter().zip(value_others) {
                    buffer.push_str(", ");
                    self.type_diff_recurse(pattern, value, buffer);
                }
                buffer.push(']');
            }
            _ => buffer.push_str(&self.interner[self.typec.types.id(pattern)]),
        }
    }

    pub fn generics(&mut self, generic_ast: GenericsAst) -> VRefSlice<Bound> {
        let mut generics = bumpvec!(cap generic_ast.len());
        for &GenericParamAst { bounds, .. } in generic_ast.iter() {
            let bound = self.bound_sum(bounds.iter()).unwrap_or_default();
            generics.push(bound);
        }
        self.typec.bound_slices.bump(generics)
    }

    pub fn insert_generics(&mut self, generics_ast: GenericsAst, offset: usize) {
        for (i, &GenericParamAst { name, .. }) in generics_ast.iter().enumerate() {
            self.insert_param(offset + i, name)
        }
    }

    fn insert_param(&mut self, index: usize, name: NameAst) {
        let param = self.typec.nth_param(index, self.interner);
        self.scope.push(ScopeItem::new(
            name.ident,
            param,
            name.span,
            name.span,
            self.current_file,
            Vis::Priv,
        ));
    }

    pub fn ty(&mut self, ty_ast: TyAst) -> Option<VRef<Ty>> {
        match ty_ast {
            TyAst::Path(ident) => self.ty_path::<TyLookup>(ident),
            TyAst::Instance(instance) => self.instance(instance),
            TyAst::Pointer(pointer) => self.pointer(*pointer),
            TyAst::Tuple(tuple) => self.tuple(tuple),
        }
    }

    pub fn tuple(&mut self, tuple_ast: TyTupleAst) -> Option<VRef<Ty>> {
        let types = tuple_ast
            .iter()
            .map(|&ty_ast| self.ty(ty_ast))
            .nsc_collect::<Option<BumpVec<_>>>()?;

        match (types.as_slice(), tuple_ast.deref()) {
            ([], _) => return Some(Ty::UNIT),
            (&[ty], [ty_ast]) if ty_ast.after_delim.is_none() => return Some(ty),
            _ => (),
        }

        let id = self.interner.intern(self.typec.tuple_id(types.as_slice()));

        let tuple = |_: &mut Types| Ty {
            kind: TyStruct {
                fields: self.typec.fields.bump(types.into_iter().map(|ty| Field {
                    vis: Vis::Pub,
                    ty,
                    flags: FieldFlags::MUTABLE,
                    ..default()
                })),
                ..default()
            }
            .into(),
            ..default()
        };

        Some(self.typec.types.get_or_insert(id, tuple))
    }

    pub fn bound_sum<'a>(
        &mut self,
        bounds: impl Iterator<Item = &'a BoundExprAst<'a>>,
    ) -> Option<VRef<Bound>> {
        let mut bounds = bounds
            .map(|&ast| self.bound(ast))
            .nsc_collect::<Option<BumpVec<_>>>()?;
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

        Some(self.typec.bounds.get_or_insert(key, fallback))
    }

    pub fn bound(&mut self, bound_ast: BoundExprAst) -> Option<VRef<Bound>> {
        match bound_ast {
            BoundExprAst::Path(ident) => self.ty_path::<BoundLookup>(ident),
        }
    }

    fn pointer(&mut self, TyPointerAst { mutability, ty, .. }: TyPointerAst) -> Option<VRef<Ty>> {
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

        Some(self.typec.types.get_or_insert(id, fallback))
    }

    fn mutability(&mut self, mutability_ast: MutabilityAst) -> Option<VRef<Ty>> {
        Some(match mutability_ast {
            MutabilityAst::Mut(..) => Ty::MUTABLE,
            MutabilityAst::None => Ty::IMMUTABLE,
            MutabilityAst::Generic(.., path) => return self.ty_path::<TyLookup>(path),
        })
    }

    fn instance(&mut self, TyInstanceAst { ident, params }: TyInstanceAst) -> Option<VRef<Ty>> {
        let base = self.ty_path::<TyLookup>(ident)?;

        let args = params
            .iter()
            .map(|&p| self.ty(p))
            .nsc_collect::<Option<BumpVec<_>>>()?;

        let generic = args.iter().any(|&arg| self.typec.types.is_generic(arg));
        let segments = self.typec.instance_id(base, &args);
        let key = self.interner.intern(segments);

        let args = self.typec.ty_slices.bump(args);

        let fallback = |s: &mut Types| Ty {
            kind: TyInstance { base, args }.into(),
            flags: TyFlags::GENERIC & generic,
            loc: s.locate(base),
        };

        Some(self.typec.types.get_or_insert(key, fallback))
    }

    fn ty_path<T: ScopeLookup>(
        &mut self,
        path @ PathExprAst { start, segments }: PathExprAst,
    ) -> Option<VRef<T::Output>> {
        match *segments {
            [] => self.lookup_typed::<T>(start.ident, start.span),
            [ty] => {
                let module = self.lookup_typed::<ModLookup>(start.ident, start.span)?;
                let id = self
                    .interner
                    .intern(scoped_ident!(module.index() as u32, ty.ident));

                let Some(ty) = T::index(self.typec, id) else {
                    self.handle_scope_error::<T>(ScopeError::NotFound, ty.ident, ty.span);
                    return None;
                };

                Some(ty)
            }
            _ => {
                self.invalid_ty_path(path);
                None
            }
        }
    }

    pub fn lookup_typed<T: ScopeLookup>(
        &mut self,
        sym: VRef<str>,
        span: Span,
    ) -> Option<VRef<T::Output>> {
        match self.scope.get_typed::<T::Output>(sym) {
            Ok((key, _)) => Some(key),
            Err(err) => {
                self.handle_scope_error::<T>(err, sym, span);
                None
            }
        }
    }

    pub fn lookup<T: ScopeLookup>(&mut self, sym: VRef<str>, span: Span) -> Option<ScopePtr> {
        match self.scope.get(sym) {
            Ok(key) => Some(key.ptr),
            Err(err) => {
                self.handle_scope_error::<T>(err, sym, span);
                None
            }
        }
    }

    gen_error_fns! {
        push invalid_ty_path(self, segment: PathExprAst) {
            err: "invalid type path composition";
            info: "valid forms: `Ty` | `mod\\Ty`";
            (segment.span(), self.current_file) {
                info[segment.span()]: "malformed path encountered here";
            }
        }
    }

    pub fn handle_scope_error<T: ScopeLookup>(
        &mut self,
        err: ScopeError,
        sym: VRef<str>,
        span: Span,
    ) -> Option<!> {
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

        None
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

    fn index(_: &Typec, _: VRef<str>) -> Option<VRef<Self::Output>> {
        unimplemented!()
    }
}

gen_scope_lookup! {
    TyLookup<"type", Ty, types> {
        Bound => "bound",
    }
    BoundLookup<"bound", Bound, bounds> {
        Ty => "type",
    }
    ModLookup<"module", Mod> {
        Bound => "bound",
        Ty => "type",
    }
}

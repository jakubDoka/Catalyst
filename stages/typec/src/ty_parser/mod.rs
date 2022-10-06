use std::{default::default, iter, ops::Deref};

use diags::*;
use lexing_t::Span;
use parsing::*;
use parsing_t::*;

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

    pub fn generics(&mut self, generic_ast: GenericsAst) -> VRefSlice<Ty> {
        let mut generics = bumpvec!(cap generic_ast.len());
        for &GenericParamAst { bounds, .. } in generic_ast.iter() {
            let bound = self.spec_sum(bounds.iter()).unwrap_or_default();
            generics.push(bound);
        }
        self.typec.ty_slices.bump(generics)
    }

    pub fn insert_generics(&mut self, generics_ast: GenericsAst, offset: usize) {
        for (i, &GenericParamAst { name, .. }) in generics_ast.iter().enumerate() {
            self.insert_param(offset + i, name)
        }
    }

    fn insert_param(&mut self, index: usize, name: NameAst) {
        let param = self.typec.nth_param(index, self.interner);
        self.scope.push(name.ident, param, name.span);
    }

    pub fn ty(&mut self, ty_ast: TyAst) -> Option<VRef<Ty>> {
        match ty_ast {
            TyAst::Path(ident) => self.ty_path(ident),
            TyAst::Instance(instance) => self.instance(instance),
            TyAst::Pointer(pointer) => self.pointer(*pointer),
            TyAst::Tuple(tuple) => self.tuple(tuple),
            TyAst::Wildcard(..) => todo!(),
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

    pub fn spec_sum<'a>(&mut self, specs: impl Iterator<Item = &'a TyAst<'a>>) -> Option<VRef<Ty>> {
        let mut specs = specs
            .map(|&ast| self.ty(ast))
            .nsc_collect::<Option<BumpVec<_>>>()?;

        if specs.is_empty() {
            return Some(Ty::ANY);
        }

        specs.sort_unstable_by_key(|b| b.index());

        let segments = self.typec.bound_sum_id(&specs);
        let key = self.interner.intern(segments);
        let inherits = self.typec.ty_slices.bump(specs);

        let fallback = |_: &mut Types| Ty {
            kind: TySpec {
                inherits,
                ..default()
            }
            .into(),
            ..default()
        };

        Some(self.typec.types.get_or_insert(key, fallback))
    }

    fn pointer(&mut self, TyPointerAst { mutability, ty, .. }: TyPointerAst) -> Option<VRef<Ty>> {
        let base = self.ty(ty)?;
        let mutability = self.mutability(mutability)?;
        let depth = self.typec.types[base]
            .kind
            .try_cast::<TyPointer>()
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
            loc: s[base].loc,
        };

        Some(self.typec.types.get_or_insert(id, fallback))
    }

    fn mutability(&mut self, mutability_ast: MutabilityAst) -> Option<VRef<Ty>> {
        Some(match mutability_ast {
            MutabilityAst::Mut(..) => Ty::MUTABLE,
            MutabilityAst::None => Ty::IMMUTABLE,
            MutabilityAst::Generic(.., path) => return self.ty_path(path),
        })
    }

    fn instance(
        &mut self,
        TyInstanceAst {
            path: ident,
            params,
        }: TyInstanceAst,
    ) -> Option<VRef<Ty>> {
        let base = self.ty_path(ident)?;

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
            loc: s[base].loc,
        };

        Some(self.typec.types.get_or_insert(key, fallback))
    }

    pub fn ty_path(
        &mut self,
        path @ PathExprAst { start, segments }: PathExprAst,
    ) -> Option<VRef<Ty>> {
        let item = self.lookup(start.ident, start.span, TY_OR_MOD)?;
        match item {
            ScopeItem::Ty(ty) => Some(ty),
            ScopeItem::Module(module) => {
                let Some(ty) = segments.first() else {
                    self.invalid_ty_path(path);
                    return None;
                };
                let segments = scoped_ident!(module.as_u32(), ty.ident);
                let id = self.interner.intern(segments);
                self.typec
                    .types
                    .index(id)
                    .or_else(|| self.scope_error(ScopeError::NotFound, id, path.span(), TY)?)
            }
            item => self.invalid_symbol_type(item, start.span, TY_OR_MOD)?,
        }
    }

    pub fn lookup(&mut self, sym: VRef<str>, span: Span, what: &str) -> Option<ScopeItem> {
        self.scope
            .get(sym)
            .map_err(|err| self.scope_error(err, sym, span, what))
            .ok()
    }

    pub fn pack_func_param_specs(&self, func: VRef<Func>) -> impl Iterator<Item = VRef<Ty>> + '_ {
        let Func {
            generics,
            upper_generics,
            ..
        } = self.typec.funcs[func];
        iter::empty()
            .chain(&self.typec.ty_slices[upper_generics])
            .chain(&self.typec.ty_slices[generics])
            .copied()
    }

    pub fn pack_spec_func_param_specs(
        &self,
        func: SpecFunc,
    ) -> impl Iterator<Item = VRef<Ty>> + '_ {
        let SpecFunc {
            generics, parent, ..
        } = func;
        let TySpec {
            generics: upper_generics,
            ..
        } = self.typec.types[parent].kind.cast::<TySpec>();
        iter::empty()
            .chain(self.typec.ty_slices[upper_generics].iter().copied())
            .chain(iter::once(parent))
            .chain(self.typec.ty_slices[generics].iter().copied())
    }

    pub fn insert_spec_functions(&mut self, generics: VRefSlice<Ty>, offset: usize) {
        for (i, generic) in self.typec.ty_slices[generics]
            .to_bumpvec()
            .into_iter()
            .enumerate()
        {
            let index = i + offset;
            let param = self.typec.nth_param(index, self.interner);
            let id = self.typec.types.id(param);
            self.insert_spec_functions_recur(id, generic);
        }
    }

    fn insert_spec_functions_recur(&mut self, id: VRef<str>, generic: VRef<Ty>) {
        let spec_base = self.typec.types.base(generic);
        let spec = self.typec.types[spec_base].kind.cast::<TySpec>();

        for super_spec in self.typec.ty_slices[spec.inherits].to_bumpvec() {
            self.insert_spec_functions_recur(id, super_spec);
        }

        for (key, &func) in self.typec.spec_funcs.indexed(spec.methods) {
            let id = self.interner.intern(scoped_ident!(id, func.name));
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

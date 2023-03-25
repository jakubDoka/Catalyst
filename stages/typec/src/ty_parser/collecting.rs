use crate::ctx::TypecTransfer;

use super::TypecParser;

use {
    ast::*,
    diags::*,
    span::*,
    std::{default::default, iter},
    storage::*,
    types::*,
};

#[allow(clippy::type_complexity)]
impl<'arena, 'ctx> TypecParser<'arena, 'ctx> {
    pub(super) fn collect<T: CollectGroup<'arena>>(
        &mut self,
        items: GroupedItemSlice<T>,
        collector: fn(&mut Self, T, &[TopLevelAttrAst]) -> Option<FragRef<T::Output>>,
    ) -> &mut Self {
        for &(item_ast, attributes) in items.iter() {
            let Some(id) = collector(self, item_ast, attributes) else {
                continue;
            };

            T::output(self.ext.transfer).push((item_ast, id));
        }

        self
    }

    pub(super) fn collect_impls(&mut self, items: GroupedItemSlice<ImplAst<'arena>>) -> &mut Self {
        for &(item, attributes) in items.iter() {
            self.collect_impl(item, attributes);
        }

        self
    }

    fn collect_impl(
        &mut self,
        r#impl @ ImplAst {
            target,
            vis,
            generics,
            body,
            ..
        }: ImplAst<'arena>,
        _: &[TopLevelAttrAst],
    ) -> Option<()> {
        let frame = self.ctx.start_frame();
        let mut spec_set = SpecSet::default();
        let mut params = TyParamIter::default();

        self.ctx.insert_generics(generics, params);
        self.generics(generics, &mut spec_set, &mut params);

        let (parsed_ty, parsed_spec) = match target {
            ImplTargetAst::Direct(ty) => (self.ty(ty)?, None),
            ImplTargetAst::Spec(spec, .., ty) => (self.ty(ty)?, Some(self.spec(spec)?)),
        };

        let spec_set_frame = spec_set.start_frame();
        self.ext
            .types
            .register_ty_generics(parsed_ty, &mut spec_set);
        if let Some(parsed_spec) = parsed_spec {
            self.ext
                .types
                .register_spec_generics(parsed_spec, &mut spec_set);
        }

        self.ctx.push_self(parsed_ty, target.span());

        let scope_data = ScopeData {
            params,
            owner: Some(parsed_ty),
            not_in_scope: parsed_spec.is_some(),
            upper_vis: vis.map(|vis| vis.vis),
        };

        let explicit_methods = body
            .map(|body| {
                body.iter()
                    .filter_map(|&func| {
                        let ImplItemAst::Func(&func) = func;

                        let id = self.collect_func_low(func, &[], &mut spec_set, scope_data)?;

                        self.ext.transfer.impl_funcs.push((func, id));
                        Some((id, func.signature.name.span))
                    })
                    .collect::<BumpVec<_>>()
            })
            .unwrap_or_default();

        let parsed_generics = self.take_generics(spec_set_frame, &mut spec_set);

        let impl_id = parsed_spec
            .map(|spec| {
                self.handle_spec_impl(
                    spec,
                    parsed_ty,
                    target.span(),
                    parsed_generics,
                    explicit_methods,
                )
            })
            .transpose()?;

        self.ext.transfer.close_impl_frame(r#impl, impl_id);

        self.ctx.end_frame(frame);

        Some(())
    }

    fn handle_spec_impl(
        &mut self,
        spec: Spec,
        ty: Ty,
        span: Span,
        generics: WhereClause,
        explicit_methods: BumpVec<(FragRef<Func>, Span)>,
    ) -> OptFragRef<Impl> {
        let spec_base = spec.base(self.ext.types);
        let ty_base = ty.base(self.ext.types);

        if SpecBase::DROP == spec_base {
            self.ext.types.may_need_drop.insert(ty_base, true);
        }

        // check for collisions in implementations,
        // we just check if spec is already implemented
        if let Some(Some((already, ..))) =
            self.ext
                .creator()
                .find_implementation(ty, spec, generics.predicates, &mut None)
        {
            CollidingImpl {
                colliding: self.meta.loc(span),
                existing: self.ext.types[already].loc.source_loc(self.ext.types),
                ty: self.ext.creator().display(ty),
                spec: self.ext.creator().display(spec),
            }
            .add(self.ext.workspace);
        }

        if spec_base == SpecBase::DROP {
            self.ext
                .types
                .may_need_drop
                .insert(ty.base(self.ext.types), true);
        }

        let methods =
            self.collect_spec_impl_methods(ty, ty_base, spec_base, explicit_methods, span)?;

        let loc = {
            let next = self.ext.types.cache.impls.next();
            let item = ModuleItem::new(Ident::default(), next, span, None);
            self.meta.item_loc(self.ext.types, item)
        };

        let impl_ent = self.ext.types.cache.impls.push(Impl {
            generics,
            key: ImplKey { ty, spec },
            methods,
            loc,
        });
        self.ext
            .types
            .impl_lookup
            .entry((spec_base, ty_base))
            .or_default()
            .inner
            .push(impl_ent);
        Some(impl_ent)
    }

    fn collect_spec_impl_methods(
        &mut self,
        ty: Ty,
        ty_base: Ty,
        spec_base: FragRef<SpecBase>,
        explicit_methods: BumpVec<(FragRef<Func>, Span)>,
        span: Span,
    ) -> Option<FragRefSlice<Func>> {
        let spec_methods = self.ext.types[spec_base].methods;
        let mut slots = bumpvec![None; spec_methods.len()];
        for (method, span) in explicit_methods {
            let method_name = self.ext.types[method].name;
            let Some((i, &spec_method)) = self.ext.types[spec_methods]
                    .iter()
                    .enumerate()
                    .find(|(_, m)| m.name == method_name)
                    else {continue};

            if let Err(err) = self.ext.check_impl_signature(ty, spec_method, method, true) {
                let spec_source_loc = self.ext.types[spec_base].loc.source_loc(self.ext.types);
                self.ext.handle_signature_check_error(
                    err,
                    span,
                    spec_method.span,
                    spec_source_loc,
                    &self.meta,
                );
                continue;
            }

            slots[i] = Some(method);
        }

        let iter = slots
            .iter_mut()
            .zip(spec_methods.keys())
            .filter(|(slot, ..)| slot.is_none());
        for (slot, i) in iter {
            let spec_func = self.ext.types[i];
            let id = self.ext.interner.intern_scoped(ty_base, spec_func.name);

            let Ok(ScopeItem::Func(id)) = self.ctx.try_lookup(id) else {
                        continue;
                    };

            if self
                .ext
                .check_impl_signature(ty, spec_func, id, false)
                .is_err()
            {
                continue;
            }

            *slot = Some(id);
        }

        let Some(methods) = slots.iter().copied().collect::<Option<BumpVec<_>>>() else {
                let missing = self.ext.types[spec_methods]
                    .iter()
                    .zip(slots.as_slice())
                    .filter_map(|(f, m)| m.is_none().then_some(&f.name))
                    .map(|name| name.get(self.ext.interner))
                    .intersperse(", ")
                    .collect::<String>();
                MissingImplMethods {
                    loc: self.meta.loc(span),
                    missing,
                }.add(self.ext.workspace)?;
            };

        Some(self.ext.types.cache.func_slices.extend(methods))
    }

    pub(super) fn collect_spec(
        &mut self,
        SpecAst { vis, name, .. }: SpecAst,
        attributes: &[TopLevelAttrAst],
    ) -> Option<FragRef<SpecBase>> {
        let meta = self.next_humid_item_id::<SpecBase>(name, attributes);
        let item = ModuleItem::new(name.ident, meta.id, name.span, vis.map(|v| v.vis));
        let loc = self
            .ctx
            .insert_scope_item(item, &mut self.ext, &self.meta)?;
        let spec = SpecBase {
            name: name.ident,
            loc: loc.into(),
            ..default()
        };
        Some(self.insert_humid_item(spec, meta))
    }

    pub(super) fn collect_func(
        &mut self,
        func: FuncDefAst,
        attributes: &[TopLevelAttrAst],
    ) -> Option<FragRef<Func>> {
        self.collect_func_low(func, attributes, &mut default(), default())
    }

    fn collect_func_low(
        &mut self,
        FuncDefAst {
            vis,
            signature: sig @ FuncSigAst { cc, name, .. },
            body,
            ..
        }: FuncDefAst,
        attributes: &[TopLevelAttrAst],
        spec_set: &mut SpecSet,
        ScopeData {
            params,
            not_in_scope,
            owner,
            upper_vis,
        }: ScopeData,
    ) -> Option<FragRef<Func>> {
        let outer_param_count = params.progress() as u16;
        let (signature, generics) = self.collect_signature(sig, spec_set, params)?;

        let entry = attributes
            .iter()
            .find(|attr| matches!(attr.value.value, TopLevelAttrKindAst::Entry(..)));
        let no_moves = attributes
            .iter()
            .find(|attr| matches!(attr.value.value, TopLevelAttrKindAst::NoMoves(..)));

        if let Some(entry) = entry && let Some(generics) = sig.generics {
            GenericEntry {
                loc: self.meta.loc(name.span),
                entry: entry.span(),
                generics: generics.span(),
            }.add(self.ext.workspace)?;
        }

        let visibility = if let FuncBodyAst::Extern(body) = body {
            if let Some(generics) = sig.generics {
                GenericExtern {
                    loc: self.meta.loc(name.span),
                    generics: generics.span(),
                    body: body.span,
                }
                .add(self.ext.workspace)?;
            }

            FuncVisibility::Imported
        } else if cc.is_some() {
            FuncVisibility::Exported
        } else {
            FuncVisibility::Local
        };

        let (loc, meta) = if not_in_scope {
            (Loc::new(self.meta.source(), None), None)
        } else {
            let meta = self.next_humid_item_id::<Func>(name, attributes);
            let local_id = owner.map_or(name.ident, |owner| {
                self.ext
                    .interner
                    .intern_scoped(owner.caller(self.ext.types), name.ident)
            });
            let item = ModuleItem::new(
                local_id,
                meta.id,
                name.span,
                vis.map(|vis| vis.vis).or(upper_vis),
            );
            let (loc, meta) = self
                .ctx
                .insert_scope_item(item, &mut self.ext, &self.meta)
                .map(|id| (id, meta))?;
            (loc.into(), Some(meta))
        };

        let func = Func {
            generics,
            outer_param_count,
            owner,
            signature,
            flags: (FuncFlags::ENTRY & entry.is_some())
                | (FuncFlags::NO_MOVES & no_moves.is_some()),
            visibility,
            name: name.ident,
            loc,
        };
        Some(if let Some(meta) = meta {
            self.insert_humid_item(func, meta)
        } else {
            self.ext.types.cache.funcs.push(func)
        })
    }

    pub(super) fn collect_signature(
        &mut self,
        FuncSigAst {
            cc,
            generics,
            args,
            ret,
            ..
        }: FuncSigAst,
        spec_set: &mut SpecSet,
        params: TyParamIter,
    ) -> Option<(Signature, WhereClause)> {
        let frame = self.ctx.start_frame();

        self.ctx.insert_generics(generics, params);
        let args = args
            .iter()
            .flat_map(|i| i.iter())
            .map(|arg| self.ty(arg.ty))
            .nsc_collect::<Option<BumpVec<_>>>()?;
        let ret = ret
            .map(|(.., ret)| self.ty(ret))
            .unwrap_or(Some(Ty::UNIT))?;

        let spec_set_frame = spec_set.start_frame();
        for ty in args.iter().copied().chain(iter::once(ret)) {
            self.ext.types.register_ty_generics(ty, spec_set)
        }

        let signature = Signature {
            cc: cc.map(|cc| self.ext.interner.intern(self.span_str(cc.span.shifted(1)))),
            args: self.ext.types.cache.args.extend(args),
            ret,
        };
        self.generics(generics, spec_set, params);

        self.ctx.end_frame(frame);

        let generics = self.take_generics(spec_set_frame, spec_set);
        Some((signature, generics))
    }

    pub(super) fn take_generics(
        &mut self,
        frame: SpecSetFrame,
        spec_set: &mut SpecSet,
    ) -> WhereClause {
        let params = spec_set
            .iter()
            .map(|group| WherePredicate {
                ty: Some(TyParam::new(group.index, group.asoc_ty).into()),
                bounds: self.ext.creator().spec_sum(group.specs()),
            })
            .collect::<BumpVec<_>>();
        spec_set.end_frame(frame);
        let len = params.iter().take_while(|p| p.ty.is_none()).count();
        WhereClause::new(params, len, self.ext.types).expect("todo: make error message about this")
    }

    pub(super) fn collect_struct(
        &mut self,
        StructAst {
            vis,
            name,
            generics,
            ..
        }: StructAst,
        attributes: &[TopLevelAttrAst],
    ) -> Option<FragRef<Struct>> {
        let (loc, meta) = self.humid_item_loc(name, attributes, vis, Some(generics))?;
        let s = Struct {
            name: name.ident,
            loc: loc.into(),
            ..default()
        };
        Some(self.insert_humid_item(s, meta))
    }

    pub(super) fn collect_const(
        &mut self,
        ConstAst {
            vis,
            name,
            ty,
            value,
            ..
        }: ConstAst,
        _: &[TopLevelAttrAst],
    ) -> Option<FragRef<Const>> {
        let loc = {
            let id = self.ext.types.cache.consts.next();
            let item = ModuleItem::new(name.ident, id, name.span, vis.map(|vis| vis.vis));
            self.ctx
                .insert_scope_item(item, &mut self.ext, &self.meta)?
        };

        // since we inserted const into scope we have to recover here
        let ty = ty.and_then(|(.., ty)| self.ty(ty));
        let (value, ty) = self.const_fold(ty, value).unwrap_or_default();
        let r#const = Const {
            name: name.ident,
            ty,
            value,
            loc: loc.into(),
        };

        Some(self.ext.types.cache.consts.push(r#const))
    }

    pub(super) fn collect_enum(
        &mut self,
        EnumAst {
            vis,
            name,
            generics,
            ..
        }: EnumAst,
        attributes: &[TopLevelAttrAst],
    ) -> Option<FragRef<Enum>> {
        let (loc, meta) = self.humid_item_loc(name, attributes, vis, Some(generics))?;
        let e = Enum {
            name: name.ident,
            loc: loc.into(),
            ..default()
        };
        Some(self.insert_humid_item(e, meta))
    }

    fn humid_item_loc<T: Humid>(
        &mut self,
        name: NameAst,
        attributes: &[TopLevelAttrAst],
        vis: Option<VisAst>,
        check_macro: Option<Option<ListAst<ParamAst>>>,
    ) -> Option<(GuaranteedLoc, HumidMeta<T>)>
    where
        FragRef<T>: Into<Ty>,
    {
        let meta = self.next_humid_item_id(name, attributes);
        let ty = meta.id.into();
        let item = ModuleItem::new(name.ident, ty, name.span, vis.map(|vis| vis.vis));
        if let Some(generics) = check_macro {
            self.handle_macro_attr(attributes, ty, generics.map(|generics| generics.span()));
        }
        self.ctx
            .insert_scope_item(item, &mut self.ext, &self.meta)
            .map(|id| (id, meta))
    }

    fn next_humid_item_id<I: Humid>(
        &mut self,
        name: NameAst,
        attributes: &[TopLevelAttrAst],
    ) -> HumidMeta<I> {
        if let Some(attr) = attributes
            .iter()
            .find(|attr| matches!(attr.value.value, TopLevelAttrKindAst::WaterDrop(..)))
        {
            let name_str = name.ident.get(self.ext.interner);
            let Some(id) = I::lookup_water_drop(name_str) else {
                InvalidWaterDrop {
                    message: "no water drop with this name exists",
                    name: name.span,
                    loc: self.meta.loc(attr.span()),
                    suggestions: I::NAMES,
                }.add(self.ext.workspace);

                return HumidMeta {
                    id: I::storage(self.ext.types).next(),
                    humid: false,
                };
            };

            if I::storage(self.ext.types)[id].name() != Interner::EMPTY {
                InvalidWaterDrop {
                    message: "water drop already exists",
                    name: name.span,
                    loc: self.meta.loc(attr.span()),
                    suggestions: I::NAMES,
                }
                .add(self.ext.workspace);
            }

            HumidMeta { id, humid: true }
        } else {
            HumidMeta {
                id: I::storage(self.ext.types).next(),
                humid: false,
            }
        }
    }

    fn insert_humid_item<I: Humid>(&mut self, item: I, meta: HumidMeta<I>) -> FragRef<I> {
        if meta.humid {
            I::storage(self.ext.types)[meta.id] = item;
            meta.id
        } else {
            I::storage(self.ext.types).push(item)
        }
    }

    fn handle_macro_attr(
        &mut self,
        attributes: &[TopLevelAttrAst],
        ty: Ty,
        generics: Option<Span>,
    ) {
        for attr in attributes {
            if let TopLevelAttrKindAst::Macro(.., name) = attr.value.value {
                if let Some(generics) = generics {
                    GenericMacro {
                        generics,
                        loc: self.meta.loc(attr.span()),
                    }
                    .add(self.ext.workspace);
                    break;
                }
                self.ext.types.macros.insert(
                    ty,
                    MacroImpl {
                        name: name.ident,
                        r#impl: None,
                        params: default(),
                    },
                );
                break;
            }
        }
    }
}

#[must_use]
struct HumidMeta<I> {
    id: FragRef<I>,
    humid: bool,
}

ctl_errors! {
    #[err => "invalid water drop"]
    #[info => "{message}"]
    #[info => ("list of available water drops:{}", suggestions.join(", "))]
    error InvalidWaterDrop: fatal {
        #[err loc.origin, name, "name does not match any water drop"]
        #[err loc]
        message: &'static str,
        name: Span,
        loc: SourceLoc,
        suggestions: &'static [&'static str],
    }

    #[err => "generic macro not allowed"]
    #[help => "add the macro attribute to concrete instance of the macro via type alias (TODO: type aliases)"]
    error GenericMacro: fatal {
        #[note loc.origin, generics, "type has generics defined here"]
        #[err loc]
        generics: Span,
        loc: SourceLoc,
    }

    #[err => "implementation is missing functions"]
    #[help => "missing: {missing}"]
    error MissingImplMethods: fatal {
        #[err loc]
        missing ref: String,
        loc: SourceLoc,
    }

    #[err => "generic extern functions are not allowed"]
    error GenericExtern: fatal {
        #[note loc.origin, generics, "function has generics defined here"]
        #[note loc.origin, body, "function iself.extern because of this"]
        #[err loc]
        body: Span,
        generics: Span,
        loc: SourceLoc,
    }

    #[err => "generic entry points are not allowed"]
    #[help => "wrap concrete instance of this function in a non-generic entry function"]
    error GenericEntry: fatal {
        #[note loc.origin, generics, "function has generics defined here"]
        #[note loc.origin, entry, "function is an entry point because of this attribute"]
        #[err loc]
        entry: Span,
        generics: Span,
        loc: SourceLoc,
    }

    #[err => "colliding implementations for type '{ty}'"]
    #[info => "'{ty}' already implements '{spec}'"]
    error CollidingImpl: fatal {
        #[info existing, "implementation that already satisfies"]
        #[err colliding, "it is colliding with this implementation"]
        colliding: SourceLoc,
        existing: SourceLoc,
        ty ref: String,
        spec ref: String,
    }
}

#[derive(Default, Clone, Copy)]
struct ScopeData {
    params: TyParamIter,
    owner: Option<Ty>,
    not_in_scope: bool,
    upper_vis: Option<Vis>,
}

pub trait CollectGroup<'arena>: Copy {
    type Output;

    fn output<'a>(
        transfer: &'a mut Active<TypecTransfer<'arena>>,
    ) -> &'a mut TypecOutput<Self, Self::Output>;
}

macro_rules! gen_group {
    ($(
        $for:ident<$lt:lifetime> => $out:ty => $field:ident,
    )*) => {
        $(
            impl<$lt> CollectGroup<$lt> for $for<$lt> {
                type Output = $out;

                fn output<'_a>(
                    transfer: &'_a mut Active<TypecTransfer<$lt>>,
                ) -> &'_a mut TypecOutput<Self, Self::Output> {
                    &mut transfer.$field
                }
            }
        )*
    };
}

gen_group! {
    FuncDefAst<'a> => Func => funcs,
    StructAst<'a> => Struct => structs,
    SpecAst<'a> => SpecBase => specs,
    EnumAst<'a> => Enum => enums,
    ConstAst<'a> => Const => consts,
}

use std::{default::default, iter, ops::Not};

use diags::*;
use lexing_t::*;
use packaging_t::*;
use parsing::*;
use parsing_t::*;
use storage::*;
use typec_t::*;

use crate::*;

#[allow(clippy::type_complexity)]
impl TyChecker<'_> {
    pub fn collect<T: CollectGroup>(
        &mut self,
        items: GroupedItemSlice<T>,
        collector: fn(&mut Self, T, &[TopLevelAttributeAst]) -> Option<FragRef<T::Output>>,
        out: &mut TypecOutput<T, T::Output>,
    ) -> &mut Self {
        for &(item_ast, attributes) in items.iter() {
            let Some(id) = collector(self, item_ast, attributes) else {
                continue;
            };

            out.push((item_ast, id));
        }

        self
    }

    pub fn collect_impls<'a>(
        &mut self,
        items: GroupedItemSlice<ImplAst<'a>>,
        transfer: &mut AstTransfer<'a>,
    ) -> &mut Self {
        for &(item, attributes) in items.iter() {
            self.collect_impl(item, attributes, transfer);
        }

        self
    }

    pub fn collect_impl<'a>(
        &mut self,
        r#impl @ ImplAst {
            target,
            vis,
            generics,
            body,
            ..
        }: ImplAst<'a>,
        _: &[TopLevelAttributeAst],
        transfer: &mut AstTransfer<'a>,
    ) -> Option<()> {
        let frame = self.scope.start_frame();

        let mut spec_set = SpecSet::default();

        self.insert_generics(generics, 0);
        self.generics(generics, &mut spec_set, 0);

        let (parsed_ty, parsed_spec) = match target {
            ImplTarget::Direct(ty) => (self.ty(ty)?, None),
            ImplTarget::Spec(spec, .., ty) => (self.ty(ty)?, Some(self.spec(spec)?)),
        };

        self.typec.register_ty_generics(parsed_ty, &mut spec_set);
        if let Some(parsed_spec) = parsed_spec {
            self.typec
                .register_spec_generics(parsed_spec, &mut spec_set);
        }

        self.scope.push(Interner::SELF, parsed_ty, target.span());

        let func_iter = body.iter().map(|&item| match item {
            ImplItemAst::Func(&func) => func,
        });

        let scope_data = ScopeData {
            offset: generics.len(),
            owner: Some(parsed_ty),
            not_in_scope: parsed_spec.is_some(),
            upper_vis: vis,
        };

        let mut explicit_methods = bumpvec![cap func_iter.clone().count()];
        for func in func_iter {
            let Some(id) = self.collect_func_low(func, &[], &mut spec_set, scope_data) else {
                continue;
            };

            transfer.impl_funcs.push((func, id));
            explicit_methods.push((id, func.span()));
        }

        let parsed_generics = self.take_generics(0, generics.len(), &mut spec_set);

        for &(method, ..) in explicit_methods.iter() {
            Typec::get_mut(&mut self.typec.funcs, method).upper_generics = parsed_generics;
        }

        let parsed_ty_base = parsed_ty.base(self.typec);
        let impl_id = parsed_spec.map(|parsed_spec| {
            let parsed_spec_base = parsed_spec.base(self.typec);

            // check for collisions in implementations,
            // we just check if spec is already implemented
            {
                if let Some(Some((already, ..))) = self.typec.find_implementation(
                    parsed_ty,
                    parsed_spec,
                    parsed_generics,
                    &mut None,
                    self.interner,
                ) {
                    self.workspace.push(CollidingImpl {
                        colliding: SourceLoc { span: r#impl.span(), origin: self.source },
                        existing: self.typec.impls[already].loc.source_loc(self.typec, self.resources),
                        ty: self.typec.display_ty(parsed_ty, self.interner),
                        spec: self.typec.display_spec(parsed_spec, self.interner),
                    });
                }
            }

            if parsed_spec_base == SpecBase::DROP {
                self.typec
                    .may_need_drop
                    .insert(parsed_ty.base(self.typec), true);
            }

            let key = ImplKey {
                ty: parsed_ty,
                spec: parsed_spec,
            };

            let group_key = (parsed_spec_base, parsed_ty_base);

            let methods = {
                let spec_methods = self.typec[parsed_spec_base].methods;

                let mut slots = bumpvec![None; spec_methods.len()];

                for (method, span) in explicit_methods {
                    let method_name = self.typec[method].name;
                    let Some(i) = self.typec[spec_methods].iter().position(|f| f.name == method_name) else {
                        continue;
                    };

                    if let Err(err) = self.check_impl_signature(parsed_ty, self.typec[spec_methods][i], method, true) {
                        let spec_source_loc = self.typec[parsed_spec_base].loc.map(|loc| loc.source_loc(self.typec, self.resources));
                        self.handle_signature_check_error(err, span, self.typec[spec_methods][i].span, spec_source_loc);
                        continue;
                    }

                    slots[i] = Some(method);
                }

                let iter = slots
                    .iter_mut()
                    .zip(spec_methods.keys())
                    .filter(|(slot, ..)| slot.is_none());
                for (slot, i) in iter {
                    let spec_func = self.typec[i];
                    let id = self.interner.intern_scoped(parsed_ty_base, spec_func.name);

                    let Ok(ScopeItem::Func(id)) = self.scope.get(id) else {
                        continue;
                    };

                    if self.check_impl_signature(parsed_ty, spec_func, id, false).is_err() {
                        continue;
                    }

                    *slot = Some(id);
                }

                let Some(methods) = slots.iter().copied().collect::<Option<BumpVec<_>>>() else {
                    let missing = self.typec[spec_methods]
                        .iter()
                        .zip(slots.as_slice())
                        .filter_map(|(f, m)| m.is_none().then_some(f.name))
                        .map(|name| &self.interner[name])
                        .intersperse(", ")
                        .collect::<String>();
                    self.workspace.push(MissingImplMethods {
                        loc: SourceLoc { span: r#impl.span(), origin: self.source },
                        missing,
                    })?;
                };

                self.typec.func_slices.extend(methods)
            };


            let loc = {
                let next = unsafe { self.typec.impls.next() };
                let item = ModuleItem::new(Ident::empty(), next, r#impl.span(), Vis::Priv);
                Loc {
                    module: self.module,
                    item: self.typec.module_items[self.module].items.push(item),
                }
            };

            let impl_ent = self.typec.impls.push(Impl {
                generics: parsed_generics,
                key,
                methods,
                loc,
            });
            self.typec.impl_lookup
                .entry(group_key)
                .or_default()
                .push(impl_ent);
            Some(impl_ent)
        })
        .transpose()?;

        transfer.close_impl_frame(r#impl, impl_id);

        self.scope.end_frame(frame);

        Some(())
    }

    pub fn collect_spec(
        &mut self,
        SpecAst { vis, name, .. }: SpecAst,
        attributes: &[TopLevelAttributeAst],
    ) -> Option<FragRef<SpecBase>> {
        let loc = {
            let id = self.next_humid_item_id::<SpecBase>(name.ident, attributes);
            let item = ModuleItem::new(name.ident, id, name.span, vis);
            self.insert_scope_item(item)?
        };
        let spec = SpecBase {
            name: name.ident,
            loc: Some(loc),
            ..default()
        };
        self.insert_humid_item(spec, attributes)
    }

    pub fn collect_func(
        &mut self,
        func: FuncDefAst,
        attributes: &[TopLevelAttributeAst],
    ) -> Option<FragRef<Func>> {
        self.collect_func_low(func, attributes, &mut default(), default())
    }

    fn collect_func_low(
        &mut self,
        func @ FuncDefAst {
            vis,
            signature: sig @ FuncSigAst { cc, name, .. },
            body,
            ..
        }: FuncDefAst,
        attributes: &[TopLevelAttributeAst],
        spec_set: &mut SpecSet,
        ScopeData {
            offset,
            not_in_scope,
            owner,
            upper_vis,
        }: ScopeData,
    ) -> Option<FragRef<Func>> {
        let (signature, generics) = self.collect_signature(sig, spec_set, offset)?;

        let entry = attributes
            .iter()
            .find(|attr| matches!(attr.value.value, TopLevelAttrKindAst::Entry(..)));
        let no_moves = attributes
            .iter()
            .find(|attr| matches!(attr.value.value, TopLevelAttrKindAst::NoMoves(..)));

        if let Some(entry) = entry && !generics.is_empty() {
            self.workspace.push(GenericEntry {
                func: func.span(),
                entry: entry.span(),
                generics: sig.generics.span(),
                source: self.source,
            })?;
        }

        let visibility = if let FuncBodyAst::Extern(..) = body {
            if !generics.is_empty() {
                self.workspace.push(GenericExtern {
                    func: func.span(),
                    generics: sig.generics.span(),
                    body: body.span(),
                    source: self.source,
                })?;
            }

            FuncVisibility::Imported
        } else if cc.is_some() {
            FuncVisibility::Exported
        } else {
            FuncVisibility::Local
        };

        let loc = if not_in_scope {
            None
        } else {
            // SAFETY: We push right after this, if item inset fails, id is forgotten.
            let id = unsafe { self.typec.funcs.next() };
            let vis = vis.or(upper_vis);
            let local_id = owner.map_or(name.ident, |owner| {
                self.interner
                    .intern_scoped(owner.caller(self.typec), name.ident)
            });
            let item = ModuleItem::new(local_id, id, name.span, vis);
            Some(self.insert_scope_item(item)?)
        };

        let func = Func {
            generics,
            owner,
            upper_generics: default(), // filled later
            signature,
            flags: (FuncFlags::ENTRY & entry.is_some())
                | (FuncFlags::NO_MOVES & no_moves.is_some()),
            visibility,
            name: name.ident,
            loc,
        };
        Some(self.typec.funcs.push(func))
    }

    pub fn collect_signature(
        &mut self,
        FuncSigAst {
            cc,
            generics,
            args,
            ret,
            ..
        }: FuncSigAst,
        spec_set: &mut SpecSet,
        offset: usize,
    ) -> Option<(Signature, Generics)> {
        let frame = self.scope.start_frame();

        self.insert_generics(generics, offset);
        let args = args
            .iter()
            .map(|arg| self.ty(arg.ty))
            .nsc_collect::<Option<BumpVec<_>>>()?;
        let ret = ret.map(|ret| self.ty(ret)).unwrap_or(Some(Ty::UNIT))?;

        for ty in args.iter().copied().chain(iter::once(ret)) {
            self.typec.register_ty_generics(ty, spec_set)
        }

        let signature = Signature {
            cc: cc.map(|cc| cc.ident),
            args: self.typec.args.extend(args),
            ret,
        };
        self.generics(generics, spec_set, offset);

        self.scope.end_frame(frame);

        let generics = self.take_generics(offset, generics.len(), spec_set);
        Some((signature, generics))
    }

    pub fn take_generics(&mut self, offset: usize, len: usize, spec_set: &mut SpecSet) -> Generics {
        let mut collected_generics = spec_set.iter();
        let prev_len = collected_generics
            .by_ref()
            .take(offset)
            .flat_map(|(.., it)| it)
            .count();
        let mut generics = bumpvec![cap len];
        let mut prev = offset;
        for (index, it) in collected_generics {
            for _ in prev..index as usize {
                generics.push(default());
            }
            prev = index as usize + 1;
            generics.push(self.typec.spec_sum(it, self.interner));
        }
        for _ in prev..len + offset {
            generics.push(default());
        }
        spec_set.truncate(prev_len);
        self.typec.params.extend(generics)
    }

    pub fn collect_struct(
        &mut self,
        StructAst {
            vis,
            name,
            generics,
            ..
        }: StructAst,
        attributes: &[TopLevelAttributeAst],
    ) -> Option<FragRef<Struct>> {
        let loc = self.humid_item_loc(name, Ty::Struct, attributes, vis)?;
        let s = Struct {
            name: name.ident,
            loc: Some(loc),
            ..default()
        };
        self.insert_humid_item(s, attributes).inspect(|&ty| {
            self.handle_macro_attr(
                attributes,
                Ty::Struct(ty),
                generics.is_empty().not().then(|| generics.span()),
            )
        })
    }

    pub fn collect_enum(
        &mut self,
        EnumAst {
            vis,
            name,
            generics,
            ..
        }: EnumAst,
        attributes: &[TopLevelAttributeAst],
    ) -> Option<FragRef<Enum>> {
        let loc = self.humid_item_loc(name, Ty::Enum, attributes, vis)?;
        let e = Enum {
            name: name.ident,
            loc: Some(loc),
            ..default()
        };
        self.insert_humid_item(e, attributes).inspect(|&ty| {
            self.handle_macro_attr(
                attributes,
                Ty::Enum(ty),
                generics.is_empty().not().then(|| generics.span()),
            )
        })
    }

    pub fn humid_item_loc<T: Humid>(
        &mut self,
        name: NameAst,
        map: impl Fn(FragRef<T>) -> Ty,
        attributes: &[TopLevelAttributeAst],
        vis: Vis,
    ) -> Option<Loc> {
        let id = self.next_humid_item_id(name.ident, attributes);
        let item = ModuleItem::new(name.ident, map(id), name.span, vis);
        self.insert_scope_item(item)
    }

    fn next_humid_item_id<I: Humid>(
        &mut self,
        name: Ident,
        attributes: &[TopLevelAttributeAst],
    ) -> FragRef<I> {
        if attributes
            .iter()
            .any(|attr| matches!(attr.value.value, TopLevelAttrKindAst::WaterDrop(..)))
        {
            // TODO: handle shadowing

            let name = &self.interner[name];
            let Some(id) = I::lookup_water_drop(name) else {
                todo!("{:?}", name)
            };
            id
        } else {
            unsafe { I::storage(self.typec).next() }
        }
    }

    fn insert_humid_item<I: Humid>(
        &mut self,
        item: I,
        attributes: &[TopLevelAttributeAst],
    ) -> Option<FragRef<I>> {
        let is_water_drop = attributes
            .iter()
            .any(|attr| matches!(attr.value.value, TopLevelAttrKindAst::WaterDrop(..)));
        Some(if is_water_drop {
            let name = &self.interner[item.name()];
            let Some(id) = I::lookup_water_drop(name) else {
                todo!()
            };
            *Typec::get_mut(I::storage(self.typec), id) = item;
            id
        } else {
            I::storage(self.typec).push(item)
        })
    }

    fn handle_macro_attr(
        &mut self,
        attributes: &[TopLevelAttributeAst],
        ty: Ty,
        generics: Option<Span>,
    ) {
        for attr in attributes {
            if let TopLevelAttrKindAst::Macro(.., name) = attr.value.value {
                if let Some(generics) = generics {
                    self.workspace.push(GenericMacro {
                        generics,
                        loc: SourceLoc {
                            origin: self.source,
                            span: attr.span(),
                        },
                    });
                    break;
                }
                self.typec.macros.insert(
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

ctl_errors! {
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
        #[note source, generics, "function has generics defined here"]
        #[note source, body, "function is extern because of this"]
        #[err source, func, "here"]
        func: Span,
        body: Span,
        generics: Span,
        source: VRef<Source>,
    }

    #[err => "generic entry points are not allowed"]
    #[help => "wrap concrete instance of this function in a non-generic entry function"]
    error GenericEntry: fatal {
        #[note source, generics, "function has generics defined here"]
        #[note source, entry, "function is an entry point because of this attribute"]
        #[err source, func, "here"]
        func: Span,
        entry: Span,
        generics: Span,
        source: VRef<Source>,
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
    offset: usize,
    owner: Option<Ty>,
    not_in_scope: bool,
    upper_vis: Vis,
}

pub trait CollectGroup: Copy {
    type Output;
}

impl CollectGroup for FuncDefAst<'_> {
    type Output = Func;
}

impl CollectGroup for StructAst<'_> {
    type Output = Struct;
}

impl CollectGroup for SpecAst<'_> {
    type Output = SpecBase;
}

impl CollectGroup for EnumAst<'_> {
    type Output = Enum;
}

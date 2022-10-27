use std::default::default;

use diags::gen_error_fns;
use lexing_t::*;
use parsing::*;
use parsing_t::{Ast, NameAst, Vis};

use storage::*;
use typec_t::*;

use crate::*;

#[allow(clippy::type_complexity)]
impl TyChecker<'_> {
    pub fn collect<T: CollectGroup>(
        &mut self,
        items: GroupedItemSlice<T>,
        collector: fn(&mut Self, T, &[TopLevelAttributeAst]) -> Option<VRef<T::Output>>,
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

        self.insert_generics(generics, 0);
        let parsed_generics = self.generics(generics);

        let (parsed_ty, parsed_spec) = match target {
            ImplTarget::Direct(ty) => (self.ty(ty)?, None),
            ImplTarget::Spec(spec, .., ty) => (self.ty(ty)?, Some(self.spec(spec)?)),
        };

        self.scope.push(Interner::SELF, parsed_ty, target.span());

        let impl_id = if let Some(parsed_spec) = parsed_spec {
            let parsed_ty_base = parsed_ty.base(self.typec);
            let parsed_spec_base = parsed_spec.base(self.typec);

            {
                let generics = self.typec[parsed_generics].to_bumpvec();
                if let Some(Some(already)) = self.typec.find_implementation(
                    parsed_ty,
                    parsed_spec,
                    generics.as_slice(),
                    &mut None,
                    self.interner,
                ) {
                    self.colliding_impl(self.typec.impls[already].span, parsed_ty, parsed_spec);
                }
            }

            let key = ImplKey {
                ty: parsed_ty,
                spec: parsed_spec,
            };

            let impl_ent = Impl {
                generics: parsed_generics,
                key,
                methods: default(),
                next: self.typec.impl_lookup.insert(
                    ImplKey {
                        ty: parsed_ty_base,
                        spec: Spec::Base(parsed_spec_base),
                    },
                    // SAFETY: We push right after this
                    unsafe { self.typec.impls.next() },
                ),
                span: Some(r#impl.span()),
            };
            Some(self.typec.impls.push(impl_ent))
        } else {
            None
        };

        let scope_data = ScopeData {
            offset: generics.len(),
            upper_generics: parsed_generics,
            owner: Some(parsed_ty),
            not_in_scope: impl_id.is_some(),
            upper_vis: vis,
        };

        let func_iter = body.iter().map(|&item| match item {
            ImplItemAst::Func(&func) => func,
        });

        for func in func_iter {
            let Some(id) = self.collect_func_low(func, &[], scope_data) else {
                continue;
            };

            transfer.impl_funcs.push((func, id));
        }

        transfer.close_impl_frame(r#impl, impl_id);

        self.scope.end_frame(frame);

        Some(())
    }

    pub fn collect_spec(
        &mut self,
        SpecAst { vis, name, .. }: SpecAst,
        attributes: &[TopLevelAttributeAst],
    ) -> Option<VRef<SpecBase>> {
        let loc = {
            let id = self.next_humid_item_id::<SpecBase>(name.ident, attributes);
            let item = ModuleItem::new(name.ident, id, name.span, vis);
            self.insert_scope_item(item)?
        };
        let spec = SpecBase {
            name: name.ident,
            generics: default(),
            methods: default(),
            loc: Some(loc),
        };

        self.insert_humid_item(spec, attributes)
    }

    pub fn collect_func(
        &mut self,
        func: FuncDefAst,
        attributes: &[TopLevelAttributeAst],
    ) -> Option<VRef<Func>> {
        self.collect_func_low(func, attributes, default())
    }

    fn collect_func_low(
        &mut self,
        func @ FuncDefAst {
            vis,
            signature: sig @ FuncSigAst {
                generics, cc, name, ..
            },
            body,
            ..
        }: FuncDefAst,
        attributes: &[TopLevelAttributeAst],
        ScopeData {
            offset,
            upper_generics,
            not_in_scope,
            owner,
            upper_vis,
        }: ScopeData,
    ) -> Option<VRef<Func>> {
        let (signature, parsed_generics) = self.collect_signature(sig, offset)?;

        let entry = attributes
            .iter()
            .find(|attr| matches!(attr.value.value, TopLevelAttrKindAst::Entry(..)));

        if let Some(entry) = entry && !parsed_generics.is_empty() {
            self.generic_entry(generics.span(), entry.span(), func.span())?;
        }

        let visibility = if let FuncBodyAst::Extern(..) = body {
            if !parsed_generics.is_empty() {
                self.generic_extern(generics.span(), body.span(), func.span())?;
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
                self.interner.intern_scoped(owner, name.ident)
            });
            let item = ModuleItem::new(local_id, id, name.span, vis);
            Some(self.insert_scope_item(item)?)
        };

        let func = Func {
            generics: parsed_generics,
            owner,
            upper_generics,
            signature,
            flags: FuncFlags::ENTRY & entry.is_some(),
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
        offset: usize,
    ) -> Option<(Signature, Generics)> {
        let frame = self.scope.start_frame();

        self.insert_generics(generics, offset);
        let args = args
            .iter()
            .map(|arg| self.ty(arg.ty))
            .nsc_collect::<Option<BumpVec<_>>>()?;
        let ret = ret.map(|ret| self.ty(ret)).unwrap_or(Some(Ty::UNIT))?;

        let signature = Signature {
            cc: cc.map(|cc| cc.ident),
            args: self.typec.args.bump(args),
            ret,
        };
        let parsed_generics = self.generics(generics);

        self.scope.end_frame(frame);

        Some((signature, parsed_generics))
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
    ) -> Option<VRef<Struct>> {
        let loc = self.humid_item_loc(name, Ty::Struct, attributes, vis)?;
        let s = Struct {
            name: name.ident,
            loc: Some(loc),
            ..default()
        };
        self.insert_humid_item(s, attributes)
            .inspect(|&ty| self.handle_macro_attr(attributes, Ty::Struct(ty), !generics.is_empty()))
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
    ) -> Option<VRef<Enum>> {
        let loc = self.humid_item_loc(name, Ty::Enum, attributes, vis)?;
        let e = Enum {
            name: name.ident,
            loc: Some(loc),
            ..default()
        };
        self.insert_humid_item(e, attributes)
            .inspect(|&ty| self.handle_macro_attr(attributes, Ty::Enum(ty), !generics.is_empty()))
    }

    pub fn humid_item_loc<T: Humid>(
        &mut self,
        name: NameAst,
        map: impl Fn(VRef<T>) -> Ty,
        attributes: &[TopLevelAttributeAst],
        vis: Vis,
    ) -> Option<Loc> {
        let id = self.next_humid_item_id(name.ident, attributes);
        let item = ModuleItem::new(name.ident, map(id), name.span, vis);
        self.insert_scope_item(item)
    }

    fn next_humid_item_id<I: Humid>(
        &mut self,
        name: VRef<str>,
        attributes: &[TopLevelAttributeAst],
    ) -> VRef<I> {
        if attributes
            .iter()
            .any(|attr| matches!(attr.value.value, TopLevelAttrKindAst::WaterDrop(..)))
        {
            let name = &self.interner[name];
            let Some(id) = I::lookup_water_drop(name) else {
                todo!()
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
    ) -> Option<VRef<I>> {
        let is_drop = attributes
            .iter()
            .any(|attr| matches!(attr.value.value, TopLevelAttrKindAst::WaterDrop(..)));
        Some(if is_drop {
            let name = &self.interner[item.name()];
            let Some(id) = I::lookup_water_drop(name) else {
                todo!()
            };
            I::storage(self.typec)[id] = item;
            id
        } else {
            I::storage(self.typec).push(item)
        })
    }

    fn handle_macro_attr(&mut self, attributes: &[TopLevelAttributeAst], ty: Ty, is_generic: bool) {
        for attr in attributes {
            if let TopLevelAttrKindAst::Macro(.., name) = attr.value.value {
                if is_generic {
                    todo!()
                }
                self.typec.macros.insert(
                    ty,
                    MacroImpl {
                        name: name.ident,
                        r#impl: None,
                    },
                );
                break;
            }
        }
    }

    gen_error_fns! {
        push generic_extern(self, generics: Span, body: Span, func: Span) {
            err: "function with extern body cannot be generic";
            help: "remove generic parameters from function signature";
            (func, self.source) {
                err[generics]: "this mean function is generic";
                info[body]: "function is extern because of this";
            }
        }

        push generic_entry(self, generics: Span, entry: Span, func: Span) {
            err: "generic entry functions are not allowed";
            help: "you can wrap concrete instance of this function in a non-generic entry function";
            (func, self.source) {
                err[entry]: "caused by this entry attribute";
                info[generics]: "generics located here";
            }
        }

        push colliding_impl(self, span: Option<Span>, ty: Ty, spec: Spec) {
            err: (
                "type '{}' already has an implementation for '{}'",
                self.typec.display_ty(ty, self.interner),
                self.typec.display_spec(spec, self.interner),
            );
            (span?, self.source) {
                err[span?]: "this already satisfies both types";
            }
        }
    }
}

#[derive(Default, Clone, Copy)]
struct ScopeData {
    offset: usize,
    upper_generics: Generics,
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

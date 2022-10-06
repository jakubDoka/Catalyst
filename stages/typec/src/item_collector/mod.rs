use std::default::default;

use diags::gen_error_fns;
use lexing_t::*;
use parsing::*;
use parsing_t::{Ast, Vis};

use storage::*;
use typec_t::*;

use crate::*;

#[allow(clippy::type_complexity)]
impl TyChecker<'_> {
    pub fn collect<T: CollectGroup>(
        &mut self,
        items: GroupedItemSlice<T>,
        collector: fn(
            &mut Self,
            T,
            &[TopLevelAttributeAst],
        ) -> Option<(ModuleItem, VRef<T::Output>)>,
        out: &mut TypecOutput<T, T::Output>,
    ) -> &mut Self {
        for &(item_ast, attributes) in items.iter() {
            let Some((item, id)) = collector(self, item_ast, attributes) else {
                continue;
            };

            self.insert_scope_item(item);
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
            ImplTarget::Spec(ty, .., spec) => (self.ty(ty)?, Some(self.ty(spec)?)),
        };

        self.scope
            .push(self.interner.intern_str("Self"), parsed_ty, target.span());

        let impl_id = if let Some(parsed_spec) = parsed_spec {
            let parsed_ty_base = self.typec.types.base(parsed_ty);
            let parsed_spec_base = self.typec.types.base(parsed_spec);

            if let Some(already) = self.typec.implements(parsed_ty, parsed_spec) {
                self.colliding_impl(self.typec.impls[already].span, parsed_ty, parsed_spec);
            }

            let impl_ent = Impl {
                generics: parsed_generics,
                ty: parsed_ty,
                spec: parsed_spec,
                methods: default(),
                next: self
                    .typec
                    .impl_lookup
                    .insert(
                        (parsed_ty_base, parsed_spec_base),
                        // SAFETY: We push right after this
                        Some(unsafe { self.typec.impls.next() }),
                    )
                    .flatten(),
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
            upper_vis: vis,
        };

        let func_iter = body.iter().map(|&item| match item {
            ImplItemAst::Func(&func) => func,
        });

        for func in func_iter {
            let Some((item, id)) = self.collect_func_low(func, &[], scope_data) else {
                continue;
            };

            self.insert_scope_item(item);

            transfer.impl_funcs.push((func, id));
        }

        transfer.close_impl_frame(r#impl, impl_id);

        self.scope.end_frame(frame);

        Some(())
    }

    pub fn collect_spec(
        &mut self,
        SpecAst { vis, name, .. }: SpecAst,
        _: &[TopLevelAttributeAst],
    ) -> Option<(ModuleItem, VRef<Ty>)> {
        let id = intern_scoped_ident!(self, name.ident);

        let fallback = |_: &mut Types| Ty {
            kind: TyKind::Spec(default()),
            flags: default(),
            loc: default(),
        };
        let id = self.typec.types.get_or_insert(id, fallback);

        Some((ModuleItem::new(name.ident, id, name.span, vis), id))
    }

    pub fn collect_func(
        &mut self,
        func: FuncDefAst,
        attributes: &[TopLevelAttributeAst],
    ) -> Option<(ModuleItem, VRef<Func>)> {
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
            owner,
            upper_vis,
        }: ScopeData,
    ) -> Option<(ModuleItem, VRef<Func>)> {
        let owner_id = owner.map(|owner| self.typec.types.id(self.typec.types.base(owner)));
        let id = intern_scoped_ident!(self, name.ident);
        let id = owner_id.map_or(id, |owner| self.interner.intern(scoped_ident!(owner, id)));

        let (signature, parsed_generics) = self.collect_signature(sig, offset)?;

        let entry = attributes
            .iter()
            .find(|attr| matches!(attr.value.value, TopLevelAttributeKindAst::Entry(..)));

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

        let func = |_: &mut Funcs| Func {
            generics: parsed_generics,
            owner,
            upper_generics,
            signature,
            flags: FuncFlags::ENTRY & entry.is_some(),
            visibility,
            name: name.ident,
            loc: default(),
        };
        let id = self.typec.funcs.get_or_insert(id, func);

        let vis = vis.or(upper_vis);
        let local_id = owner_id.map_or(name.ident, |owner| {
            self.interner.intern(scoped_ident!(owner, name.ident))
        });
        Some((ModuleItem::new(local_id, id, name.span, vis), id))
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
    ) -> Option<(Signature, VRefSlice<Ty>)> {
        let frame = self.scope.start_frame();

        self.insert_generics(generics, offset);
        let args = args
            .iter()
            .map(|arg| self.ty(arg.ty))
            .nsc_collect::<Option<BumpVec<_>>>()?;
        let ret = ret.map(|ret| self.ty(ret)).unwrap_or(Some(Ty::UNIT))?;

        let signature = Signature {
            cc: cc.map(|cc| cc.ident),
            args: self.typec.ty_slices.bump(args),
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
            generics,
            name,
            ..
        }: StructAst,
        _: &[TopLevelAttributeAst],
    ) -> Option<(ModuleItem, VRef<Ty>)> {
        let key = intern_scoped_ident!(self, name.ident);

        let ty = |_: &mut Types| Ty {
            kind: TyStruct::default().into(),
            flags: TyFlags::GENERIC & !generics.is_empty(),
            loc: default(),
        };
        let id = self.typec.types.get_or_insert(key, ty);

        Some((ModuleItem::new(name.ident, id, name.span, vis), id))
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

        push colliding_impl(self, span: Option<Span>, ty: VRef<Ty>, spec: VRef<Ty>) {
            err: (
                "type '{}' already has an implementation for '{}'",
                &self.interner[self.typec.types.id(ty)],
                &self.interner[self.typec.types.id(spec)],
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
    upper_generics: VRefSlice<Ty>,
    owner: Option<VRef<Ty>>,
    upper_vis: Vis,
}

pub trait CollectGroup: Copy {
    type Output;
}

impl CollectGroup for FuncDefAst<'_> {
    type Output = Func;
}

impl CollectGroup for StructAst<'_> {
    type Output = Ty;
}

impl CollectGroup for SpecAst<'_> {
    type Output = Ty;
}

impl CollectGroup for ImplAst<'_> {
    type Output = ();
}

use std::default::default;

use diags::gen_error_fns;
use lexing_t::*;
use packaging_t::*;
use parsing::*;
use parsing_t::Ast;
use scope::*;
use storage::*;
use typec_t::*;

use crate::*;

#[allow(clippy::type_complexity)]
impl TyChecker<'_> {
    pub fn collect<T: CollectGroup>(
        &mut self,
        items: GroupedItemSlice<T>,
        collector: fn(&mut Self, T, &[TopLevelAttributeAst]) -> Option<(ModItem, VRef<T::Output>)>,
        out: &mut TypecOutput<T::Output>,
    ) -> &mut Self {
        for (i, &(item, attributes)) in items.iter().enumerate() {
            let Some((item, id)) = collector(self, item, attributes) else {
                continue;
            };

            self.insert_scope_item(item);
            out.push((i, id));
        }

        self
    }

    pub fn collect_impls(
        &mut self,
        items: GroupedItemSlice<ImplAst>,
        ctx: &mut TyCheckerCtx,
    ) -> &mut Self {
        for (i, &(item, attributes)) in items.iter().enumerate() {
            self.collect_impl(i, item, attributes, ctx);
        }

        self
    }

    pub fn collect_impl(
        &mut self,
        i: usize,
        r#impl @ ImplAst { target, .. }: ImplAst,
        attrs: &[TopLevelAttributeAst],
        ctx: &mut TyCheckerCtx,
    ) -> Option<()> {
        match target {
            ImplTarget::Direct(ty) => self.collect_direct_impl(i, ty, r#impl, attrs, ctx),
            ImplTarget::Spec(_, _, _) => todo!(),
        }
    }

    pub fn collect_direct_impl(
        &mut self,
        i: usize,
        ty: TyAst,
        ImplAst {
            vis,
            generics,
            body,
            ..
        }: ImplAst,
        _: &[TopLevelAttributeAst],
        ctx: &mut TyCheckerCtx,
    ) -> Option<()> {
        let frame = self.scope.start_frame();
        self.insert_generics(generics, 0);
        let parsed_generics = self.generics(generics);
        let parsed_ty = self.ty(ty)?;

        self.scope.push(Item {
            id: self.interner.intern_str("Self"),
            ptr: parsed_ty.into(),
            span: ty.span().into(),
            whole_span: None,
            module: self.source.into(),
            vis,
        });

        let func_iter = body.iter().enumerate().map(|(i, &item)| match item {
            ImplItemAst::Func(&func) => (i, func),
        });

        let scope_data = ScopeData {
            offset: generics.len(),
            upper_generics: parsed_generics,
            owner: Some(self.typec.types.id(parsed_ty)),
            upper_vis: vis,
        };

        for (j, func) in func_iter {
            let Some((item, id)) = self.collect_func_low(func, &[], scope_data) else {
                continue;
            };

            self.insert_scope_item(item);

            ctx.impl_funcs.push((i, j, id));
        }

        self.scope.end_frame(frame);
        Some(())
    }

    pub fn collect_spec(
        &mut self,
        spec @ SpecAst { vis, name, .. }: SpecAst,
        _: &[TopLevelAttributeAst],
    ) -> Option<(ModItem, VRef<Spec>)> {
        let id = intern_scoped_ident!(self, name.ident);

        let fallback = |_: &mut Specs| Spec {
            kind: BoundKind::Base(default()),
            flags: default(),
            loc: Loc::new(name.ident, self.source, name.span, spec.span()),
        };
        let id = self.typec.specs.get_or_insert(id, fallback);

        Some((
            ModItem::new(name.ident, id, name.span, spec.span(), vis),
            id,
        ))
    }

    pub fn collect_func(
        &mut self,
        func: FuncDefAst,
        attributes: &[TopLevelAttributeAst],
    ) -> Option<(ModItem, VRef<Func>)> {
        self.collect_func_low(func, attributes, default())
    }

    fn collect_func_low(
        &mut self,
        func @ FuncDefAst {
            vis,
            signature: sig @ FuncSigAst {
                generics, cc, name, ..
            },
            span,
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
    ) -> Option<(ModItem, VRef<Func>)> {
        let id = intern_scoped_ident!(self, name.ident);
        let id = owner.map_or(id, |owner| self.interner.intern(scoped_ident!(owner, id)));

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
            upper_generics,
            signature,
            flags: FuncFlags::ENTRY & entry.is_some(),
            visibility,
            loc: Loc::new(name.ident, self.source, name.span, span),
        };
        let id = self.typec.funcs.get_or_insert(id, func);

        let vis = vis.or(upper_vis);
        let local_id = owner.map_or(name.ident, |owner| {
            self.interner.intern(scoped_ident!(owner, name.ident))
        });
        Some((ModItem::new(local_id, id, name.span, span, vis), id))
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
    ) -> Option<(Signature, VRefSlice<Spec>)> {
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
            span,
            ..
        }: StructAst,
        _: &[TopLevelAttributeAst],
    ) -> Option<(ModItem, VRef<Ty>)> {
        let key = intern_scoped_ident!(self, name.ident);

        let ty = |_: &mut Types| Ty {
            kind: TyStruct::default().into(),
            flags: TyFlags::GENERIC & !generics.is_empty(),
            loc: Loc::new(name.ident, self.source, name.span, span),
        };
        let id = self.typec.types.get_or_insert(key, ty);

        Some((ModItem::new(name.ident, id, name.span, span, vis), id))
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
    }
}

#[derive(Default, Clone, Copy)]
struct ScopeData {
    offset: usize,
    upper_generics: VRefSlice<Spec>,
    owner: Option<VRef<str>>,
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
    type Output = Spec;
}

impl CollectGroup for ImplAst<'_> {
    type Output = ();
}

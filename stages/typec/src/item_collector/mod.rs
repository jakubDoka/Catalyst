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

    pub fn collect_spec(
        &mut self,
        spec @ SpecAst { vis, name, .. }: SpecAst,
        _: &[TopLevelAttributeAst],
    ) -> Option<(ModItem, VRef<Spec>)> {
        let id = intern_scoped_ident!(self, name.ident);

        let fallback = |_: &mut Specs| Spec {
            kind: BoundKind::Base(default()),
            flags: default(),
            loc: Loc::new(name.ident, self.current_file, name.span, spec.span()),
        };
        let id = self.typec.specs.get_or_insert(id, fallback);

        Some((
            ModItem::new(name.ident, id, name.span, spec.span(), vis),
            id,
        ))
    }

    pub fn collect_func(
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
    ) -> Option<(ModItem, VRef<Func>)> {
        let id = intern_scoped_ident!(self, name.ident);

        let (signature, parsed_generics) = self.collect_signature(sig, 0)?;

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
            signature,
            flags: FuncFlags::ENTRY & entry.is_some(),
            visibility,
            loc: Loc::new(name.ident, self.current_file, name.span, span),
        };
        let id = self.typec.funcs.get_or_insert(id, func);

        Some((ModItem::new(name.ident, id, name.span, span, vis), id))
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
            loc: Loc::new(name.ident, self.current_file, name.span, span),
        };
        let id = self.typec.types.get_or_insert(key, ty);

        Some((ModItem::new(name.ident, id, name.span, span, vis), id))
    }

    gen_error_fns! {
        push generic_extern(self, generics: Span, body: Span, func: Span) {
            err: "function with extern body cannot be generic";
            help: "remove generic parameters from function signature";
            (func, self.current_file) {
                err[generics]: "this mean function is generic";
                info[body]: "function is extern because of this";
            }
        }

        push generic_entry(self, generics: Span, entry: Span, func: Span) {
            err: "generic entry functions are not allowed";
            help: "you can wrap concrete instance of this function in a non-generic entry function";
            (func, self.current_file) {
                err[entry]: "caused by this entry attribute";
                info[generics]: "generics located here";
            }
        }
    }
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

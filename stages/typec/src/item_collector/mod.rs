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

    pub fn collect_func(
        &mut self,
        func @ FuncDefAst {
            vis,
            signature:
                FuncSigAst {
                    generics,
                    cc,
                    name,
                    args,
                    ret,
                    ..
                },
            span,
            body,
            ..
        }: FuncDefAst,
        attributes: &[TopLevelAttributeAst],
    ) -> Option<(ModItem, VRef<Func>)> {
        let parsed_generics = self.generics(generics);
        let id = intern_scoped_ident!(self, name.ident);

        let args = args
            .iter()
            .map(|arg| self.ty(arg.ty))
            .nsc_collect::<Option<BumpVec<_>>>()?;
        let ret = ret.map(|ret| self.ty(ret)).unwrap_or(Some(Ty::UNIT))?;

        let signature = Signature {
            cc: cc.map(|cc| cc.ident).into(),
            args: self.typec.ty_slices.bump(args),
            ret,
        };

        let entry = attributes
            .iter()
            .find(|attr| matches!(attr.value.value, TopLevelAttributeKindAst::Entry(..)));

        if let Some(entry) = entry && !parsed_generics.is_empty() {
            self.generic_entry(generics.span(), entry.span(), func.span());
            return None;
        }

        let visibility = if let FuncBodyAst::Extern(..) = body {
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
        let generics = self.generics(generics);

        let key = intern_scoped_ident!(self, name.ident);

        let ty = |_: &mut Types| Ty {
            kind: TyStruct {
                generics,
                ..default()
            }
            .into(),
            flags: TyFlags::GENERIC & !generics.is_empty(),
            loc: Loc::new(name.ident, self.current_file, name.span, span),
        };
        let id = self.typec.types.get_or_insert(key, ty);

        Some((ModItem::new(name.ident, id, name.span, span, vis), id))
    }

    gen_error_fns! {
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

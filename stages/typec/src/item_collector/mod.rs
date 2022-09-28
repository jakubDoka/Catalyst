use std::default::default;

use lexing_t::*;
use packaging_t::*;
use parsing::*;
use scope::*;
use storage::*;
use typec_t::*;

use crate::*;

#[allow(clippy::type_complexity)]
impl TyChecker<'_> {
    pub fn collect<T: CollectGroup>(
        &mut self,
        items: &[T],
        collector: fn(&mut Self, GroupedItemsSlice<T>) -> Option<(ModItem, VRef<T::Output>)>,
        out: &mut TypecOutput<T::Output>,
    ) -> &mut Self {
        for (i, &item) in items.iter().enumerate() {
            let Some((item, id)) = collector(self, item) else {
                continue;
            };

            self.insert_scope_item(item);
            out.push((i, id));
        }

        self
    }

    pub fn collect_func(
        &mut self,
        FuncDefAst {
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
    ) -> Option<(ModItem, VRef<Func>)> {
        let generics = self.generics(generics);
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

        let visibility = if let FuncBodyAst::Extern(..) = body {
            FuncVisibility::Imported
        } else {
            // TODO: whe should use Local unless
            // function is explicitly exported
            FuncVisibility::Exported
        };

        let func = |_: &mut Funcs| Func {
            generics,
            signature,
            flags: FuncFlags::empty(),
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

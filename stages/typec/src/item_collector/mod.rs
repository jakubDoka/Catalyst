use std::default::default;

use lexing_t::*;
use packaging_t::*;
use parsing::*;
use scope::*;
use storage::*;
use typec_t::*;

use crate::*;

pub type Structs<'a> = BumpVec<(StructAst<'a>, VRef<Ty>)>;
pub type FuncDefs<'a> = BumpVec<(FuncDefAst<'a>, VRef<Func>)>;

impl TyChecker<'_> {
    pub fn collect_funcs<'a>(
        &mut self,
        items: ItemsAst<'a>,
        funcs: &mut FuncDefs<'a>,
    ) -> &mut Self {
        for &item in items.iter() {
            let res = match item {
                ItemAst::Func(&func) => self.collect_func(func, funcs),
                ItemAst::Struct(..) => continue,
            };

            let Ok(item) = res else {
                continue;
            };

            self.insert_scope_item(item);
        }

        self
    }

    pub fn collect_structs<'a>(
        &mut self,
        items: ItemsAst<'a>,
        structs: &mut Structs<'a>,
    ) -> &mut Self {
        for &item in items.iter() {
            let res = match item {
                ItemAst::Struct(&r#struct) => self.collect_struct(r#struct, structs),
                ItemAst::Func(..) => continue,
            };

            let Ok(item) = res else {
                continue;
            };

            self.insert_scope_item(item);
        }

        self
    }

    fn collect_func<'a>(
        &mut self,
        func_ast @ FuncDefAst {
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
            ..
        }: FuncDefAst<'a>,
        funcs: &mut FuncDefs<'a>,
    ) -> errors::Result<ModItem> {
        let generics = self.generics(generics);
        let id = intern_scoped_ident!(self, name.ident);

        let args = args
            .iter()
            .map(|arg| self.ty(arg.ty))
            .nsc_collect::<errors::Result<BumpVec<_>>>()?;
        let ret = ret.map(|ret| self.ty(ret)).unwrap_or(Ok(Ty::UNIT))?;

        let signature = Signature {
            cc: cc.map(|cc| cc.ident).into(),
            args: self.typec.ty_slices.bump(args),
            ret,
        };

        let func = Func {
            generics,
            signature,
            flags: FuncFlags::empty(),
            loc: Loc::new(name.ident, self.current_file, name.span, span),
        };
        let (func, shadow) = self.typec.funcs.insert(id, func);

        if shadow.is_none() {
            funcs.push((func_ast, func));
        }

        Ok(ModItem::new(name.ident, func, name.span, span, vis))
    }

    fn collect_struct<'a>(
        &mut self,
        r#struct @ StructAst {
            vis,
            generics,
            name,
            span,
            ..
        }: StructAst<'a>,
        structs: &mut Structs<'a>,
    ) -> errors::Result<ModItem> {
        let generics = self.generics(generics);

        let key = intern_scoped_ident!(self, name.ident);

        let ty = Ty {
            kind: TyStruct {
                generics,
                ..default()
            }
            .into(),
            flags: TyFlags::GENERIC & !generics.is_empty(),
            loc: Loc::new(name.ident, self.current_file, name.span, span),
        };
        let (id, shadow) = self.typec.types.insert(key, ty);

        if shadow.is_none() {
            structs.push((r#struct, id));
        }

        Ok(ModItem::new(name.ident, id, name.span, span, vis))
    }
}

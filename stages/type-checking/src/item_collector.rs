use std::ops::Not;

use crate::*;
use packaging_t::*;
use parsing_t::*;
use scope::*;
use storage::*;
use type_checking_t::*;

impl ItemCollector<'_> {
    pub fn collect(&mut self, ast: Maybe<AstList>) -> errors::Result {
        for &item in &self.ast_data[ast] {
            let res = match item.kind {
                AstKind::Struct { vis } => self.collect_struct(item, vis),
                AstKind::Func { .. }
                | AstKind::Impl { .. }
                | AstKind::BoundImpl { .. }
                | AstKind::Bound { .. } => continue,
                kind => unimplemented!("{:?}", kind),
            };

            let Ok(Some(res)) = res else {
                continue;
            };

            insert_scope_item!(self, res);
        }

        for &item in &self.ast_data[ast] {
            let res = match item.kind {
                AstKind::Struct { .. }
                | AstKind::Impl { .. }
                | AstKind::BoundImpl { .. }
                | AstKind::Bound { .. } => continue,
                AstKind::Func { vis } => self.collect_fn(item, vis),
                kind => unimplemented!("{:?}", kind),
            };

            let Ok(Some(res)) = res else {
                continue;
            };

            insert_scope_item!(self, res);
        }

        Ok(())
    }

    pub fn collect_fn(&mut self, item: AstEnt, vis: Vis) -> errors::Result<Option<ModItem>> {
        let [cc, generics, name, ref args @ .., ret, _body] = self.ast_data[item.children] else {
            unreachable!("{:?}", &self.ast_data[item.children]);
        };

        self.scope.start_frame();
        let params = self.generics(generics)?;
        let sig = self.sig(cc, args, ret)?;

        let local_id = self.interner.intern_str(span_str!(self, name.span));
        let id = intern_scoped_ident!(self, local_id);
        self.visibility[id] = vis;

        let ent = DefEnt {
            true_func: id,
            source: self.current_file.into(),
            span: name.span.into(),
            body: Maybe::none(),
            tir_data: TirData::new(),
            sig,
        };
        let def = self.funcs.defs.push(ent);
        let ent = FuncEnt {
            params,
            flags: FuncFlags::GENERIC & params.is_some(),
            def,
        };
        self.funcs.ents.insert(id, ent);
        self.item_context.funcs.push((item, def));

        Ok(Some(ModItem {
            id: local_id,
            ptr: ScopePtr::new(def),
            span: name.span,
        }))
    }

    pub fn collect_struct(&mut self, item: AstEnt, vis: Vis) -> errors::Result<Option<ModItem>> {
        let [generics, ident, ..] = self.ast_data[item.children] else {
            unreachable!();
        };

        let local = self.interner.intern_str(span_str!(self, ident.span));
        let id = intern_scoped_ident!(self, local);
        self.visibility[id] = vis;

        let ent = TyEnt {
            kind: TyKind::Inferrable,
            flags: TyFlags::GENERIC & generics.children.is_some(),
            param_count: self.ast_data[generics.children].len() as u8,
            file: self.current_file.into(),
            span: ident.span.into(),
        };
        let ty = self.types.ents.insert_unique(id, ent);
        self.item_context.types.push((item, ty));

        Ok(Some(ModItem {
            id: local,
            ptr: ScopePtr::new(ty),
            span: ident.span,
        }))
    }

    pub fn sig(&mut self, cc: AstEnt, args: &[AstEnt], ret: AstEnt) -> errors::Result<Sig> {
        let cc = cc
            .kind
            .is_none()
            .not()
            .then(|| self.interner.intern_str(span_str!(self, cc.span.shrink(1))))
            .into();
        let args = self.args(args)?;
        let ret = if ret.kind.is_none() {
            Maybe::none()
        } else {
            ty_parser!(self, self.current_file).parse(ret)?.into()
        };
        Ok(Sig { cc, args, ret })
    }

    pub fn args(&mut self, args: &[AstEnt]) -> errors::Result<Maybe<TyList>> {
        let mut reserved = self.types.slices.reserve(args.len());
        for &ast_arg in args {
            let [.., ty] = self.ast_data[ast_arg.children] else {
                unreachable!("{:?}", &self.ast_data[ast_arg.children]);
            };
            let ty = ty_parser!(self, self.current_file).parse(ty)?;
            self.types.slices.push_to_reserved(&mut reserved, ty);
        }
        Ok(self.types.slices.finish_reserved(reserved))
    }

    pub fn generics(&mut self, generics: AstEnt) -> errors::Result<Maybe<TyList>> {
        let mut params = Vec::with_capacity(self.ast_data[generics.children].len());
        for &ast_param in &self.ast_data[generics.children] {
            let (&name, bounds) = self.ast_data[ast_param.children].split_first().unwrap();
            let name = self.interner.intern_str(span_str!(self, name.span));
            let bound = ty_parser!(self, self.current_file).parse_bound_sum(bounds)?;
            let param = ty_factory!(self).param_of(bound);
            let item = ScopeItem::new(name, param, ast_param.span, self.current_file);
            self.scope.push(item);
            let param = params
                .iter()
                .rev()
                .find_map(|&p| (p == param).then(|| ty_factory!(self).next_param_of(param)))
                .unwrap_or(param);
            params.push(param);
        }
        Ok(self.types.slices.bump_slice(&params))
    }
}

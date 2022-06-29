use crate::BuiltinBuilder;

use lexer::*;
use module_types::*;
use storage::*;
use typec_types::*;

impl BuiltinBuilder<'_> {
    /// instantiates all builtin items like types, operators and functions.
    pub fn create_builtin_items(&mut self, target: &mut Vec<ModuleItem>) {
        // init the drop trait, TODO: maybe there is less time consuming way to do this
        {
            let span = self.builtin_source.make_span(self.sources, "drop");
            let id = ID::owned(self.types[self.builtin_types.drop].id, "drop".into());
            self.ty_lists.push_one(self.builtin_types.drop);
            let param = ty_factory!(self).parse_composite_bound_low(span);
            let arg = ty_factory!(self).pointer_of(param, true);
            let drop_func = self.create_func_with_params(
                span,
                &[param],
                &[arg],
                self.builtin_types.nothing,
                id,
                target,
            );
            let funcs = self.func_lists.push(&[drop_func]);
            self.types[self.builtin_types.drop].kind = TyKind::Bound(funcs);
        }

        // construct str struct
        {
            let len = self.builtin_types.uint;
            let len_name = self.builtin_source.make_span(self.sources, "len");
            let ptr = ty_factory!(self).pointer_of(self.builtin_types.u8, false);
            let ptr_name = self.builtin_source.make_span(self.sources, "ptr");

            let parent_id = self.types[self.builtin_types.str].id;

            for (i, (name, ty)) in [(ptr_name, ptr), (len_name, len)].into_iter().enumerate() {
                let id = ID::owned(parent_id, self.sources.id_of(name));
                let field = TyCompEnt {
                    name,
                    ty,
                    index: i as u32,
                };
                let field = self.ty_comps.push_one(field);
                let module_item = ModuleItem::new(id, field, name);
                target.push(module_item);
            }

            let fields = self.ty_comps.close_frame();

            self.types[self.builtin_types.str].kind = TyKind::Struct(fields);
        }

        // construct size_of function
        {
            let span = self.builtin_source.make_span(self.sources, "size_of");
            let id = ID::new("size_of");
            self.create_func_with_params(
                span,
                &[self.builtin_types.any],
                &[],
                self.builtin_types.uint,
                id,
                target,
            );
        }

        for from in self.builtin_types.numbers() {
            for to in self.builtin_types.numbers() {
                let name = self.types[to].name;
                let id = {
                    let ty = self.types[from].id;
                    let name = self.sources.id_of(name);
                    ID::owned(ty, name)
                };
                self.create_func(name, &[from], to, id, target);
            }
        }

        for ty in self.builtin_types.all() {
            let ent = &self.types[ty];
            self.ty_instances.insert(ent.id, ty);
            target.push(ModuleItem::new(ent.id, ty, ent.name));
        }

        let comparison_operators = "== != < > <= >=";
        let math_operators = "+ - * / %";
        let integer_binary_operators = format!("{} {}", comparison_operators, math_operators);
        let math_unary_operators = "-";
        let integer_unary_operators = format!("{}", math_unary_operators);

        for op in integer_binary_operators.split(' ') {
            for ty in self.builtin_types.integers() {
                let id = {
                    let id = self.types[ty].id;
                    ID::binary(id, ID::new(op))
                };
                let ret = (comparison_operators.contains(op))
                    .then_some(self.builtin_types.bool)
                    .unwrap_or(ty);
                let op = self.builtin_source.make_span(self.sources, op);
                self.create_func(op, &[ty, ty], ret, id, target);
            }
        }

        for op in integer_unary_operators.split(' ') {
            for ty in self.builtin_types.integers() {
                let id = {
                    let id = self.types[ty].id;
                    ID::unary(id, ID::new(op))
                };
                let op = self.builtin_source.make_span(self.sources, op);
                self.create_func(op, &[ty], ty, id, target);
            }
        }
    }

    fn create_func(
        &mut self,
        span: Span,
        args: &[Ty],
        ret: Ty,
        id: ID,
        target: &mut Vec<ModuleItem>,
    ) -> Func {
        self.create_func_with_params(span, &[], args, ret, id, target)
    }

    fn create_func_with_params(
        &mut self,
        span: Span,
        params: &[Ty],
        args: &[Ty],
        ret: Ty,
        id: ID,
        target: &mut Vec<ModuleItem>,
    ) -> Func {
        let sig = Sig {
            args: self.ty_comps.push_iter(args.iter().map(|&ty| TyCompEnt {
                ty,
                ..Default::default()
            })),
            ret,
            ..Default::default()
        };
        let func = {
            let ent = FuncEnt {
                id,
                flags: FuncFlags::GENERIC & !params.is_empty(),
                ..Default::default()
            };
            let meta = FuncMeta {
                sig,
                params: self.ty_lists.push(params),
                name: span,
                kind: FuncKind::Builtin,
                ..Default::default()
            };
            self.funcs.push(ent, meta)
        };

        self.func_instances.insert_unique(id, func);
        let item = ModuleItem::new(id, func, span);
        target.push(item);

        func
    }
}

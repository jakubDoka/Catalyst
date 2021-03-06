use crate::*;
use ownership::*;

impl MainTirBuilder<'_> {
    /// Generic type representation is built here. `ty_buffer` is for memory reuse and should be empty
    /// before and after this function.
    fn build_types(&mut self, stage: usize, source: Source, ty_buffer: &mut Vec<Ty>) {
        ty_buffer.extend(
            self.modules[source]
                .items
                .iter()
                .skip(stage)
                .filter_map(|item| item.kind.may_read::<Ty>()),
        );

        for ty in ty_buffer.drain(..) {
            ty_builder!(self, ty).build();
        }
    }

    /// Similar to `Self::build_types` but for functions. This action depends on types
    /// so it has to be called after.
    fn build_funcs(&mut self, stage: usize, source: Source, func_buffer: &mut Vec<Func>) {
        func_buffer.extend(
            self.modules[source]
                .items
                .iter()
                .skip(stage)
                .filter_map(|item| item.kind.may_read::<Func>()),
        );

        for func in func_buffer.drain(..) {
            // let id = self.funcs[func].id;
            // if self.incr.functions.get(id).is_some() {
            //     continue;
            // }

            // println!("{}", self.sources.display(self.funcs[func.meta()].name));

            self.tir_data.clear();

            if tir_builder!(self).func(func).is_err() {
                continue;
            }

            if !self.funcs[func].flags.contains(FuncFlags::NO_OWNERSHIP) {
                if ownership_solver!(self).solve(func).is_err() {
                    continue;
                }

                drop_solver!(self).solve(func);
            }

            self.save_used_types();
            // println!("{}", tir_display!(self, self.funcs[func.meta()].body, self.tir_data));
            self.funcs[func.meta()].tir_data = self.tir_data.clone();
        }
    }

    /// Compute the type layouts. This is only used for incremental
    /// computation while jit-compiling macros.
    fn build_layouts(&mut self, bottom: usize) {
        let iter = (bottom..self.types.len()).map(Ty::new);

        let check_point = self.ty_order.len();
        if let Err(cycle) = self.ty_graph.total_ordering(&mut self.ty_order) {
            self.diagnostics
                .push(TyError::InfinitelySizedType { cycle });
        }

        layout_builder!(self).build(self.ty_order[check_point..].iter().copied());
        build_reprs(self.host_isa.pointer_type(), &mut self.reprs, iter);
        self.ty_graph.clear();
    }

    fn build_globals(&mut self, stage: usize, source: Source, global_buffer: &mut Vec<Global>) {
        global_buffer.extend(
            self.modules[source]
                .items
                .iter()
                .skip(stage)
                .filter_map(|item| item.kind.may_read::<Global>()),
        );

        for global in global_buffer.drain(..) {
            self.tir_data.clear();
            let Ok(init) = tir_builder!(self).global(global) else {
                continue;
            };

            if ownership_solver!(self).solve(init).is_err() {
                continue;
            }

            drop_solver!(self).solve(init);
            self.save_used_types();

            self.funcs[init.meta()].tir_data = self.tir_data.clone();
            self.to_compile.push((init, TyList::reserved_value()));
            // self.initializers.push(init); // this is performed during dead code elimination
        }
    }

    fn save_used_types(&mut self) {
        self.tir_data.used_types = self.ty_lists.push(&self.scope_context.used_types);
        self.scope_context.used_types.clear();
        self.scope_context.used_types_set.clear();
    }

    /// Probably the slowest stage in frontend. Building Tir means
    /// type-checking all imported source code. Parsing is also included
    /// so that ast does not have to be accumulated for all files. Types are
    /// checked one ta the time but Tir is accumulated. Tir is also generic
    /// and ty_instances are not materialized here but rather the Tir has notion
    /// of generic calls.
    pub fn build(&mut self, module_order: &[Source]) {
        layout_builder!(self).build(self.types.keys().skip(self.builtin_types.all().len()));
        layout_builder!(self).build([self.builtin_types.str, self.builtin_types.stack_trace]);

        let mut func_buffer = vec![];
        let mut ty_buffer = vec![];
        let mut global_buffer = vec![];

        for &source in module_order {
            self.ast_data.clear();
            let mut inter_state_opt = Some(Parser::parse_imports(
                &self.sources,
                &mut self.diagnostics,
                &mut self.ast_data,
                &mut self.ast_temp,
                source,
            ));

            main_scope_builder!(self).build_scope(source);

            let mut stage = 0;

            while let Some(inter_state) = inter_state_opt {
                self.ast_data.clear();
                inter_state_opt = Parser::parse_code_chunk(
                    &self.sources,
                    &mut self.diagnostics,
                    &mut self.ast_data,
                    &mut self.ast_temp,
                    inter_state,
                );

                logger!(self).log();

                let bottom = self.types.len();

                scope_builder!(self, source).collect_items(self.ast_data.elements());

                logger!(self).log();

                self.build_types(stage, source, &mut ty_buffer);

                logger!(self).log();

                bound_verifier!(self).verify();

                self.build_globals(stage, source, &mut global_buffer);

                self.build_funcs(stage, source, &mut func_buffer);

                self.build_layouts(bottom);

                stage = self.modules[source].items.len();
            }

            self.scope.clear();
        }
    }
}

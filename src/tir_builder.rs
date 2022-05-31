use crate::*;

impl MainTirBuilder<'_> {
    /// Generic type representation is built here. `ty_buffer` is for memory reuse and should be empty
    /// before and after this function.
    pub fn build_types(&mut self, stage: usize, source: Source, ty_buffer: &mut Vec<Ty>) {
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
    pub fn build_funcs(&mut self, stage: usize, source: Source, func_buffer: &mut Vec<Func>) {
        func_buffer.extend(
            self.modules[source]
                .items
                .iter()
                .skip(stage)
                .filter_map(|item| item.kind.may_read::<Func>()),
        );

        for func in func_buffer.drain(..) {
            // TODO: This can save some time but its unclear 
            // how to determinate this
            // let id = self.funcs[func].id;
            // if self.incr.functions.get(id).is_some() {
            //     continue;
            // }

            self.tir_data.clear();
            if tir_builder!(self, func).build().is_err() {
                continue;
            };
            // println!("{}", TirDisplay::new(
            //     &self.types,
            //     &self.ty_lists,
            //     &self.ty_comps,
            //     &self.sources,
            //     &self.tir_temp_body,
            //     self.func_meta[func].body,
            // ));
            self.func_bodies[func] = self.tir_data.clone();
        }
    }

    /// Compute the type layouts. This is only used for incremental
    /// computation while jit-compiling macros.
    pub fn build_layouts(&mut self, bottom: usize) {
        let iter = (bottom..self.types.len()).map(|i| {
            let ty = Ty::new(i);
            ty
        });

        for ty in iter.clone() {
            self.ty_graph.add_vertex(ty);
        }

        let check_point = self.ty_order.len();
        if let Err(cycle) = self.ty_graph.total_ordering(&mut self.ty_order) {
            self.diagnostics
                .push(TyError::InfinitelySizedType { cycle });
        }

        layout_builder!(self).build_layouts(&self.ty_order[check_point..]);
        build_reprs(self.host_isa.pointer_type(), &mut self.reprs, iter);
        self.ty_graph.clear();
    }

    /// Probably the slowest stage in frontend. Building Tir means
    /// type-checking all imported source code. Parsing is also included
    /// so that ast does not have to be accumulated for all files. Types are
    /// checked one ta the time but Tir is accumulated. Tir is also generic
    /// and instances are not materialized here but rather the Tir has notion
    /// of generic calls.
    pub fn build(&mut self, module_order: &[Source]) {

        let mut func_buffer = vec![];
        let mut ty_buffer = vec![];

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

                self.build_funcs(stage, source, &mut func_buffer);

                self.build_layouts(bottom);

                stage = self.modules[source].items.len();
            }

            self.scope.clear();
        }
    }
}
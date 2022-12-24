#![feature(associated_type_defaults)]
#![feature(const_type_id)]
#![feature(default_free_fn)]
#![feature(anonymous_lifetime_in_impl_trait)]
#![feature(let_chains)]
#![feature(never_type)]
#![feature(iter_intersperse)]
#![feature(try_blocks)]
#![feature(result_option_inspect)]
#![feature(if_let_guard)]
#![feature(slice_group_by)]

/*
    At collection stage, we maintain a spec set (Vec<(u32, Spec)>) collected from
    uses of the parameter. When the collection is done, sort the set and collect it
    into final spec sums. This reduces boiler plate as one can specify specs on struct
    or enum it self once and use it implicitly in impls.

    Spec sums need to be unique, which we can achieve by indexing existing spec sets.
    This way it is easy to compare signatures.
*/

macro_rules! lookup {
    ($what:ident $self:expr, $id:expr, $span:expr) => {
        match $self.lookup($id, $span, stringify!($what))? {
            ScopeItem::$what(func) => func,
            item => $self.invalid_symbol_type(item, $span, stringify!($what))?,
        }
    };
}

mod func_builder;
mod item_collector;
mod state_gen;
mod tir_display;
mod ty_builder;
mod ty_parser;

pub use util::{build_scope, ActiveAstTransfer, AstTransfer, TyCheckerCtx};

pub use item_collector::CollectGroup;
pub use state_gen::TyChecker;

mod util {
    use std::mem;

    use diags::*;
    use lexing_t::*;
    use packaging_t::*;
    use parsing_t::*;
    use storage::*;
    use typec_t::*;

    pub type ImplFrames<'a> = Vec<(ImplAst<'a>, Option<FragRef<Impl>>, usize)>;

    #[derive(Default)]
    pub struct TyCheckerCtx {
        pub extern_funcs: Vec<FragRef<Func>>,
        pub ty_graph: TyGraph,
    }

    impl TyCheckerCtx {
        pub fn clear(&mut self) {
            self.extern_funcs.clear();
            self.ty_graph.clear();
        }
    }

    #[derive(Default)]
    pub struct AstTransfer<'a> {
        pub structs: TypecOutput<StructAst<'a>, Struct>,
        pub funcs: TypecOutput<FuncDefAst<'a>, Func>,
        pub specs: TypecOutput<SpecAst<'a>, SpecBase>,
        pub enums: TypecOutput<EnumAst<'a>, Enum>,
        pub impl_funcs: TypecOutput<FuncDefAst<'a>, Func>,
        pub impl_frames: ImplFrames<'a>,
    }

    impl<'a> AstTransfer<'a> {
        pub fn activate(&mut self) -> ActiveAstTransfer {
            ActiveAstTransfer(unsafe { mem::transmute(self) })
        }

        pub fn clear(&mut self) {
            self.structs.clear();
            self.funcs.clear();
            self.specs.clear();
            self.enums.clear();
            self.impl_funcs.clear();
            self.impl_frames.clear();
        }

        pub fn close_impl_frame(&mut self, ast: ImplAst<'a>, r#impl: Option<FragRef<Impl>>) {
            self.impl_frames.push((ast, r#impl, self.impl_funcs.len()));
        }
    }

    #[repr(transparent)]
    pub struct ActiveAstTransfer<'a, 'b>(&'b mut AstTransfer<'a>);

    impl Drop for ActiveAstTransfer<'_, '_> {
        fn drop(&mut self) {
            self.0.clear();
        }
    }

    impl TyChecker<'_> {
        pub fn execute<'a>(
            &mut self,
            arena: &'a Arena,
            items: GroupedItemsAst<'a>,
            ctx: &mut TyCheckerCtx,
            tir_builder_ctx: &mut TirBuilderCtx,
            transfer: ActiveAstTransfer<'a, '_>,
            type_checked_funcs: &mut BumpVec<(FragRef<Func>, TirFunc<'a>)>,
        ) -> &mut Self {
            ctx.clear();
            self.collect(items.specs, Self::collect_spec, &mut transfer.0.specs)
                .collect(items.structs, Self::collect_struct, &mut transfer.0.structs)
                .collect(items.enums, Self::collect_enum, &mut transfer.0.enums)
                .build(Self::build_spec, &transfer.0.specs)
                .collect(items.funcs, Self::collect_func, &mut transfer.0.funcs)
                .collect_impls(items.impls, transfer.0)
                .build(Self::build_struct, &transfer.0.structs)
                .build(Self::build_enum, &transfer.0.enums)
                .detect_infinite_types(ctx, transfer.0)
                .build_funcs(
                    arena,
                    &transfer.0.funcs,
                    type_checked_funcs,
                    &mut ctx.extern_funcs,
                    tir_builder_ctx,
                    0,
                )
                .build_impl_funcs(
                    arena,
                    transfer.0,
                    type_checked_funcs,
                    &mut ctx.extern_funcs,
                    tir_builder_ctx,
                )
        }

        pub fn detect_infinite_types(
            &mut self,
            ctx: &mut TyCheckerCtx,
            transfer: &AstTransfer,
        ) -> &mut Self {
            let all_new_types = transfer.structs.iter().map(|&(_, ty)| ty);

            if all_new_types.clone().next().is_none() {
                return self;
            }

            let nodes = all_new_types.clone().map(Ty::Struct);

            ctx.ty_graph.load_nodes(nodes.clone());

            for ty in all_new_types {
                let Struct { fields, .. } = self.typec[ty];
                ctx.ty_graph
                    .new_node(Ty::Struct(ty))
                    .add_edges(self.typec[fields].iter().map(|field| field.ty));
            }

            if let Err(cycle) = ctx.ty_graph.ordering(nodes, &mut bumpvec![]) {
                let cycle_chart = cycle
                    .iter()
                    .map(|&ty| self.typec.display_ty(ty, self.interner))
                    .intersperse(" -> ".into())
                    .collect::<String>();

                let snippet = CtlSnippet {
                    title: ctl_error_annotation!(err => "infinitely sized type detected between defined types"),
                    footer: vec![ctl_error_annotation!(info => ("cycle: {}", cycle_chart))],
                    source_annotations: cycle
                        .iter()
                        .skip(1) // the first and last elements are the same
                        .filter_map(|&ty| {
                            let span = ty.span(self.typec).expect("builtin types should not have cycles");
                            ctl_error_source_annotation!(info self.source, span, "this type is part of the cycle")
                        })
                        .collect(),
                };

                self.workspace.push(snippet);
            };
            self
        }
    }

    pub fn build_scope(
        module: VRef<Module>,
        scope: &mut Scope,
        resources: &Resources,
        typec: &Typec,
        interner: &mut Interner,
    ) -> BumpVec<MacroCompileRequest> {
        scope.clear();

        for ty in Builtin::ALL {
            scope.insert_builtin(interner.intern(ty.name()), Ty::Builtin(ty));
        }

        for &func in typec.builtin_funcs.iter() {
            let id = typec[func].name;
            scope.insert_builtin(id, func);
        }

        let mut token_macros = bumpvec![];
        let mod_ent = &resources.modules[module];
        for dep in &resources.module_deps[mod_ent.deps] {
            scope.push(dep.name, dep.ptr, dep.name_span);

            let items = &typec.module_items[dep.ptr];
            for &item in items.items.values() {
                scope.insert(module, dep.ptr, item, resources, interner);

                if let ModuleItemPtr::Ty(ty) = item.ptr
                    && let Some(r#impl) = typec.macros.get(&ty)
                    && let MacroImpl { name, r#impl: Some(r#impl), params } = r#impl.to_owned()
                {
                    token_macros.push(MacroCompileRequest { name, ty, r#impl, params });
                }
            }
        }
        token_macros
    }

    use crate::TyChecker;

    impl TyChecker<'_> {
        pub fn insert_scope_item(&mut self, item: ModuleItem) -> Option<Loc> {
            if let Err(record) = self.scope.insert_current(item) {
                match record.span() {
                    Some(span) => self.workspace.push(DuplicateDefinition {
                        duplicate: item.span,
                        existing: span,
                        source: self.source,
                    }),
                    None => self.workspace.push(ShadowedBuiltin {
                        item: self.interner[item.id].to_string(),
                        span: item.span,
                        source: self.source,
                    }),
                }?;
            }

            let item = self.typec.module_items[self.module].items.push(item);
            Some(Loc {
                module: self.module,
                item,
            })
        }
    }

    ctl_errors! {
        #[err => "duplicate definition"]
        #[info => "this happens when two exportable items have the same name"]
        error DuplicateDefinition: fatal {
            #[info source, duplicate, "this name"]
            #[info source, existing, "matches this already existing item"]
            duplicate: Span,
            existing: Span,
            source: VRef<Source>,
        }

        #[err => "shadowing of builtin item"]
        #[info => "shadowing {item} is disallowed"]
        error ShadowedBuiltin: fatal {
            #[info source, span, "this name"]
            span: Span,
            source: VRef<Source>,
            item ref: String,
        }
    }
}

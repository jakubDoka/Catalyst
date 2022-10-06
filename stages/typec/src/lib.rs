#![feature(let_else)]
#![feature(associated_type_defaults)]
#![feature(const_type_id)]
#![feature(default_free_fn)]
#![feature(anonymous_lifetime_in_impl_trait)]
#![feature(let_chains)]
#![feature(never_type)]
#![feature(iter_intersperse)]
#![feature(try_blocks)]
#![feature(if_let_guard)]

const TY: &str = "type";
const FUNC: &str = "function";
const TY_OR_MOD: &str = "type or module";
const FUNC_OR_MOD: &str = "function or module";

macro_rules! scoped_ident {
    ($scope:expr, $name:expr) => {
        ident!($scope, "\\", $name)
    };
}

macro_rules! intern_scoped_ident {
    ($self:expr, $name:expr) => {
        $self
            .interner
            .intern(scoped_ident!($self.module.as_u32(), $name))
    };
}

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
    use parsing::*;
    use storage::*;
    use typec_t::*;

    #[derive(Default)]
    pub struct TyCheckerCtx {
        pub tir_arena: Arena,
        pub extern_funcs: Vec<VRef<Func>>,
        pub ty_graph: TyGraph,
        pub impl_funcs: Vec<(usize, usize, VRef<Func>)>,
    }

    impl TyCheckerCtx {
        pub fn clear(&mut self) {
            self.tir_arena.clear();
            self.extern_funcs.clear();
            self.ty_graph.clear();
            self.impl_funcs.clear();
        }
    }

    #[derive(Default)]
    pub struct AstTransfer<'a> {
        pub structs: TypecOutput<StructAst<'a>, Ty>,
        pub funcs: TypecOutput<FuncDefAst<'a>, Func>,
        pub specs: TypecOutput<SpecAst<'a>, Ty>,
    }

    impl AstTransfer<'_> {
        pub fn activate<'a, 'b>(&'a mut self) -> ActiveAstTransfer<'a, 'b> {
            ActiveAstTransfer(unsafe { mem::transmute(self) })
        }

        pub fn clear(&mut self) {
            self.structs.clear();
            self.funcs.clear();
            self.specs.clear();
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
        pub fn execute<'a, 'b>(
            &mut self,
            items: GroupedItemsAst<'b>,
            ctx: &'a mut TyCheckerCtx,
            transfer: ActiveAstTransfer<'b, 'b>,
            type_checked_funcs: &mut Vec<(VRef<Func>, TirNode<'a>)>,
        ) -> &mut Self {
            ctx.clear();
            self.collect(items.specs, Self::collect_spec, &mut transfer.0.specs)
                .collect(items.structs, Self::collect_struct, &mut transfer.0.structs)
                .build(Self::build_spec, &transfer.0.specs)
                .collect(items.funcs, Self::collect_func, &mut transfer.0.funcs)
                .collect_impls(items.impls, ctx)
                .build(Self::build_struct, &transfer.0.structs)
                .detect_infinite_types(ctx, &transfer)
                .build_funcs(
                    &ctx.tir_arena,
                    &transfer.0.funcs,
                    type_checked_funcs,
                    &mut ctx.extern_funcs,
                )
                .build_impl_funcs(
                    items.impls,
                    &ctx.tir_arena,
                    &ctx.impl_funcs,
                    type_checked_funcs,
                    &mut ctx.extern_funcs,
                )
        }

        pub fn detect_infinite_types(
            &mut self,
            ctx: &mut TyCheckerCtx,
            transfer: &ActiveAstTransfer<'_, '_>,
        ) -> &mut Self {
            let all_new_types = transfer.0.structs.iter().map(|&(_, ty)| ty);

            if all_new_types.clone().next().is_none() {
                return self;
            }

            let nodes = all_new_types.clone().map(|ty| ty.as_u32());

            ctx.ty_graph.load_nodes(nodes.clone());

            for ty in all_new_types {
                let Ty { kind, .. } = self.typec.types[ty];
                match kind {
                    TyKind::Struct(s) => {
                        ctx.ty_graph.new_node(ty.as_u32()).add_edges(
                            self.typec.fields[s.fields]
                                .iter()
                                .map(|field| self.typec.types.base(field.ty).as_u32()),
                        );
                    }

                    TyKind::Instance(..) // FIXME: We still don't catch all cycles
                    | TyKind::Pointer(..)
                    | TyKind::Param(..)
                    | TyKind::Integer(..)
                    | TyKind::Spec(..)
                    | TyKind::Bool => (),
                }
            }

            if let Err(cycle) = ctx.ty_graph.ordering(nodes, &mut bumpvec![]) {
                let types = cycle
                    .into_iter()
                    .map(|i| unsafe { VRef::new(i as usize) })
                    .collect::<BumpVec<_>>();

                let cycle_chart = types
                    .iter()
                    .map(|&ty| self.typec.types.id(ty))
                    .map(|id| &self.interner[id])
                    .intersperse(" -> ")
                    .collect::<String>();

                let slice = try {
                    diags::Slice {
                        span: types
                            .iter()
                            .filter_map(|&ty| self.typec.span(ty))
                            .reduce(|a, b| a.joined(b))?,
                        origin: self.source,
                        annotations: types
                            .iter()
                            .skip(1)
                            .filter_map(|&ty| self.typec.span(ty))
                            .map(
                                |span| source_annotation!(info[span]: "this type is part of cycle"),
                            )
                            .collect(),
                        fold: true,
                    }
                };

                let snippet = Snippet {
                    title: annotation!(err: "infinitely sized type detected between defined types"),
                    footer: vec![annotation!(info: ("cycle: {}", cycle_chart))],
                    slices: vec![slice],
                    origin: format!("{}:{}", file!(), line!()),
                };

                self.workspace.push(snippet);
            };

            self
        }
    }

    pub fn build_scope(
        module: VRef<Module>,
        scope: &mut Scope,
        packages: &Resources,
        typec: &Typec,
    ) {
        scope.clear();

        for &ty in Ty::ALL {
            let id = typec.types.id(ty);
            scope.insert_builtin(id, ty);
        }

        for &func in &typec.builtin_funcs {
            let id = typec.funcs.id(func);
            scope.insert_builtin(id, func);
        }

        let mod_ent = &packages.modules[module];
        for dep in &packages.module_deps[mod_ent.deps] {
            let items = &typec.module_items[dep.ptr];
            scope.push(dep.name, dep.ptr, dep.name_span);
            for &item in items.values() {
                scope.insert(module, dep.ptr, item);
            }
        }
    }

    use crate::TyChecker;

    impl TyChecker<'_> {
        pub fn insert_scope_item(&mut self, item: ModuleItem) {
            if let Err(spans) = self.scope.insert_current(item) {
                self.duplicate_definition(item.span, spans, self.source);
                return;
            }

            self.typec.module_items[self.module].push(item);
        }

        gen_error_fns! {
            push duplicate_definition(
                self,
                duplicate: Span,
                existing: Option<Span>,
                file: VRef<Source>,
            ) {
                err: "duplicate definition";
                (duplicate, file) {
                    info[duplicate]: "this name";
                }
                (existing?, file) {
                    info[existing?]: "matches this already existing item";
                }
            }
        }
    }
}

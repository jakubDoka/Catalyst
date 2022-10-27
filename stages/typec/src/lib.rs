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

const FUNC: &str = "function";
const TY_OR_MOD: &str = "type or module";
const FUNC_OR_MOD: &str = "function or module";

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

    pub type ImplFrames<'a> = Vec<(ImplAst<'a>, Option<VRef<Impl>>, usize)>;

    #[derive(Default)]
    pub struct TyCheckerCtx {
        pub extern_funcs: Vec<VRef<Func>>,
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
        pub fn activate<'c, 'b>(&'c mut self) -> ActiveAstTransfer<'c, 'b> {
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

        pub fn close_impl_frame(&mut self, ast: ImplAst<'a>, r#impl: Option<VRef<Impl>>) {
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
            transfer: ActiveAstTransfer<'a, '_>,
            type_checked_funcs: &mut BumpVec<(VRef<Func>, TirNode<'a>)>,
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
                    0,
                )
                .build_impl_funcs(arena, transfer.0, type_checked_funcs, &mut ctx.extern_funcs)
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

                let slice = (|| {
                    Some(diags::Slice {
                        span: cycle
                            .iter()
                            .filter_map(|&ty| ty.span(self.typec))
                            .reduce(|a, b| a.joined(b))?,
                        origin: self.source,
                        annotations: cycle
                            .iter()
                            .skip(1)
                            .filter_map(|&ty| ty.span(self.typec))
                            .map(
                                |span| source_annotation!(info[span]: "this type is part of cycle"),
                            )
                            .collect(),
                        fold: true,
                    })
                })();

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
        interner: &mut Interner,
    ) -> BumpVec<VRef<Impl>> {
        scope.clear();

        for ty in Builtin::ALL {
            scope.insert_builtin(interner.intern(ty.name()), Ty::Builtin(ty));
        }

        for &func in typec.builtin_funcs.values() {
            let id = typec[func].name;
            scope.insert_builtin(id, func);
        }

        let mut token_macros = bumpvec![];
        let mod_ent = &packages.modules[module];
        for dep in &packages.module_deps[mod_ent.deps] {
            let items = &typec.module_items[dep.ptr];
            scope.push(dep.name, dep.ptr, dep.name_span);
            for &item in items.items.values() {
                scope.insert(module, dep.ptr, item, interner);
            }
            token_macros.extend(items.macros.iter().copied());
        }
        token_macros
    }

    use crate::TyChecker;

    impl TyChecker<'_> {
        pub fn insert_scope_item(&mut self, item: ModuleItem) -> Option<Loc> {
            if let Err(spans) = self.scope.insert_current(item) {
                self.duplicate_definition(item.span, spans, self.source);
                return None;
            }

            let item = self.typec.module_items[self.module].items.push(item);
            Some(Loc {
                module: self.module,
                item,
            })
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

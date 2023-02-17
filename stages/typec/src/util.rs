use {
    diags::*,
    lexing_t::*,
    packaging_t::*,
    parsing_t::*,
    std::{
        mem,
        ops::{Deref, DerefMut},
    },
    storage::*,
    typec_t::*,
};

pub type ImplFrames<'a> = Vec<(ImplAst<'a>, Option<FragRef<Impl>>, usize)>;

#[derive(Default)]
pub struct TyCheckerCtx {
    pub extern_funcs: Vec<FragRef<Func>>,
    pub ty_graph: ProjectedCycleDetector<Ty>,
}

impl TyCheckerCtx {
    pub fn clear(&mut self) {
        self.extern_funcs.clear();
        self.ty_graph.clear();
    }
}

macro_rules! gen_transfere {
    ($(
        $field:ident $ast:ident $struct:ident;
    )*) => {
        #[derive(Default)]
        pub struct AstTransfer<'a> {
            $(
                pub $field: TypecOutput<$ast<'a>, $struct>,
            )*
            pub impl_frames: ImplFrames<'a>,
        }

        impl AstTransfer<'_> {
            pub fn clear(&mut self) {
                $(
                    self.$field.clear();
                )*
                self.impl_frames.clear();
            }
        }
    };
}

gen_transfere! {
    structs StructAst Struct;
    funcs FuncDefAst Func;
    specs SpecAst SpecBase;
    enums EnumAst Enum;
    impl_funcs FuncDefAst Func;
    consts ConstAst Const;
}

impl AstTransfer<'static> {
    pub fn activate(&mut self) -> ActiveAstTransfer {
        ActiveAstTransfer::new(self)
    }
}

impl<'a> AstTransfer<'a> {
    pub fn close_impl_frame(&mut self, ast: ImplAst<'a>, r#impl: Option<FragRef<Impl>>) {
        self.impl_frames.push((ast, r#impl, self.impl_funcs.len()));
    }
}

mod active_ast_transfer {
    use super::*;

    #[repr(transparent)]
    pub struct ActiveAstTransfer<'a, 'b>(&'b mut AstTransfer<'a>);

    impl<'a, 'b> ActiveAstTransfer<'a, 'b> {
        pub fn new(transfer: &'b mut AstTransfer<'static>) -> Self {
            Self(unsafe { mem::transmute(transfer) })
        }
    }

    impl<'a> Deref for ActiveAstTransfer<'a, '_> {
        type Target = AstTransfer<'a>;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    impl<'a> DerefMut for ActiveAstTransfer<'a, '_> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.0
        }
    }

    impl Drop for ActiveAstTransfer<'_, '_> {
        fn drop(&mut self) {
            self.0.clear();
        }
    }
}
use active_ast_transfer::*;

impl TyChecker<'_> {
    #[allow(clippy::too_many_arguments)]
    pub fn execute<'a>(
        &mut self,
        arena: &'a Arena,
        items: GroupedItemsAst<'a>,
        ctx: &mut TyCheckerCtx,
        tir_builder_ctx: &mut TirBuilderCtx,
        mut transfer: ActiveAstTransfer<'a, '_>,
        type_checked_funcs: &mut BumpVec<(FragRef<Func>, TirFunc<'a>)>,
        type_checked_consts: &mut BumpVec<(FragRef<Const>, TirNode<'a>)>,
    ) -> &mut Self {
        ctx.clear();
        self.collect(items.specs, Self::collect_spec, &mut transfer.specs)
            .collect(items.structs, Self::collect_struct, &mut transfer.structs)
            .collect(items.enums, Self::collect_enum, &mut transfer.enums)
            .build(Self::build_spec, &transfer.specs)
            .build(Self::build_struct, &transfer.structs)
            .build(Self::build_enum, &transfer.enums)
            .collect(items.consts, Self::collect_const, &mut transfer.consts)
            // we build before collecting functions to ensure no cycles
            // this is bit restrictive but loosening restrictions is easier
            .build_consts(
                arena,
                tir_builder_ctx,
                &transfer.consts,
                type_checked_consts,
            )
            .collect(items.funcs, Self::collect_func, &mut transfer.funcs)
            .collect_impls(items.impls, &mut transfer)
            .detect_infinite_types(ctx, &mut transfer)
            .build_funcs(
                arena,
                &transfer.funcs,
                type_checked_funcs,
                &mut ctx.extern_funcs,
                tir_builder_ctx,
                0,
            )
            .build_impl_funcs(
                arena,
                &mut transfer,
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
    builtin_funcs: &[FragRef<Func>],
) -> BumpVec<MacroCompileRequest> {
    scope.clear();

    for ty in Builtin::ALL {
        scope.insert_builtin(interner.intern(ty.name()), Ty::Builtin(ty));
    }

    for &func in builtin_funcs {
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

impl TyChecker<'_> {}

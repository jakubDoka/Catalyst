use std::{
    default::default,
    ops::{Index, IndexMut},
};

use diags::*;
use lexing::*;
use resources::*;
use parsing_t::*;
use storage::*;
use types::*;
use type_creator::TypeCreator;

pub struct TypecExternalCtx<'arena, 'ctx> {
    pub types: &'ctx mut Types,
    pub interner: &'ctx mut Interner,
    pub workspace: &'ctx mut Workspace,
    pub resources: &'ctx Resources,
    pub transfere: &'ctx mut Active<TypecTransfere<'arena>>,
    pub folder: &'ctx mut (dyn ConstFolder + 'ctx),
}

impl<'arena, 'ctx> TypecExternalCtx<'arena, 'ctx> {
    pub fn clone_borrow(&mut self) -> TypecExternalCtx<'arena, '_> {
        TypecExternalCtx {
            types: self.types,
            interner: self.interner,
            workspace: self.workspace,
            resources: self.resources,
            transfere: self.transfere,
            folder: self.folder,
        }
    }

    pub fn check_impl_signature(
        &mut self,
        implementor: Ty,
        spec_func: SpecFunc,
        func_id: FragRef<Func>,
        collect: bool,
    ) -> Result<(), SignatureCheckError> {
        let func = self.types[func_id];

        let spec_func_params = self
            .types
            .pack_spec_func_param_specs(spec_func)
            .collect::<BumpVec<_>>();
        let generic_start = spec_func_params.len() - spec_func.generics.len();
        let mut spec_func_slots = vec![None; spec_func_params.len()];
        spec_func_slots[generic_start - 1] = Some(implementor);
        let func_params = self
            .types
            .pack_func_param_specs(func_id)
            .collect::<BumpVec<_>>();

        match (spec_func.signature.args.len(), func.signature.args.len()) {
            (a, b) if a == b => (),
            (a, b) => return Err(SignatureCheckError::ArgCountMismatch(a, b)),
        }

        match (spec_func.signature.cc, func.signature.cc) {
            (a, b) if a == b => (),
            (a, b) => return Err(SignatureCheckError::CCMismatch(a, b)),
        }

        let mut mismatches = collect.then(|| bumpvec![cap spec_func.signature.args.len() + 1]);
        for (i, (spec_arg, func_arg)) in spec_func
            .signature
            .args
            .keys()
            .zip(func.signature.args.keys())
            .enumerate()
        {
            if let Err((a, b)) = self.types.compatible(
                &mut spec_func_slots,
                self.types[func_arg],
                self.types[spec_arg],
            ) {
                if let Some(ref mut mismatches) = mismatches {
                    mismatches.push((Some(i), a, b));
                } else {
                    return Err(SignatureCheckError::ArgMismatch(default()));
                }
            }
        }

        if let Err((a, b)) = self.types.compatible(
            &mut spec_func_slots,
            func.signature.ret,
            spec_func.signature.ret,
        ) {
            if let Some(ref mut mismatches) = mismatches {
                mismatches.push((None, a, b));
            } else {
                return Err(SignatureCheckError::ArgMismatch(default()));
            }
        }

        if let Some(mismatches) = mismatches && !mismatches.is_empty() {
            return Err(SignatureCheckError::ArgMismatch(mismatches));
        }

        let Some(spec_func_slots) = spec_func_slots.into_iter().collect::<Option<BumpVec<_>>>() else {
            unimplemented!("hmmm lets see when this happens");
        };

        let mut missing_specs = collect.then(|| bumpvec![]);
        for (specs, &ty) in spec_func_params.into_iter().zip(spec_func_slots.iter()) {
            let implements = self.creator().implements_sum(
                ty,
                specs,
                func_params.as_slice(),
                &spec_func_slots,
                &mut missing_specs.as_mut(),
            );

            if !collect && !implements {
                return Err(SignatureCheckError::MissingSpecs(default()));
            }
        }

        if let Some(missing_keys) = missing_specs && !missing_keys.is_empty() {
            return Err(SignatureCheckError::MissingSpecs(missing_keys));
        }

        Ok(())
    }

    pub fn handle_signature_check_error(
        &mut self,
        err: SignatureCheckError,
        tested: Span,
        pattern: Span,
        spec_source_loc: Option<SourceLoc>,
        meta: &TypecMeta,
    ) {
        let loc = meta.loc(tested);
        match err {
            SignatureCheckError::ArgCountMismatch(expected, actual) => {
                self.workspace.push(ImplArgCountMismatch {
                    expected,
                    actual,
                    loc,
                    pattern,
                    spec_source_loc,
                });
            }
            SignatureCheckError::ArgMismatch(args) => {
                let mismatches = args
                    .into_iter()
                    .map(|(i, expected, actual)| {
                        format!(
                            "{}{} {} {}",
                            ["ret", "arg"][i.is_some() as usize],
                            i.map(|i| format!(" {i}")).unwrap_or_default(),
                            self.creator().type_diff(actual, expected),
                            self.creator().type_diff(expected, actual),
                        )
                    })
                    .intersperse_with(|| "\n".into())
                    .collect::<String>();
                self.workspace.push(DataflowMismatch {
                    mismatches,
                    loc,
                    pattern,
                    spec_source_loc,
                });
            }
            SignatureCheckError::MissingSpecs(mut specs) => {
                specs.sort_unstable();

                let missing = specs
                    .group_by(|a, b| a.ty == b.ty)
                    .map(|g| (g[0].ty, g.iter().map(|s| s.spec)))
                    .map(|(ty, bounds)| {
                        format!(
                            "{}: {}",
                            self.creator().display(ty),
                            bounds
                                .map(|b| self.creator().display(b))
                                .intersperse_with(|| " + ".into())
                                .collect::<String>(),
                        )
                    })
                    .intersperse_with(|| "\n".into())
                    .collect::<String>();

                self.workspace.push(MissingImplFuncSpecs {
                    missing,
                    loc,
                    pattern,
                    spec_source_loc,
                });
            }
            SignatureCheckError::CCMismatch(expected, actual) => {
                self.workspace.push(ImplCCMismatch {
                    expected: expected
                        .as_ref()
                        .map_or("", |cc| cc.get(self.interner))
                        .to_string(),
                    actual: actual
                        .as_ref()
                        .map_or("", |cc| cc.get(self.interner))
                        .to_string(),
                    loc,
                    pattern,
                    spec_source_loc,
                });
            }
        }
    }

    pub fn creator(&mut self) -> TypeCreator {
        TypeCreator {
            types: self.types,
            interner: self.interner,
        }
    }

    pub(crate) fn const_fold(&mut self, ret: Ty, body: TirNode) -> FolderValue {
        let ctx = ConstFolderContext {
            types: self.types,
            interner: self.interner,
            resources: self.resources,
            workspace: self.workspace,
        };

        self.folder.fold(ret, body, ctx)
    }
}

pub enum SignatureCheckError {
    ArgCountMismatch(usize, usize),
    ArgMismatch(BumpVec<(Option<usize>, Ty, Ty)>),
    MissingSpecs(BumpVec<ImplKey>),
    CCMismatch(Option<Ident>, Option<Ident>),
}

ctl_errors! {
    #[err => "dataflow of impl method does not match spec"]
    #[info => "found mismatches:\n{mismatches}"]
    error DataflowMismatch: fatal {
        #[info spec_source_loc, "spec is defined here"]
        #[info spec_source_loc.map(|loc| loc.origin), pattern, "this function has different dataflow"]
        #[err loc]
        mismatches ref: String,
        loc: SourceLoc,
        pattern: Span,
        spec_source_loc: Option<SourceLoc>,
    }

    #[err => "amount of arguments of impl method does not match spec"]
    #[info => "expected {expected} but got {actual}"]
    error ImplArgCountMismatch: fatal {
        #[info spec_source_loc, "spec is defined here"]
        #[info spec_source_loc.map(|loc| loc.origin), pattern, "this function takes {expected} arguments"]
        #[err loc, "impl method takes {actual} arguments"]
        expected: usize,
        actual: usize,
        loc: SourceLoc,
        pattern: Span,
        spec_source_loc: Option<SourceLoc>,
    }

    #[err => "amount of arguments passed to the function is incorrect"]
    #[info => "expected {expected}, but got {actual}"]
    pub error ArgCountMismatch: fatal {
        #[info func_loc, "because of this definition"]
        #[err loc]
        expected: usize,
        actual: usize,
        loc: SourceLoc,
        func_loc: Option<SourceLoc>,
    }

    #[err => "missing spec inherits"]
    #[info => "'{ty}: {missing}' is not implemented"]
    pub error MissingSpecInherits: fatal {
        #[err loc]
        ty ref: String,
        missing ref: String,
        loc: SourceLoc,
    }

    #[err => "missing impl function specs"]
    #[info => "missing:\n{missing}"]
    error MissingImplFuncSpecs: fatal {
        #[info spec_source_loc, "spec is defined here"]
        #[info spec_source_loc.map(|loc| loc.origin), pattern, "defined here"]
        #[err loc]
        missing ref: String,
        loc: SourceLoc,
        pattern: Span,
        spec_source_loc: Option<SourceLoc>,
    }

    #[err => "calling convention of impl method does not match spec"]
    #[info => "expected '{expected}' but got '{actual}'"]
    error ImplCCMismatch: fatal {
        #[info spec_source_loc, "spec is defined here"]
        #[info spec_source_loc.map(|loc| loc.origin), pattern, "this function has calling convention '{expected}'"]
        #[err loc, "impl method has calling convention '{actual}'"]
        expected ref: String,
        actual ref: String,
        loc: SourceLoc,
        pattern: Span,
        spec_source_loc: Option<SourceLoc>,
    }

}

#[derive(Default)]
pub struct TypecCtx {
    ty_graph: ProjectedCycleDetector<NewTy>,
    scope: Scope,
    vars: Vec<VarHeaderTir>,
    generics: Vec<FragSlice<Spec>>,
    cast_checks: Vec<CastCheck>,
    loops: PushMap<LoopHeaderTir>,
}

impl TypecCtx {
    pub fn clear(&mut self) {
        self.ty_graph.clear();
        // self.scope.clear(); // cleared when building scope
        assert!(self.vars.is_empty());
        // self.generics.clear(); // cleared on load
        assert!(self.cast_checks.is_empty());
        assert!(self.loops.is_empty());
    }

    pub fn insert_generics(
        &mut self,
        generics_ast: Option<ListAst<ParamAst>>,
        offset: usize,
    ) -> usize {
        let Some(generics_ast) = generics_ast else {return 0};

        for (i, &ParamAst { name, .. }) in generics_ast.iter().enumerate() {
            self.insert_param(offset + i, name)
        }

        generics_ast.len()
    }

    fn insert_param(&mut self, index: usize, name: NameAst) {
        self.scope
            .push(name.ident, Ty::Param(index as u8), name.span);
    }

    pub fn generics(&self) -> &[FragSlice<Spec>] {
        &self.generics
    }

    pub fn create_var(&mut self, mutable: bool, ty: Ty, name: NameAst) -> VRef<VarHeaderTir> {
        let index = self.vars.len();
        self.vars.push(VarHeaderTir {
            ty,
            span: name.span,
            mutable,
        });
        let var = VRef::new(index);
        self.scope.push(name.ident, var, name.span);
        var
    }

    pub fn start_frame(&self) -> TirFrame {
        TirFrame {
            scope_frame: self.scope.start_frame(),
            base: self.vars.len(),
        }
    }

    pub fn end_frame(&mut self, frame: TirFrame) {
        self.vars.truncate(frame.base);
        self.scope.end_frame(frame.scope_frame);
    }

    pub fn build_scope(
        &mut self,
        module: VRef<Module>,
        resources: &Resources,
        types: &Types,
        interner: &mut Interner,
        builtin_funcs: &[FragRef<Func>],
    ) -> BumpVec<MacroCompileRequest> {
        self.scope.clear();

        for ty in Builtin::ALL {
            self.scope
                .insert_builtin(interner.intern(ty.name()), Ty::Builtin(ty));
        }

        for &func in builtin_funcs {
            let id = types[func].name;
            self.scope.insert_builtin(id, func);
        }

        let mut token_macros = bumpvec![];
        let mod_ent = &resources.modules[module];
        for dep in &resources.module_deps[mod_ent.deps] {
            let dep_source = resources.modules[dep.ptr].source;
            self.scope.push(dep.name, dep_source, dep.name_span);
            let items = &types.module_items[dep_source];
            for &item in items.items.values() {
                self.scope
                    .insert(mod_ent.source, dep_source, item, resources, interner);

                if let ModuleItemPtr::Ty(ty) = item.ptr
                    && let Some(r#impl) = types.macros.get(&ty)
                    && let MacroImpl { name, r#impl: Some(r#impl), params } = r#impl.to_owned()
                {
                    token_macros.push(MacroCompileRequest { name, ty, r#impl, params });
                }
            }
        }
        token_macros
    }

    pub fn detect_infinite_types(
        &mut self,
        TypecExternalCtx {
            types,
            interner,
            workspace,
            transfere,
            ..
        }: &mut TypecExternalCtx,
        meta: &TypecMeta,
    ) {
        let nodes = transfere.new_types();

        if nodes.clone().next().is_none() {
            return;
        }

        self.ty_graph.load_nodes(nodes.clone());

        for ty in nodes.clone() {
            let (fields, variants) = match ty {
                NewTy::Struct(s) => (&types[types[s].fields], &[][..]),
                NewTy::Enum(e) => (&[][..], &types[types[e].variants]),
            };

            let iter = fields
                .iter()
                .map(|field| field.ty)
                .chain(variants.iter().map(|variant| variant.ty))
                .filter_map(|ty| NewTy::new(ty, types));

            self.ty_graph.new_node(ty).add_edges(iter);
        }

        if let Err(cycle) = self.ty_graph.ordering(nodes, &mut bumpvec![]) {
            let cycle_chart = cycle
                .iter()
                .map(|&ty| type_creator::display(types, interner, ty.as_ty()))
                .intersperse(" -> ".into())
                .collect::<String>();

            let snippet = CtlSnippet {
                title: ctl_error_annotation!(err => "infinitely sized type detected between defined types"),
                footer: vec![ctl_error_annotation!(info => ("cycle: {}", cycle_chart))],
                source_annotations: cycle
                    .iter()
                    .skip(1) // the first and last elements are the same
                    .filter_map(|&ty| {
                        let span = ty.as_ty().span(types).expect("builtin types should not have cycles");
                        ctl_error_source_annotation!(info meta.loc(span), "this type is part of the cycle")
                    })
                    .collect(),
            };

            workspace.push(snippet);
        };
    }

    pub(crate) fn first_unlabeled_loop(&self) -> Option<VRef<LoopHeaderTir>> {
        self.loops
            .iter()
            .rev()
            .find_map(|(id, l)| l.label.is_none().then_some(id))
    }

    pub(crate) fn pop_loop(&mut self) -> Ty {
        self.loops.pop().unwrap().return_type
    }

    pub(crate) fn push_loop(
        &mut self,
        inference: Inference,
        label: Option<NameAst>,
    ) -> VRef<LoopHeaderTir> {
        let res = self.loops.push(LoopHeaderTir {
            return_type: Ty::TERMINAL,
            inference,
            label: label.map(|l| l.ident),
        });

        if let Some(label) = label {
            self.scope.push(label.ident, res, label.span);
        }

        res
    }

    pub(crate) fn cast_check(&mut self, loc: Span, from: Ty, to: Ty) {
        self.cast_checks.push(CastCheck { loc, from, to });
    }

    pub(crate) fn load_generics(&mut self, func: impl Iterator<Item = FragSlice<Spec>>) {
        self.generics.clear();
        self.generics.extend(func);
    }

    pub fn insert_spec_functions(
        &mut self,
        generics: Generics,
        offset: usize,
        types: &Types,
        interner: &mut Interner,
    ) {
        let specs = types[generics]
            .iter()
            .enumerate()
            .flat_map(|(i, &spec)| types[spec].iter().map(move |&spec| (offset + i, spec)));
        for (i, generic) in specs {
            self.insert_spec_functions_recur(i, generic, types, interner);
        }
    }

    fn insert_spec_functions_recur(
        &mut self,
        index: usize,
        generic: Spec,
        types: &Types,
        interner: &mut Interner,
    ) {
        let spec_base = generic.base(types);
        let functions = types[spec_base].methods;

        for (key, &func) in functions.keys().zip(&types[functions]) {
            let id = interner.intern_scoped(Ty::Param(index as u8), func.name);
            self.scope.push(id, key, func.span);
        }
    }

    pub fn lookup(
        &mut self,
        sym: Ident,
        span: Span,
        what: &'static str,
        ext: &mut TypecExternalCtx,
        meta: &TypecMeta,
    ) -> Option<ScopeItem> {
        self.scope
            .get(sym)
            .map_err(|err| self.scope_error(err, sym, span, what, ext, meta))
            .ok()
    }

    pub fn scope_error(
        &mut self,
        err: ScopeError,
        sym: Ident,
        span: Span,
        expected: &'static str,
        TypecExternalCtx {
            types,
            interner,
            workspace,
            resources,
            ..
        }: &mut TypecExternalCtx,
        meta: &TypecMeta,
    ) -> Option<!> {
        let loc = meta.loc(span);
        match err {
            ScopeError::NotFound => workspace.push(ScopeItemNotFound {
                loc,
                expected,
                queried: sym.get(interner).to_string(),
            }),
            ScopeError::Collision => {
                let suggestions = resources.module_deps[resources.modules[meta.module].deps]
                    .iter()
                    .filter(|dep| {
                        types[resources.modules[dep.ptr].source]
                            .items
                            .values()
                            .any(|i| i.id == sym)
                    })
                    .map(|dep| dep.name.get(interner))
                    .intersperse(", ")
                    .collect::<String>();
                workspace.push(ScopeItemCollision { suggestions, loc })
            }
            ScopeError::Inaccessible(pos, item_def) => {
                workspace.push(InaccessibleScopeItem { loc, item_def, pos })
            }
        }
    }

    pub fn invalid_symbol_type(
        &mut self,
        item: ScopeItem,
        span: Span,
        expected: &'static str,
        workspace: &mut Workspace,
        meta: &TypecMeta,
    ) -> Option<!> {
        workspace.push(InvalidScopeItemType {
            loc: meta.loc(span),
            expected,
            actual: item.name(),
        })
    }

    pub(crate) fn try_lookup(&self, ident: Ident) -> Result<ScopeItem, ScopeError> {
        self.scope.get(ident)
    }

    pub(crate) fn push_self(&mut self, item: impl Into<ScopeItem>, span: Span) {
        self.scope.push(Interner::SELF, item, span);
    }

    pub fn insert_scope_item(
        &mut self,
        item: ModuleItem,
        TypecExternalCtx {
            types,
            interner,
            workspace,
            ..
        }: &mut TypecExternalCtx,
        meta: &TypecMeta,
    ) -> Option<GuaranteedLoc> {
        if let Err(record) = self.scope.insert_current(item) {
            match record.span() {
                Some(span) => workspace.push(DuplicateDefinition {
                    duplicate: item.span,
                    existing: span,
                    source: meta.source,
                }),
                None => workspace.push(ShadowedBuiltin {
                    item: item.id.get(interner).to_string(),
                    loc: meta.loc(item.span),
                }),
            }?;
        }

        Some(meta.item_loc(types, item))
    }

    pub fn cast_checks(&mut self) -> impl Iterator<Item = CastCheck> + '_ {
        self.cast_checks.drain(..)
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
        #[info loc, "this name"]
        loc: SourceLoc,
        item ref: String,
    }

    #[err => "scope item not found"]
    #[help => "expected {expected}"]
    #[info => "debug: queried '{queried}'"]
    error ScopeItemNotFound: fatal {
        #[err loc, "this does not exist or is not imported"]
        expected: &'static str,
        queried ref: String,
        loc: SourceLoc,
    }

    #[err => "identifier is ambiguous"]
    #[info => "items from multiple modules match the identifier"]
    #[help => "you have to specify one of ({suggestions}) as a module"]
    #[help => MOD_HELP]
    error ScopeItemCollision: fatal {
        #[err loc]
        suggestions ref: String,
        loc: SourceLoc,
    }

    #[err => "invalid scope item type"]
    #[info => "expected {expected}, got {actual}"]
    error InvalidScopeItemType: fatal {
        #[err loc]
        expected: &'static str,
        actual: &'static str,
        loc: SourceLoc,
    }

    #[err => "inaccessible scope item"]
    #[info => ("item is defined in a different {}", match pos {
        ScopePosition::Module => "module and is private",
        ScopePosition::Package => "package and is not public",
    })]
    pub error InaccessibleScopeItem: fatal {
        #[info item_def, "item defined here"]
        #[err loc]
        pos: ScopePosition,
        item_def: SourceLoc,
        loc: SourceLoc,
    }
}

impl Index<VRef<VarHeaderTir>> for TypecCtx {
    type Output = VarHeaderTir;

    fn index(&self, index: VRef<VarHeaderTir>) -> &Self::Output {
        &self.vars[index.index()]
    }
}

impl Index<VRef<LoopHeaderTir>> for TypecCtx {
    type Output = LoopHeaderTir;

    fn index(&self, index: VRef<LoopHeaderTir>) -> &Self::Output {
        &self.loops[index]
    }
}

impl IndexMut<VRef<LoopHeaderTir>> for TypecCtx {
    fn index_mut(&mut self, index: VRef<LoopHeaderTir>) -> &mut Self::Output {
        &mut self.loops[index]
    }
}

#[must_use]
pub struct TirFrame {
    scope_frame: ScopeFrame,
    base: usize,
}

#[derive(Clone, Copy)]
pub struct TypecMeta {
    module: VRef<Module>,
    source: VRef<Source>,
}

impl TypecMeta {
    pub fn new(resources: &Resources, module: VRef<Module>) -> Self {
        let source = resources.modules[module].source;
        Self { module, source }
    }

    pub fn loc(&self, span: Span) -> SourceLoc {
        SourceLoc {
            origin: self.source,
            span,
        }
    }

    pub fn source(&self) -> VRef<Source> {
        self.source
    }

    pub fn span_str<'a>(&self, span: Span, resources: &'a Resources) -> &'a str {
        resources.sources[self.source].span_str(span)
    }

    pub fn can_access(
        &self,
        loc: Loc,
        vis: Option<Vis>,
        code_span: Span,
        def_span: Span,
        ext: &mut TypecExternalCtx,
    ) -> bool {
        let (position, accessible) =
            Scope::compute_accessibility(self.source, loc.source(), vis, ext.resources);

        if let Some(pos) = position && !accessible {
            ext.workspace.push(InaccessibleScopeItem {
                pos,
                item_def: SourceLoc {
                    origin: loc.source(),
                    span: def_span,
                },
                loc: self.loc(code_span),
            });
        }

        accessible
    }

    pub(crate) fn item_loc(&self, types: &mut Types, item: ModuleItem) -> GuaranteedLoc {
        GuaranteedLoc::new(self.source, types[self.source].items.push(item))
    }
}

gen_erasable! {
    #[derive(Default)]
    pub struct TypecTransfere<'a> {
        pub(crate) structs: TypecOutput<StructAst<'a>, Struct>,
        pub(crate) funcs: TypecOutput<FuncDefAst<'a>, Func>,
        pub(crate) specs: TypecOutput<SpecAst<'a>, SpecBase>,
        pub(crate) enums: TypecOutput<EnumAst<'a>, Enum>,
        pub(crate) impl_funcs: TypecOutput<FuncDefAst<'a>, Func>,
        pub(crate) consts: TypecOutput<ConstAst<'a>, Const>,
        pub(crate) impl_frames: ImplFrames<'a>,
        pub(crate) checked_funcs: Vec<(FragRef<Func>, TirFunc<'a>)>,
        pub(crate) extern_funcs: Vec<FragRef<Func>>,
    }
}
pub type ImplFrames<'a> = Vec<(ImplAst<'a>, Option<FragRef<Impl>>, usize)>;

impl<'a> TypecTransfere<'a> {
    pub fn close_impl_frame(&mut self, ast: ImplAst<'a>, r#impl: Option<FragRef<Impl>>) {
        self.impl_frames.push((ast, r#impl, self.impl_funcs.len()));
    }

    pub fn new_types(&self) -> impl Iterator<Item = NewTy> + Clone + '_ {
        self.structs
            .iter()
            .map(|&(_, ty)| ty.into())
            .chain(self.enums.iter().map(|&(_, ty)| ty.into()))
    }

    pub fn checked_funcs(&self) -> &[(FragRef<Func>, TirFunc<'a>)] {
        &self.checked_funcs
    }
}

wrapper_enum! {
    #[derive(Hash, PartialEq, Eq, Clone, Copy)]
    enum NewTy: {
        Struct: FragRef<Struct>,
        Enum: FragRef<Enum>,
    }
}

impl NewTy {
    pub fn new(ty: Ty, types: &Types) -> Option<Self> {
        match ty {
            Ty::Struct(s) => Some(Self::Struct(s)),
            Ty::Enum(e) => Some(Self::Enum(e)),
            Ty::Instance(i) => match types[i].base {
                GenericTy::Struct(s) => Some(Self::Struct(s)),
                GenericTy::Enum(e) => Some(Self::Enum(e)),
            },
            _ => None,
        }
    }

    pub fn as_ty(self) -> Ty {
        match self {
            Self::Struct(s) => Ty::Struct(s),
            Self::Enum(e) => Ty::Enum(e),
        }
    }
}

use crate::ty_parser::MOD_HELP;

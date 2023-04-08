use std::ops::{Deref, DerefMut};

use rkyv::{Archive, Deserialize, Serialize};
use type_creator::type_creator;

use super::*;

#[derive(Serialize, Deserialize, Archive)]
pub(super) struct TaskBase {
    pub(super) interner: InternerBase,
    types: TypecBase,
    pub(super) gen: GenBase,
    mir: MirBase,
}

impl TaskBase {
    pub fn new(thread_count: u8, builtin_functions: &mut Vec<FragRef<Func>>) -> Self {
        let mut s = Self {
            interner: InternerBase::new(thread_count),
            types: TypecBase::new(thread_count),
            gen: GenBase::new(thread_count),
            mir: MirBase::new(thread_count),
        };

        if let Some(mut t) = {
            let t = s.split(None).next();
            t
        } {
            type_creator!(t).init(builtin_functions);
            t.commit_unique(&mut s);
        }

        s
    }

    pub fn expand(&mut self, thread_count: u8) {
        self.interner.expand(thread_count);
        self.types.expand(thread_count);
        self.gen.expand(thread_count);
        self.mir.expand(thread_count);
    }

    pub fn split<'a>(
        &'a mut self,
        args: Option<&'a MiddlewareArgs>,
    ) -> impl Iterator<Item = Task> + 'a {
        let mut interner_split = self.interner.split();
        let mut typec_split = self.types.split();
        let mut mir_split = self.mir.split();
        let mut gen_split = self.gen.split();
        let mut ids = 0..;
        iter::from_fn(move || {
            Some(Task {
                id: ids.next()?,
                ir_dump: args.is_some_and(|a| a.dump_ir).then(String::new),
                mir_dump: args.is_some_and(|a| a.dump_mir).then(String::new),
                tir_dump: args.is_some_and(|a| a.dump_tir).then(String::new),
                resources: TaskResources::default(),
                interner: interner_split.next()?,
                types: typec_split.next()?,
                mir: mir_split.next()?,
                gen: gen_split.next()?,
            })
        })
    }

    pub fn register<'a>(
        &'a mut self,
        frags: &mut RelocatedObjects<'a>,
        gen_relocator: &mut GenRelocator,
    ) {
        self.types.register(frags);
        self.mir.register(frags);
        self.gen.register(frags, gen_relocator);
    }
}

#[derive(Default)]
pub struct TaskResources {
    pub modules_to_compile: Vec<VRef<Module>>,
    pub entry_points: Vec<FragRef<Func>>,
    pub workspace: Workspace,
}

pub struct Task {
    pub id: usize,
    pub ir_dump: Option<String>,
    pub mir_dump: Option<String>,
    pub tir_dump: Option<String>,
    pub resources: TaskResources,
    pub interner: Interner,
    pub types: Types,
    pub mir: Mir,
    pub gen: Gen,
}

impl Deref for Task {
    type Target = TaskResources;

    fn deref(&self) -> &Self::Target {
        &self.resources
    }
}

impl DerefMut for Task {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.resources
    }
}

impl Task {
    pub(super) fn pull(&mut self, task_base: &TaskBase) {
        self.types.pull(&task_base.types);
        self.mir.pull(&task_base.mir);
    }

    pub(super) fn commit(&mut self, main_task: &mut TaskBase) {
        self.types.commit(&mut main_task.types);
        self.mir.commit(&mut main_task.mir);
    }

    pub(super) fn commit_unique(self, main_task: &mut TaskBase) {
        self.types.commit_unique(&mut main_task.types);
        self.mir.commit_unique(&mut main_task.mir);
    }
}

#[derive(Default)]
pub(super) struct TaskGraph {
    meta: Vec<(usize, VRefSlice<Package>)>,
    frontier: VecDeque<VRef<Package>>,
    inverse_graph: PushMap<VRef<Package>>,
    done: usize,
}

impl TaskGraph {
    pub(super) fn prepare(&mut self, resources: &Resources) {
        self.clear();

        let mut edges = resources
            .packages
            .keys()
            .flat_map(|key| {
                resources.package_deps[resources.packages[key].deps]
                    .iter()
                    .map(move |dep| (dep.ptr, key))
            })
            .collect::<BumpVec<_>>();
        edges.sort_unstable();

        resources
            .packages
            .values()
            .map(|package| resources.package_deps[package.deps].len())
            .map(|count| (count, default()))
            .collect_into(&mut self.meta);
        self.meta
            .iter()
            .zip(resources.packages.keys())
            .filter_map(|(&(count, ..), package)| (count == 0).then_some(package))
            .collect_into(&mut self.frontier);

        for package_deps in edges.group_by(|(a, _), (b, _)| *a == *b) {
            let package = package_deps[0].0;

            let (.., ref mut children) = self.meta[package.index()];
            *children = self
                .inverse_graph
                .extend(package_deps.iter().map(|&(_, dep)| dep));
        }
    }

    pub(super) fn next_task(&mut self) -> Option<VRef<Package>> {
        self.frontier.pop_front()
    }

    pub(super) fn finish(&mut self, package: VRef<Package>) {
        self.done += 1;
        let (.., deps) = self.meta[package.index()];
        for &dep in &self.inverse_graph[deps] {
            let index = dep.index();
            self.meta[index].0 -= 1;
            if self.meta[index].0 == 0 {
                self.frontier.push_back(dep);
            }
        }
    }

    fn clear(&mut self) {
        self.meta.clear();
        self.frontier.clear();
        self.inverse_graph.clear();
        self.done = 0;
    }

    pub(super) fn has_tasks(&self) -> bool {
        self.meta.len() != self.done
    }
}

pub(super) struct CompileRequestCollector<'a> {
    pub(super) requests: &'a mut CompileRequests,
    pub(super) isa: &'a Isa,
    pub(super) types: &'a mut Types,
    pub(super) interner: &'a mut Interner,
    pub(super) gen: &'a mut Gen,
    pub(super) ctx: &'a mut CompileRequestCollectorCtx,
    pub(super) mir: &'a Mir,
}

impl<'a> CompileRequestCollector<'a> {
    pub(super) fn distribute_compile_requests(
        mut self,
        roots: impl IntoIterator<Item = FragRef<Func>>,
    ) -> (
        Vec<CompiledFuncRef>, // in executable
        Vec<CompiledFuncRef>, // in lib or dll
    ) {
        let distributor = |func| {
            let key = Generator::func_instance_name(
                self.isa.jit,
                &self.isa.triple,
                func,
                iter::empty(),
                &self.types,
                &mut self.interner,
            );
            let (id, ..) = self.gen.get_or_insert_func(key, func);
            CompileRequestChild {
                id,
                func,
                params: default(),
            }
        };
        let frontier = roots.into_iter().map(distributor).collect::<BumpVec<_>>();

        let internal = frontier.iter().map(|req| req.id).collect();
        let external = self.collect(frontier);
        (internal, external.to_vec())
    }
    pub(super) fn collect(
        &mut self,
        roots: impl IntoIterator<Item = CompileRequestChild>,
    ) -> BumpVec<CompiledFuncRef> {
        self.requests.clear();
        self.ctx.init(roots);

        let mut imported = bumpvec![];
        while let Some(CRNode {
            req: CompileRequestChild { id, func, params },
            skipped,
        }) = self.ctx.frontier.pop()
        {
            if skipped {
                continue;
            }

            let Func {
                flags, visibility, ..
            } = self.types[func];
            if flags.contains(FuncFlags::BUILTIN) {
                continue;
            }
            if visibility == FuncVisibility::Imported {
                imported.push(id);
                continue;
            }

            let generics = self
                .types
                .pack_func_param_specs(func)
                .collect::<BumpVec<_>>();

            let body = self
                .mir
                .get_func(func, &self.types.cache.funcs)
                .unwrap()
                .to_owned();
            let module = self.mir.reference_module(body.module());
            let view = body.view(&module);

            swap_mir_types(
                &view,
                &mut self.ctx.temp_types,
                &self.requests.ty_slices[params],
                type_creator!(self),
            );

            let drops = view
                .drops
                .values()
                .map(|drop| {
                    let prev = self.ctx.frontier.len();
                    let ty = view.values[drop.value].ty();
                    self.instantiate_destructors(ty, &generics);

                    self.requests
                        .children
                        .extend(self.ctx.frontier[prev..].iter().map(CRNode::req))
                })
                .collect::<BumpVec<_>>();

            let drops = self.requests.drop_children.extend(drops);

            let calls = view
                .calls
                .values()
                .map(|call| {
                    let params = view.ty_params[call.params]
                        .iter()
                        .map(|&p| self.ctx.temp_types[p].ty)
                        .collect::<BumpVec<_>>();
                    self.load_call(call.callable, params)
                })
                .collect::<BumpVec<_>>();

            let children = self.requests.children.extend(calls.iter().map(CRNode::req));
            self.requests.queue.push(CompileRequest {
                func,
                id,
                params,
                children,
                drops,
            });

            self.ctx.frontier.extend(calls);
        }

        imported
    }

    fn instantiate_destructors(&mut self, root: VRef<TyMir>, generics: &[FragSlice<Spec>]) {
        self.ctx.ty_frontier.push(self.ctx.temp_types[root].ty);
        while let Some(ty) = self.ctx.ty_frontier.pop() {
            if !ty.may_need_drop(self.types) {
                continue;
            }

            if let Some(Some((r#impl, params))) = type_creator!(self).is_drop(ty, generics) {
                let func = self.types[self.types[r#impl].methods][0];
                let params = self.types[params].to_bumpvec();
                let call_req = self.load_call(CallableMir::Func(func), params);
                self.ctx.frontier.push(call_req);
            }

            match ty.to_base_and_params(self.types) {
                Ok((base, params)) => {
                    let types = match base {
                        BaseTy::Struct(s) => type_creator!(self).instantiate_fields(s, params),
                        BaseTy::Enum(e) => type_creator!(self).instantiate_variants(e, params),
                    };
                    self.ctx.ty_frontier.extend(types.into_iter().rev());
                }
                Err(NonBaseTy::Array(a)) => self.ctx.ty_frontier.push(self.types[a].item),
                _ => (),
            }
        }
    }

    fn load_call(&mut self, callable: CallableMir, params: BumpVec<Ty>) -> CRNode {
        let (func_id, params) = match callable {
            CallableMir::Func(func_id) => (func_id, params),
            CallableMir::SpecFunc(func) => self.load_spec_func(func, &params),
            CallableMir::Pointer(_) => todo!(),
        };

        let key = Generator::func_instance_name(
            self.isa.jit,
            &self.isa.triple,
            func_id,
            params.iter().cloned(),
            self.types,
            self.interner,
        );
        let (id, not_skipped) = self.gen.get_or_insert_func(key, func_id);

        CRNode {
            req: CompileRequestChild {
                func: func_id,
                id,
                params: self.requests.ty_slices.bump_slice(&params),
            },
            skipped: !not_skipped,
        }
    }

    fn load_spec_func(
        &mut self,
        func: FragRef<SpecFunc>,
        params: &[Ty],
    ) -> (FragRef<Func>, BumpVec<Ty>) {
        // TODO: I dislike how much can panic here, maybe improve this in the future
        let SpecFunc {
            parent, generics, ..
        } = self.types[func];
        let SpecBase { methods, .. } = self.types[parent];
        let index = methods.keys().position(|key| key == func).unwrap();
        let generic_count = params.len() - generics.len() - 1;
        let (upper, caller) = (&params[..generic_count], params[generic_count]);
        let used_spec = if upper.is_empty() {
            Spec::Base(parent)
        } else {
            Spec::Instance(type_creator!(self).spec_instance(parent, upper))
        };

        let (r#impl, params) = type_creator!(self)
            .find_implementation(caller, used_spec, &[][..], &mut None)
            .unwrap()
            .unwrap();
        let func_id = self.types[self.types[r#impl].methods][index];
        let params = self.types[params].to_bumpvec();
        (func_id, params)
    }
}

#[derive(Default)]
pub(super) struct CompileRequestCollectorCtx {
    frontier: Vec<CRNode>,
    temp_types: PushMap<TyMir>,
    ty_frontier: Vec<Ty>,
}

impl CompileRequestCollectorCtx {
    fn init(&mut self, roots: impl IntoIterator<Item = CompileRequestChild>) {
        assert!(self.frontier.is_empty());
        self.frontier.extend(roots.into_iter().map(CRNode::from));
    }
}

#[derive(Clone, Copy)]
struct CRNode {
    req: CompileRequestChild,
    skipped: bool,
}

impl CRNode {
    fn req(&self) -> CompileRequestChild {
        self.req
    }
}

impl From<CompileRequestChild> for CRNode {
    fn from(req: CompileRequestChild) -> Self {
        Self {
            req,
            skipped: false,
        }
    }
}

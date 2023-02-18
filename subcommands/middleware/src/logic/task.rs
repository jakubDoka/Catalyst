use std::ops::{Deref, DerefMut};

use rkyv::{Archive, Deserialize, Serialize};
use type_creator::type_creator;

use super::*;

#[derive(Serialize, Deserialize, Archive)]

pub struct TaskBase {
    pub interner: InternerBase,
    pub types: TypecBase,
    pub gen: GenBase,
    pub mir: MirBase,
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
            let t = s.split(false).next();
            t
        } {
            type_creator!(t).init(builtin_functions);
            t.commit_unique(&mut s);
        }

        s
    }

    pub fn split(&self, ir_dump: bool) -> impl Iterator<Item = Task> + '_ {
        let mut interner_split = self.interner.split();
        let mut typec_split = self.types.split();
        let mut mir_split = self.mir.split();
        let mut gen_split = self.gen.split();
        let mut ids = 0..;
        iter::from_fn(move || {
            Some(Task {
                id: ids.next()?,
                ir_dump: ir_dump.then(String::new),
                resources: TaskResources::default(),
                interner: interner_split.next()?,
                types: typec_split.next()?,
                mir: mir_split.next()?,
                gen: gen_split.next()?,
            })
        })
    }

    pub fn register<'a>(&'a mut self, frags: &mut RelocatedObjects<'a>) {
        self.types.register(frags);
        self.mir.register(frags);
        self.gen.register(frags);
    }
}

#[derive(Default)]
pub struct TaskResources {
    pub compile_requests: CompileRequests,
    pub modules_to_compile: Vec<VRef<Module>>,
    pub entry_points: Vec<FragRef<Func>>,
    pub workspace: Workspace,
}

pub struct Task {
    pub id: usize,
    pub ir_dump: Option<String>,
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
    pub(crate) fn traverse_compile_requests(
        mut frontier: BumpVec<(CompileRequestChild, Result<usize, usize>)>,
        tasks: &mut [Task],
        isa: &Isa,
    ) -> BumpVec<CompiledFuncRef> {
        let mut cycle = (0..tasks.len()).cycle().fuse();
        let mut imported = bumpvec![];
        let mut type_frontier = bumpvec![];
        let mut dependant_types = PushMap::default();
        let mut seen = Set::default();
        while let Some((CompileRequestChild { id, func, params }, task_id)) = frontier.pop() {
            let Ok(task_id) = task_id else { continue; };

            let task = &mut tasks[task_id];

            let Func {
                flags, visibility, ..
            } = task.types[func];
            if flags.contains(FuncFlags::BUILTIN) {
                continue;
            }
            if visibility == FuncVisibility::Imported {
                imported.push(id);
                continue;
            }

            let generics = task
                .types
                .pack_func_param_specs(func)
                .collect::<BumpVec<_>>();

            let body = task
                .mir
                .bodies
                .get(&BodyOwner::Func(func))
                .unwrap()
                .to_owned();
            let module = task.mir.modules.reference(body.module());
            let view = body.view(&module);

            swap_mir_types(
                &view,
                &mut dependant_types,
                &task.resources.compile_requests.ty_slices[params],
                type_creator!(task),
            );

            let mut drops = bumpvec![cap view.drops.len()];
            for (drop, other_id) in view.drops.values().zip(cycle.by_ref()) {
                let (task, other) = get_two_refs(tasks, other_id, task_id);
                let prev = frontier.len();
                let ty = view.values[drop.value].ty();
                type_frontier.push(dependant_types[ty].ty);
                task.instantiate_destructors(
                    &mut type_frontier,
                    isa,
                    task_id,
                    &generics,
                    &mut frontier,
                    &mut seen,
                );

                let task = other.unwrap_or(task);
                drops.push(
                    task.compile_requests
                        .children
                        .extend(frontier[prev..].iter().map(|&(child, _)| child)),
                );
            }

            let task = &mut tasks[task_id];
            let drops = task.compile_requests.drop_children.extend(drops);
            let prev = frontier.len();

            view.calls
                .values()
                .zip(cycle.by_ref())
                .map(|(&call, task_id)| {
                    let task = &mut tasks[task_id];
                    let params = view.ty_params[call.params]
                        .iter()
                        .map(|&p| dependant_types[p].ty)
                        .collect::<BumpVec<_>>();
                    task.load_call(call.callable, params, task_id, isa, &mut seen)
                })
                .collect_into(&mut *frontier);

            let children = frontier[prev..]
                .iter()
                .map(|&(mut child, other_id)| {
                    let (this, other) =
                        get_two_refs(tasks, task_id, other_id.unwrap_or_else(|e| e));
                    if let Some(other) = other {
                        child.params = this.compile_requests.ty_slices.extend(
                            other.compile_requests.ty_slices[child.params]
                                .iter()
                                .copied(),
                        );
                    }
                    child
                })
                .collect::<BumpVec<_>>();

            let task = &mut tasks[task_id];
            let children = task.compile_requests.children.extend(children);
            task.compile_requests.queue.push(CompileRequest {
                func,
                id,
                params,
                children,
                drops,
            });
        }

        imported
    }

    fn instantiate_destructors(
        &mut self,
        type_frontier: &mut BumpVec<Ty>,
        isa: &Isa,
        task_id: usize,
        generics: &[FragSlice<Spec>],
        frontier: &mut BumpVec<(CompileRequestChild, Result<usize, usize>)>,
        seen: &mut Set<CompiledFuncRef>,
    ) {
        while let Some(ty) = type_frontier.pop() {
            if !type_creator!(self).may_need_drop(ty) {
                continue;
            }

            if let Some(Some((r#impl, params))) = type_creator!(self).is_drop(ty, generics) {
                let func = self.types[self.types[r#impl].methods][0];
                let params = self.types[params].to_bumpvec();
                frontier.push(self.load_call(CallableMir::Func(func), params, task_id, isa, seen));
            }

            match ty.to_base_and_params(&self.types) {
                Ok((base, params)) => {
                    let types = match base {
                        BaseTy::Struct(s) => type_creator!(self).instantiate_fields(s, params),
                        BaseTy::Enum(e) => type_creator!(self).instantiate_variants(e, params),
                    };
                    type_frontier.extend(types);
                }
                Err(NonBaseTy::Array(..)) => todo!(),
                _ => (),
            }
        }
    }

    fn load_call(
        &mut self,
        callable: CallableMir,
        params: BumpVec<Ty>,
        task_id: usize,
        isa: &Isa,
        seen: &mut Set<CompiledFuncRef>,
    ) -> (CompileRequestChild, Result<usize, usize>) {
        let (func_id, params) = match callable {
            CallableMir::Func(func_id) => (func_id, params),
            CallableMir::SpecFunc(func) => self.load_spec_func(func, &params),
            CallableMir::Pointer(_) => todo!(),
        };

        let key = Generator::func_instance_name(
            false,
            &isa.triple,
            func_id,
            params.iter().cloned(),
            &self.types,
            &mut self.interner,
        );
        let id = self.gen.get_or_insert_func(key, func_id);

        let task_id = seen.insert(id).then_some(task_id).ok_or(task_id);

        (
            CompileRequestChild {
                func: func_id,
                id,
                params: self.compile_requests.ty_slices.bump_slice(&params),
            },
            task_id,
        )
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

    pub fn pull(&mut self, task_base: &TaskBase) {
        self.types.pull(&task_base.types);
    }

    pub fn commit(&mut self, main_task: &mut TaskBase) {
        self.types.commit(&mut main_task.types);
    }

    pub fn commit_unique(self, main_task: &mut TaskBase) {
        self.types.commit_unique(&mut main_task.types);
    }
}

#[derive(Default)]
pub struct TaskGraph {
    meta: Vec<(usize, VRefSlice<Package>)>,
    frontier: VecDeque<VRef<Package>>,
    inverse_graph: PushMap<VRef<Package>>,
    done: usize,
}

impl TaskGraph {
    pub fn prepare(&mut self, resources: &Resources) {
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

    pub(crate) fn next_task(&mut self) -> Option<VRef<Package>> {
        self.frontier.pop_front()
    }

    pub(crate) fn finish(&mut self, package: VRef<Package>) {
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

    pub(crate) fn has_tasks(&self) -> bool {
        self.meta.len() != self.done
    }
}

fn get_two_refs<T>(slice: &mut [T], a: usize, b: usize) -> (&mut T, Option<&mut T>) {
    if a == b {
        return (&mut slice[a], None);
    }

    // SAFETY: Since a != b borrowing these elements is valid.
    unsafe { (mem::transmute(&mut slice[a]), Some(&mut slice[b])) }
}

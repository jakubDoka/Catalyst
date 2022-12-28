use std::ops::{Deref, DerefMut};

use super::*;

pub struct TaskBase {
    pub interner: InternerBase,
    pub typec: TypecBase,
    pub gen: GenBase,
    pub mir: Mir,
}

impl TaskBase {
    pub fn new(thread_count: u8) -> Self {
        Self {
            interner: InternerBase::new(thread_count),
            typec: TypecBase::new(thread_count),
            gen: GenBase::new(thread_count),
            mir: default(),
        }
    }

    pub fn split(&self, ir_dump: bool) -> impl Iterator<Item = Task> + '_ {
        let mut interner_split = self.interner.split();
        let mut typec_split = self.typec.split();
        let mut gen_split = self.gen.split();
        let mut ids = (0..).into_iter();
        iter::from_fn(move || {
            Some(Task {
                id: ids.next()?,
                ir_dump: ir_dump.then(String::new),
                resources: TaskResources::default(),
                interner: interner_split.next()?,
                typec: typec_split.next()?,
                mir: self.mir.clone(),
                gen: gen_split.next()?,
            })
        })
    }
}

derive_relocated!(struct TaskBase { typec gen mir });

#[derive(Default)]
pub struct TaskResources {
    pub compile_requests: CompileRequests,
    pub modules_to_compile: Vec<VRef<Module>>,
    pub entry_points: Vec<FragRef<Func>>,
    pub workspace: Workspace,
    pub dependant_types: FuncTypes,
}

pub struct Task {
    pub id: usize,
    pub ir_dump: Option<String>,
    pub resources: TaskResources,
    pub interner: Interner,
    pub typec: Typec,
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
        mut frontier: BumpVec<(CompileRequestChild, Option<usize>)>,
        tasks: &mut [Task],
        isa: &Isa,
    ) -> BumpVec<FragRef<CompiledFunc>> {
        let mut cycle = (0..tasks.len()).cycle().fuse();
        let mut imported = bumpvec![];
        let mut type_frontier = bumpvec![];
        while let Some((CompileRequestChild { id, func, params }, task_id)) = frontier.pop() {
            let Some(task_id) = task_id else { continue; };
            let task = &mut tasks[task_id];
            let Func {
                flags, visibility, ..
            } = task.typec[func];
            if flags.contains(FuncFlags::BUILTIN) {
                continue;
            }
            if visibility == FuncVisibility::Imported {
                imported.push(id);
                continue;
            }

            let generics = task
                .typec
                .pack_func_param_specs(func)
                .collect::<BumpVec<_>>();

            let body = task.mir.bodies.get(&func).unwrap().clone();
            task.dependant_types.clear();
            task.dependant_types.extend(body.types.values().copied());
            let bumped_params = task.compile_requests.ty_slices[params].to_bumpvec();
            swap_mir_types(
                &body.inner,
                &mut task.dependant_types,
                &bumped_params,
                &mut task.typec,
                &mut task.interner,
            );

            let mut drops = bumpvec![cap body.drops.len()];
            for (drop, task_id) in body.drops.values().zip(cycle.by_ref()) {
                let task = &mut tasks[task_id];
                let prev = frontier.len();

                type_frontier.push(task.dependant_types[body.values[drop.value].ty].ty);
                task.instantiate_destructors(
                    &mut type_frontier,
                    isa,
                    task_id,
                    &generics,
                    &mut frontier,
                );

                drops.push(
                    task.compile_requests
                        .children
                        .extend(frontier[prev..].iter().map(|&(child, _)| child)),
                );
            }
            let task = &mut tasks[task_id];
            let drops = task.compile_requests.drop_children.extend(drops);

            let prev = frontier.len();
            body.calls
                .values()
                .zip(cycle.by_ref())
                .map(|(&call, task_id)| {
                    let task = &mut tasks[task_id];
                    let params = body.ty_params[call.params]
                        .iter()
                        .map(|&p| task.dependant_types[p].ty)
                        .collect::<BumpVec<_>>();
                    task.load_call(call.callable, params, task_id, isa)
                })
                .collect_into(&mut *frontier);
            let task = &mut tasks[task_id];
            let children = task
                .compile_requests
                .children
                .extend(frontier[prev..].iter().map(|&(child, _)| child));

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
        frontier: &mut BumpVec<(CompileRequestChild, Option<usize>)>,
    ) {
        while let Some(ty) = type_frontier.pop() {
            if !self.typec.may_need_drop(ty, &mut self.interner) {
                continue;
            }

            if let Some(Some((r#impl, params))) =
                ty.is_drop(generics, &mut self.typec, &mut self.interner)
            {
                let func = self.typec[self.typec[r#impl].methods][0];
                let params = self.typec[params].to_bumpvec();
                frontier.push(self.load_call(CallableMir::Func(func), params, task_id, isa));
            }

            match ty {
                Ty::Struct(s) => {
                    self.typec[self.typec[s].fields]
                        .iter()
                        // we do rev to preserve recursive order of fields
                        .rev()
                        .map(|f| f.ty)
                        .collect_into(&mut **type_frontier);
                }
                Ty::Enum(e) => {
                    self.typec[self.typec[e].variants]
                        .iter()
                        .rev()
                        .map(|v| v.ty)
                        .collect_into(&mut **type_frontier);
                }
                Ty::Instance(i) => {
                    let Instance { base, args } = self.typec[i];
                    match base {
                        GenericTy::Struct(s) => {
                            self.typec[self.typec[s].fields]
                                .to_bumpvec()
                                .into_iter()
                                .rev()
                                .map(|f| self.typec.instantiate(f.ty, args, &mut self.interner))
                                .collect_into(&mut **type_frontier);
                        }
                        GenericTy::Enum(e) => {
                            self.typec[self.typec[e].variants]
                                .to_bumpvec()
                                .into_iter()
                                .rev()
                                .map(|v| self.typec.instantiate(v.ty, args, &mut self.interner))
                                .collect_into(&mut **type_frontier);
                        }
                    }
                }
                Ty::Pointer(..) | Ty::Param(..) | Ty::Builtin(..) => (),
            }
        }
    }

    fn load_call(
        &mut self,
        callable: CallableMir,
        params: BumpVec<Ty>,
        task_id: usize,
        isa: &Isa,
    ) -> (CompileRequestChild, Option<usize>) {
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
            &self.typec,
            &mut self.interner,
        );
        let task_id = self.gen.get(key).is_none().then_some(task_id);
        let id = self
            .gen
            .get_or_insert(key, || CompiledFunc::new(func_id, key));

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
        } = self.typec[func];
        let SpecBase { methods, .. } = self.typec[parent];
        let index = methods.keys().position(|key| key == func).unwrap();
        let generic_count = params.len() - generics.len() - 1;
        let (upper, caller) = (&params[..generic_count], params[generic_count]);
        let used_spec = if upper.is_empty() {
            Spec::Base(parent)
        } else {
            Spec::Instance(self.typec.spec_instance(parent, upper, &mut self.interner))
        };

        let (r#impl, params) = self
            .typec
            .find_implementation(caller, used_spec, &[][..], &mut None, &mut self.interner)
            .unwrap()
            .unwrap();
        let func_id = self.typec[self.typec[r#impl].methods][index];
        let params = self.typec[params].to_bumpvec();
        (func_id, params)
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

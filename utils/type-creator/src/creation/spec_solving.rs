use storage::*;
use types::*;

pub struct SpecQuestion<'a> {
    pub spec: Spec<'a>,
    pub ty: Ty<'a>,
    pub implications: ExpWhereClause<'a>,
}

pub struct SpecAnswer<'a> {
    pub implementation: OptFragRef<Impl>,
    pub params: &'a [Ty<'a>],
}

pub struct Facts<'a> {
    prooved: Map<TyParam, Vec<Spec<'a>>>,
}

impl<'a> SpecQuestion<'a> {
    pub fn impl_key(&self) -> ImplKey {
        ImplKey {
            spec: self.spec.base(),
            ty: self.ty.significant_type().map(|ty| CompactBaseTy::new(ty)),
        }
    }
}

pub struct SpecSolverCtx<'a> {
    proof_stack: Vec<SpecQuestion<'a>>,
}

pub struct SpecSolver<'ctx, 'arena, 'outher_arena> {
    pub types: &'ctx Types,
    pub arena: &'ctx mut ProxyArena<'arena>,
    pub facts: &'ctx Active<Facts<'outher_arena>>,
    pub ctx: &'ctx mut Active<SpecSolverCtx<'arena>>,
}

impl<'ctx, 'arena, 'outher_arena> SpecSolver<'ctx, 'arena, 'outher_arena> {
    fn impl_candidates(&self, question: &SpecQuestion<'arena>) -> Option<ImplList> {
        self.types
            .mapping
            .impl_lookup
            .get(&question.impl_key())
            .map(|list| list.to_owned())
    }

    pub fn answer(
        &self,
        question: &SpecQuestion<'arena>,
        missing_proofs: &mut BumpVec<SpecQuestion<'arena>>,
    ) -> Option<SpecAnswer<'arena>> {
        let candidates = self.impl_candidates(question)?;

        candidates
            .inner
            .into_iter()
            .find_map(|c| self.check_implementation(c, question, missing_proofs))
    }

    fn check_implementation(
        &self,
        candidate_ref: FragRef<Impl>,
        question: &SpecQuestion<'arena>,
        missing_proofs: &mut BumpVec<SpecQuestion<'arena>>,
    ) -> Option<SpecAnswer<'arena>> {
        let candidate = &self.types[candidate_ref];
        let mut params = bumpvec![None; candidate.generics.parameter_count.get()];
        params[0] = Some(question.ty);
        let candidate_ty = self.load_ty(candidate.ty);
        question.ty.infer(candidate_ty, &mut params).ok()?;
        let candidate_spec = self.load_spec(candidate.spec);
        question.spec.infer(candidate_spec, &mut params).ok()?;
        let mut params = params.transpose_options()?;

        self.check_predicates(candidate.generics, &params, missing_proofs);

        let archived_params = self.arena.alloc_slice(&params);

        if let (Some(candidate_base), Ok((question_base, question_params))) =
            (candidate_ty.base(), question.ty.to_base_and_params())
        {
            params.truncate(1);
            params.extend_from_slice(question_params);

            self.check_predicates(candidate_base.generics(self.types), &params, missing_proofs);
        }

        params.truncate(1);
        params.extend_from_slice(question.spec.params());
        self.check_predicates(
            self.types[candidate_spec.base()].generics,
            &params,
            missing_proofs,
        );

        Some(SpecAnswer {
            implementation: Some(candidate_ref),
            params: archived_params,
        })
    }

    fn check_predicates(
        &self,
        generics: WhereClause,
        params: &[Ty<'arena>],
        missing_proofs: &mut BumpVec<SpecQuestion<'arena>>,
    ) {
        for pred in &self.types[generics.predicates] {
            let ty = self.load_ty(pred.ty);
            let ty = self.instantiate(ty, params, generics);
            for &spec in &self.types[pred.bounds] {
                let spec = self.load_spec(spec);
                let spec = self.instantiate_spec(spec, params);
            }
        }
    }

    fn instantiate(
        &self,
        ty: Ty<'arena>,
        params: &[Ty<'arena>],
        where_clause: WhereClause,
    ) -> Ty<'arena> {
        match ty {
            Ty::Node(Node::Instance(b)) => Ty::Node(Node::Instance(b)),
            Ty::Instance(ExpInstance { base, args }) => {
                let args = args
                    .iter()
                    .map(|&arg| self.instantiate(arg, params, where_clause));
                let args = self.arena.alloc_iter(args);
                Ty::Instance(ExpInstance { base, args })
            }
            Ty::Pointer(Pointer {
                mutability,
                depth,
                ty,
            }) => {
                let ty = self.instantiate(*ty, params, where_clause);
                let ty = self.arena.alloc(ty);
                Ty::Pointer(Pointer {
                    mutability,
                    depth,
                    ty,
                })
            }
            Ty::Array(ExpArray { item, len }) => {
                let item = self.instantiate(*item, params, where_clause);
                let item = self.arena.alloc(item);
                Ty::Array(ExpArray { item, len })
            }
            Ty::Param(param) => self.instantiate_param(param, params, where_clause),
            Ty::Builtin(b) => Ty::Builtin(b),
        }
    }

    fn instantiate_param(
        &self,
        param: TyParam,
        params: &[Ty<'arena>],
        where_clause: WhereClause,
    ) -> Ty<'arena> {
        let Some(mut asoc) = param.asoc else {
            return params[param.index.get()];
        };

        let mut asoc_seq = bumpvec![asoc];
        let mut next = param.index;
        let predicates = &self.types[where_clause.predicates];
        while let Some(pred) = predicates[next.get()] {
            let ty = self.load_ty(pred.ty);
            let spec = 
            
        } 
    }

    fn load_ty(&self, ty: CompactTy) -> Ty<'arena> {
        Ty::load(ty, self.types, self.arena)
    }

    fn load_spec(&self, spec: CompactSpec) -> Spec<'arena> {
        Spec::load(spec, self.types, self.arena)
    }
}

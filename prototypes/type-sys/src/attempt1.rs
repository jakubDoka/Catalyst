use std::{collections::HashMap, iter};

type Bounds = Vec<Spec>;

crate::records! {
    (Clone, Debug, PartialEq, Eq)
    enum Ty {
        Instance(Instance<TyDef>),
        Base(TyDef),
        Param(Param),
    }

    enum AsocTy {
        Instance(Instance<AsocDef>),
        Def(AsocDef),
    }

    enum BaseTy {
        Def(TyDef),
    }

    enum Spec {
        Instance(Instance<SpecDef>),
        Def(SpecDef),
    }

    struct Goal {
        ty: Ty,
        bounds: Bounds,
    }

    struct Param {
        index: usize,
        asoc: Option<AsocTy>,
    }

    struct Fact {
        spec: Spec,
        ty: Ty,
        asoc_types: Vec<Ty>,
        conditions: Where,
    }

    struct Where {
        params: Vec<Bounds>,
        goals: Vec<Goal>,
    }
}

impl Default for Where {
    fn default() -> Self {
        Where {
            params: vec![vec![]],
            goals: vec![],
        }
    }
}

impl From<TyDef> for BaseTy {
    fn from(def: TyDef) -> Self {
        BaseTy::Def(def)
    }
}

impl From<TyDef> for Ty {
    fn from(def: TyDef) -> Self {
        Ty::Node(Node::Instance(def))
    }
}

impl From<SpecDef> for Spec {
    fn from(def: SpecDef) -> Self {
        Spec::Def(def)
    }
}

impl From<Instance<SpecDef>> for Spec {
    fn from(inst: Instance<SpecDef>) -> Self {
        Spec::Instance(inst)
    }
}

impl Param {
    fn simple(index: usize) -> Self {
        Param { index, asoc: None }
    }
}

records! {
    (Clone, Debug, PartialEq, Eq, Default)
    struct Instance<T> {
        def: T,
        params: Vec<Ty>,
    }

    struct TyDef {
        name: String,
        conditions: Where,
    }

    struct SpecDef {
        name: String,
        asoc_types: Vec<AsocDef>,
        conditions: Where,
    }

    struct AsocDef {
        spec: String,
        name: String,
        conditions: Where,
    }


    struct Program {
        specs: HashMap<String, SpecDef>,
        facts: Vec<Fact>,
    }
}

impl Param {
    fn instantiate(&self, params: &[Ty], origin: &Fact, program: &Program) -> Option<Ty> {
        let Some(ref asoc) = self.asoc else {
            return Some(params[self.index].clone());
        };

        let mut path = vec![(self.index, asoc)];
        while let Some(parent) = path
            .last()
            .unwrap()
            .0
            .checked_sub(origin.conditions.params.len())
            .map(|index| &origin.conditions.goals[index].ty)
        {
            let &Ty::Param(Param { asoc: Some(ref asoc), index }) = parent else {
                break;
            };

            path.push((index, asoc));
        }

        let mut base = params[path.last().unwrap().0].clone();
        for (.., asoc) in path.into_iter().rev().skip(1) {
            let target_spec = program.specs.get(&asoc.base().spec).unwrap();
            let index = target_spec.asoc_index(&asoc.base().name).unwrap();

            let impls = program.answer(Fact {
                spec: target_spec.clone().into(),
                ty: base.clone(),
                asoc_types: vec![],
                conditions: origin.conditions.clone(),
            })?;

            let Some(impl_fact) = impls.first() else {
                 return Some(Ty::Param(self.clone()));
            };

            base = impl_fact.asoc_types[index].clone();
        }

        Some(base)
    }
}

impl Ty {
    fn base(&self) -> Option<&TyDef> {
        Some(match self {
            Ty::Node(Node::Instance(inst)) => &inst.def,
            Ty::Node(Node::Instance(def)) => def,
            Ty::Param(..) => return None,
        })
    }

    fn infer_params(&self, q: &Ty, params: &mut [Option<Ty>]) -> bool {
        match (self, q) {
            (Ty::Param(param), q) => match params[param.index] {
                Some(ref p) => p == q,
                None => {
                    params[param.index] = Some(q.clone());
                    true
                }
            },
            (Ty::Node(Node::Instance(inst)), Ty::Instance(q_inst)) => inst.infer_params(q_inst, params),
            (a, b) => a == b,
        }
    }

    fn instantiate(&self, params: &[Ty], origin: &Fact, program: &Program) -> Option<Ty> {
        match self {
            Ty::Param(param) => param.instantiate(params, origin, program),
            Ty::Node(Node::Instance(inst)) => inst.instantiate(params, origin, program).map(Ty::Instance),
            Ty::Node(Node::Instance(def)) => Some(Ty::Base(def.clone())),
        }
    }
}

impl TyDef {
    fn implications(&self, q: &Ty, origin: &Fact, program: &Program) -> Option<Vec<(Spec, Ty)>> {
        let Ty::Node(Node::Instance(inst)) = q else {
            assert_eq!(&Ty::Base(self.clone()), q);
            return Some(vec![]);
        };

        self.conditions
            .implications(inst.params.clone(), origin, program)
    }
}

impl AsocTy {
    fn implications(&self, q: &Ty, origin: &Fact, program: &Program) -> Option<Vec<(Spec, Ty)>> {
        let Ty::Node(Node::Instance(inst)) = q else {
        assert!(matches!(q, Ty::Param(Param { asoc: Some(asoc), .. }) if asoc == self));
        return Some(vec![]);
    };

        self.base()
            .conditions
            .implications(inst.params.clone(), origin, program)
    }

    fn base(&self) -> &AsocDef {
        match self {
            Self::Def(d) => d,
            Self::Instance(inst) => &inst.def,
        }
    }

    fn infer_params(&self, q: &AsocTy, params: &mut [Option<Ty>]) -> bool {
        match (self, q) {
            (AsocTy::Node(Node::Instance(inst)), AsocTy::Instance(q_inst)) => inst.infer_params(q_inst, params),
            (a, b) => a == b,
        }
    }

    fn instantiate(&self, params: &[Ty], origin: &Fact, program: &Program) -> Option<AsocTy> {
        Some(match self {
            AsocTy::Node(Node::Instance(inst)) => AsocTy::Instance(inst.instantiate(params, origin, program)?),
            AsocTy::Def(def) => AsocTy::Def(def.clone()),
        })
    }
}

impl<T> Instance<T> {
    fn infer_params(&self, q: &Instance<T>, params: &mut [Option<Ty>]) -> bool
    where
        T: Eq,
    {
        self.def == q.def
            && self
                .params
                .iter()
                .zip(q.params.iter())
                .all(|(p, q)| p.infer_params(q, params))
    }

    fn instantiate(&self, params: &[Ty], origin: &Fact, program: &Program) -> Option<Instance<T>>
    where
        T: Clone,
    {
        Some(Instance {
            def: self.def.clone(),
            params: self
                .params
                .iter()
                .map(|p| p.instantiate(params, origin, program))
                .collect::<Option<_>>()?,
        })
    }
}

impl Spec {
    fn base(&self) -> &SpecDef {
        match self {
            Spec::Instance(inst) => &inst.def,
            Spec::Def(def) => def,
        }
    }

    fn infer_params(&self, q: &Spec, params: &mut [Option<Ty>]) -> bool {
        match (self, q) {
            (Spec::Instance(inst), Spec::Instance(q_inst)) => inst.infer_params(q_inst, params),
            (a, b) => a == b,
        }
    }

    fn instantiate(&self, params: &[Ty], origin: &Fact, program: &Program) -> Option<Spec> {
        match self {
            Spec::Instance(inst) => inst
                .instantiate(params, origin, program)
                .map(Spec::Instance),
            Spec::Def(def) => Some(Spec::Def(def.clone())),
        }
    }

    fn expand_self(&self, origin: &Fact, program: &Program) -> Option<Vec<Spec>> {
        match self {
            Spec::Instance(inst) => {
                let specs = inst.def.conditions.self_bounds();
                specs
                    .iter()
                    .map(|spec| spec.instantiate(&inst.params, origin, program))
                    .collect()
            }
            Spec::Def(def) => Some(def.conditions.self_bounds().to_vec()),
        }
    }

    fn get_asoc(&self, name: &str) -> Option<&AsocDef> {
        self.base().get_asoc(name)
    }
}

impl SpecDef {
    fn implications(
        &self,
        q: &Spec,
        base: &Ty,
        origin: &Fact,
        program: &Program,
    ) -> Option<Vec<(Spec, Ty)>> {
        let Spec::Instance(inst) = q else {
            assert_eq!(&Spec::Def(self.clone()), q);
            return Some(vec![]);
        };

        self.conditions.implications(
            iter::once(base.clone())
                .chain(inst.params.clone())
                .collect(),
            origin,
            program,
        )
    }

    fn get_asoc(&self, name: &str) -> Option<&AsocDef> {
        self.asoc_types.iter().find(|asoc| asoc.name == name)
    }

    fn asoc_index(&self, name: &str) -> Option<usize> {
        self.asoc_types.iter().position(|asoc| asoc.name == name)
    }
}

impl Where {
    fn implications(
        &self,
        params: Vec<Ty>,
        fact: &Fact,
        program: &Program,
    ) -> Option<Vec<(Spec, Ty)>> {
        let param_implications = params
            .iter()
            .zip(self.params.clone())
            .flat_map(|(p, b)| b.into_iter().zip(iter::repeat(p.clone())));
        let goal_implications = self
            .goals
            .iter()
            .flat_map(|goal| {
                goal.bounds
                    .clone()
                    .into_iter()
                    .zip(iter::repeat(goal.ty.instantiate(&params, fact, program)))
            })
            .map(|(spec, ty)| ty.map(|ty| (spec, ty)))
            .collect::<Option<Vec<_>>>()?;
        Some(param_implications.chain(goal_implications).collect())
    }

    fn self_bounds(&self) -> &[Spec] {
        self.params.first().map_or(&[], |p| p)
    }
}

impl Fact {
    fn matches_bases(&self, q: &Fact) -> bool {
        (self.ty.base().is_none() || self.ty.base() == q.ty.base())
            && self.spec.base() == q.spec.base()
    }

    fn implications(&self, q: &Fact, program: &Program) -> Option<Vec<Fact>> {
        let mut params = vec![None; self.conditions.params.len()];
        params[0] = Some(q.ty.clone());

        if !self.spec.infer_params(&q.spec, &mut params)
            || !self.ty.infer_params(&q.ty, &mut params)
        {
            unreachable!();
        }

        let params = params
            .into_iter()
            .collect::<Option<Vec<_>>>()
            .expect("unreachable");

        let param_implications = self
            .conditions
            .implications(params.clone(), self, program)?;
        let spec_implications = self
            .spec
            .base()
            .implications(&q.spec, &q.ty, self, program)?;
        let type_implications = self
            .ty
            .base()
            .and_then(|def| def.implications(&q.ty, self, program))?;

        Some(
            param_implications
                .into_iter()
                .chain(spec_implications)
                .chain(type_implications)
                .map(|(spec, ty)| Fact {
                    ty,
                    spec,
                    conditions: q.conditions.clone(),
                    asoc_types: vec![],
                })
                .collect(),
        )
    }

    fn self_proves(&self, program: &Program) -> bool {
        let Ty::Param(ref param) = self.ty else {
        return false;
    };

        let mut bounds = self.conditions.params[param.index].clone();

        while !bounds.is_empty() {
            if bounds.iter().find(|b| *b == &self.spec).is_some() {
                return true;
            }

            bounds = bounds
                .into_iter()
                .flat_map(|b| b.expand_self(self, program))
                .flatten()
                .collect();
        }

        false
    }
}

impl Program {
    fn answer(&self, q: Fact) -> Option<Vec<Fact>> {
        if q.self_proves(self) {
            return Some(vec![]);
        }

        let mut matching = self
            .facts
            .iter()
            .filter(|fact| fact.matches_bases(&q))
            .cloned()
            .collect::<Vec<_>>();

        matching.retain(|f| {
            let Some(implications) = f.implications(&q, self) else { return dbg!(false); };
            implications.into_iter().all(|i| self.answer(i).is_some())
        });

        Some(matching).filter(|m| !m.is_empty())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! ty {
    (@$group:ident $(
        struct $name:ident $(where $(
            $params:tt$(: $bound:tt)?
        ),*)?
    )*) => {
        $(
            where_clause!(@params $($($params),*)?);
            let $name = TyDef {
                name: stringify!($name).into(),
                conditions: where_clause!(where $($($params$(: $bound)?),*)?),
            };
        )*
        let $group = vec![$($name.clone()),*];
    };
    ({$param:tt $trait:ident $name:ident}) => {{
        let asoc = $trait.get_asoc(stringify!($name))
            .expect("unknown asoc type").clone();
        let Ty::Param(param) = ty!($param) else {unreachable!("expected base param")};
        Ty::Param(Param {
            index: param.index,
            asoc: Some(AsocTy::Def(asoc)),
        })
    }};
    (Self) => {Ty::Param(Param::simple(0))};
    ($name:ident) => {$name.clone().into()};
    (($name:tt $($ty:tt)*)) => {
        Ty::Instance(Instance {
            def: ty!($name),
            params: vec![ty!($($ty)*)],
        })
    };
}

    macro_rules! bounds {
    (@$group:ident $(
        trait $name:ident $(where
            $($params:tt$(: $bounds:tt)?),*)?
        $({$($types:tt)*})?
    )+) => {
        $(
            where_clause!(@params $($($params),*)?);
            let $name = SpecDef {
                name: stringify!($name).to_string(),
                asoc_types: bounds!(@asocs $name $($($types)*)?),
                conditions: where_clause!(where $($($params$(: $bounds)?),*)?)
            };
        )+
        let $group = vec![$($name.clone()),*];
    };

    (@asocs) => {vec![]};
    (@asocs $spec:ident $(
        type $name:ident $(where
            $($params:tt$(: $bounds:tt)?),*)?
    )*) => {
        vec![$(
            AsocDef {
                name: stringify!($name).to_string(),
                spec: stringify!($spec).to_string(),
                conditions: where_clause!(where $($($params$(: $bounds)?),*)?)
            }
        ),*]
    };
    () => {
        vec![]
    };
    ($bound:ident) => {
        vec![$bound.clone().into()]
    };
    ([$($bound:tt)*]) => {
        vec![$(bounds!($bound).pop().unwrap()),*]
    };
    (($name:ident $($ty:tt)*)) => {
        vec![Instance {
            def: $name.clone(),
            params: vec![$(ty!($ty)),*],
        }.into()]
    };
}

    macro_rules! where_clause {
    (where) => {Where::default()};
    (where $($param:tt$(: $bound:tt)?),*) => {{
        #[allow(unused_mut)]
        let (mut params, mut goals) = (vec![vec![]], vec![]);
        $(
            let bounds: Vec<Spec> = bounds!($($bound)?);
            where_clause!(@inner $param: bounds, params, goals);
        )*
        Where {
            params,
            goals,
        }
    }};

    (@params $($param:tt),*) => {$(
        #[allow(unused_mut)]
        let mut _counter = 1;
        where_clause!(@params_inner _counter $param);
    )*};

    (@params_inner $counter:ident [$param:ident]) => {
        let $param = Ty::Param(Param::simple($counter));
        $counter += 1;
    };
    (@params_inner $counter:ident $ignored:tt) => {};

    (@inner Self: $bound:expr, $params:expr, $goals:expr) => {
        $params[0].extend($bound);
    };
    (@inner [$param:ident]: $bound:expr, $params:expr, $goals:expr) => {
        $params.push($bound);
    };
    (@inner $ty:tt: $bound:expr, $params:expr, $goals:expr) => {
        $goals.push(Goal {
            ty: ty!($ty),
            bounds: $bound,
        });
    };
}

    macro_rules! fact {
    (@$group:ident $(
        let $name:ident = impl $spec:tt for $ty:tt $(where
            $($params:tt$(: $bounds:tt)?),+)?
        $({$($types:tt)*})?
    )*) => {
        $(let $name = {
            where_clause!(@params $($($params),*)?);
            let spec = {
                let mut spec: Vec<Spec> = bounds!($spec);
                spec.pop().unwrap()
            };
            Fact {
                asoc_types: fact!(@@asocs spec $($($types)*)?),
                spec,
                ty: ty!($ty),
                conditions: where_clause!(where $($($params$(: $bounds)?),*)?)
            }
        };)*
        let $group = vec![$($name.clone()),*];
    };

    (@@asocs $spec:ident) => {vec![]};
    (@@asocs $spec:ident $(
        type $([$($param:ident)*])? $name:ident = $ty:tt;
    )*) => {{
        where_clause!(@params $($($($param),*)?),*);
        let mut types = vec![None; $spec.base().asoc_types.len()];
        $(
            let index = $spec.base().asoc_types.iter()
                .position(|t| t.name == stringify!($name)).unwrap();
            types[index] = Some(ty!($ty));
        )*
        types.into_iter().collect::<Option<_>>().expect("All types must be specified")
    }};
}

    macro_rules! test {
    ($prog:expr => $($name:literal $(($($expected:tt)*))? => { $($fact:tt)*})*) => {$(
        fact!(@_ign let fact = $($fact)*);
        assert_eq!($prog.answer(fact.clone()), test!(@res $($($expected)*)?), "{}", $name);
    )*};

    (@res) => {None};
    (@res $name:ident) => {Some(vec![$name.clone()])};
    (@res $expr:expr) => {Some($expr)};
}

    #[test]
    fn test() {
        ty! {
            @_types
            struct foo
            struct goo
            struct wrap where [_t]
        }

        bounds! {
            @specs
            trait bar
            trait unim
            trait baz where Self: bar
            trait qux where Self: bar, [_t]: baz {type foo}
            trait kix where Self: (qux _t), [_t]: baz, {Self qux foo}: unim
        }

        fact! {
            @facts
            let foo_bar_fact = impl bar for foo
            let wrap_bar_fact = impl bar for (wrap t) where [t]: bar
            let wrap_qux_fact = impl (qux t) for (wrap t) where [t]: baz {
                type foo = foo;}
            let baz_any_fact = impl baz for t where [t]: bar
            let kix_any_fact = impl (kix t) for t where [t]
        }

        let prog = Program {
            specs: specs.into_iter().map(|d| (d.name.clone(), d)).collect(),
            facts,
        };

        test! { prog =>
            "Simple unimplemented" => {impl bar for goo}
            "Simple implemented" (foo_bar_fact) => {impl bar for foo}
            "Generic type impl" (wrap_bar_fact) => {impl bar for (wrap foo)}
            "Naked parameter" => {impl bar for t where [t]}
            "Self proof" (vec![]) => {impl bar for t where [t]: bar}
            "Indirect self proof" (vec![]) => {impl bar for t where [t]: baz}
            "Complex generic" (wrap_qux_fact) => {impl (qux foo) for (wrap foo)}
            "Naked complex generic" (vec![]) => {impl (qux e) for t where [t]: (qux e), [e]}
            "Failed naked complex generic" => {impl (qux e) for t where [t]: baz, [e]}
            "Failed asociate type" (kix_any_fact) => {impl (kix foo) for foo}
        }
    }
}

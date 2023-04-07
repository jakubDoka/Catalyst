#![allow(incomplete_features)]
#![feature(specialization, default_free_fn)]

use std::{collections::HashMap, default::default, iter, sync::Arc, vec};

#[macro_export]
macro_rules! records {
    ($derive:tt $($record:item)*) => {
       $(
            #[derive $derive]
            $record
       )*
    };
}

// mod attempt1;

type Preds = Arc<Predicates>;

records! {
    (Clone, Debug, PartialEq, Eq, Hash)
    enum Node<T> {
        Instance(Instance<T>),
        Type(T),
    }

    struct Instance<T> {
        base: T,
        params: Vec<Ty>,
    }

    struct Spec {
        name: &'static str,
        predicates: Preds,
        asocs: Arc<Vec<Asoc>>,
    }


    enum Ty {
        Node(Node<BaseTy>),
        Param(Param),
    }

    struct Param {
        index: usize,
        asocs: Vec<Node<Asoc>>,
    }

    struct Asoc {
        name: &'static str,
        spec: &'static str,
        predicates: Preds,
    }

    struct Predicate {
        ty: Ty,
        specs: Vec<Node<Spec>>,
    }

    struct BaseTy {
        name: &'static str,
        predicates: Preds,
    }

    struct Question {
        ty: Ty,
        implements: Node<Spec>,
        implications: Preds,
    }

    struct Implementation {
        ty: Ty,
        asocs: Vec<Ty>,
        spec: Node<Spec>,
        predicates: Preds,
    }
}

impl<T> From<T> for Ty
where
    T: Into<Node<BaseTy>>,
{
    default fn from(t: T) -> Self {
        Self::Node(t.into())
    }
}

impl From<Param> for Ty {
    fn from(t: Param) -> Self {
        Self::Param(t)
    }
}

impl<T> From<T> for Node<T> {
    default fn from(t: T) -> Self {
        Self::Type(t)
    }
}

impl<T> From<Instance<T>> for Node<T> {
    fn from(t: Instance<T>) -> Self {
        Self::Instance(t)
    }
}

impl<T> Node<T> {
    fn base(&self) -> &T {
        match self {
            Self::Instance(i) => &i.base,
            Self::Type(t) => t,
        }
    }

    fn params(&self) -> &[Ty] {
        match self {
            Self::Instance(i) => &i.params,
            Self::Type(_) => &[],
        }
    }
}

impl Ty {
    fn base(&self) -> Option<&BaseTy> {
        match self {
            Self::Node(n) => Some(n.base()),
            Self::Param(_) => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Default)]
struct Predicates {
    param_count: usize,
    preds: Vec<Predicate>,
}

type Capabilities = HashMap<Param, Vec<Node<Spec>>>;

struct Solver {
    specs: HashMap<&'static str, Node<Spec>>,
    implementations: Vec<Implementation>,
}

impl Solver {
    fn new(specs: Vec<Spec>, implementations: Vec<Implementation>) -> Self {
        let specs = specs
            .into_iter()
            .map(|spec| (spec.name, spec.into()))
            .collect();
        Self {
            specs,
            implementations,
        }
    }

    fn answer<'a>(
        &'a self,
        question: &'a Question,
        mut capabilities: Result<&'a Capabilities, &'a mut Capabilities>,
    ) -> Result<&Implementation, Vec<Question>> {
        if let Err(ref mut capabilities) = capabilities {
            **capabilities = self.collect_question_implications(question);
        }
        self.implementations
            .iter()
            .filter(|i| Self::is_candidate(i, question))
            .map(move |i| {
                self.check_implementation(
                    i,
                    question,
                    capabilities.as_ref().map(|v| *v).unwrap_or_else(|e| *e),
                )
            })
            .reduce(|a, b| a.or(b))
            .unwrap_or(Err(vec![]))
    }

    fn is_candidate(implementation: &Implementation, question: &Question) -> bool {
        implementation.spec.base() == question.implements.base()
            && (implementation.ty.base().is_none()
                || implementation.ty.base() == question.ty.base())
    }

    fn param_implements(param: &Param, spec: &Node<Spec>, capabilities: &Capabilities) -> bool {
        capabilities
            .get(param)
            .map(|specs| specs.contains(spec))
            .unwrap_or(false)
    }

    fn collect_question_implications(&self, question: &Question) -> Capabilities {
        let mut implied = Capabilities::default();

        let params = iter::once(question.ty.clone())
            .chain((1..question.implications.param_count).map(|i| {
                Ty::Param(Param {
                    index: i,
                    asocs: vec![],
                })
            }))
            .collect::<Vec<_>>();

        self.collect_implications(&question.implications, &mut implied, &params);

        implied
    }

    fn collect_implications(
        &self,
        implications: &Predicates,
        implied: &mut Capabilities,
        params: &[Ty],
    ) -> Result<(), Vec<Question>> {
        for pred in &implications.preds {
            let ty = self.instantiate_ty(&pred.ty, params, implied)?;
            for spec in &pred.specs {
                let spec = self.instantiate(spec, params, implied)?;
                self.collect_spec_implications(&ty, &spec, implied);
            }
        }

        Ok(())
    }

    fn instantiate_ty(
        &self,
        ty: &Ty,
        params: &[Ty],
        capabilities: &Capabilities,
    ) -> Result<Ty, Vec<Question>> {
        Ok(match ty {
            Ty::Node(n) => Ty::Node(self.instantiate(n, params, capabilities)?),
            Ty::Param(p) => {
                let mut base = params[p.index].clone();
                if p.asocs.is_empty() {
                    return Ok(base);
                }

                match dbg!(&base) {
                    Ty::Node(_) => {
                        for a in p
                            .asocs
                            .iter()
                            .map(|a| self.instantiate(a, params, capabilities))
                        {
                            let a = a?;
                            let spec = self.specs[a.base().spec].clone();
                            let index = spec
                                .base()
                                .asocs
                                .iter()
                                .position(|sa| sa.name == a.base().name)
                                .unwrap();
                            let quest = Question {
                                ty: base.clone(),
                                implements: dbg!(spec),
                                implications: default(),
                            };
                            let implementation = self.answer(&quest, Ok(capabilities))?;
                            base = self.instantiate_ty(
                                &implementation.asocs[index],
                                params,
                                capabilities,
                            )?;
                        }

                        return Ok(base);
                    }
                    Ty::Param(param) => Ty::Param(Param {
                        index: param.index,
                        asocs: p
                            .asocs
                            .iter()
                            .map(|a| self.instantiate(a, params, capabilities))
                            .collect::<Result<_, _>>()?,
                    }),
                }
            }
        })
    }

    fn instantiate<T>(
        &self,
        node: &Node<T>,
        params: &[Ty],
        capabilities: &Capabilities,
    ) -> Result<Node<T>, Vec<Question>>
    where
        T: Clone,
    {
        Ok(match node {
            Node::Instance(i) => Node::Instance(Instance {
                base: i.base.clone(),
                params: i
                    .params
                    .iter()
                    .map(|t| self.instantiate_ty(t, params, capabilities))
                    .collect::<Result<_, _>>()?,
            }),
            Node::Type(t) => Node::Type(t.clone()),
        })
    }

    fn collect_spec_implications(&self, ty: &Ty, spec: &Node<Spec>, implied: &mut Capabilities) {
        match ty {
            Ty::Node(n) => self.collect_node_implications(n, spec, implied),
            Ty::Param(param) => implied.entry(param.clone()).or_default().push(spec.clone()),
        }
    }

    fn collect_node_implications(
        &self,
        ty: &Node<BaseTy>,
        spec: &Node<Spec>,
        implied: &mut Capabilities,
    ) {
        let mut params = vec![];
        params.push(Ty::Node(ty.clone()));
        params.extend_from_slice(ty.params());
        self.collect_implications(&ty.base().predicates, implied, &params);
        params.truncate(1);
        params.extend_from_slice(spec.params());
        self.collect_implications(&spec.base().predicates, implied, &params);

        let question = Question {
            ty: Ty::Node(ty.clone()),
            implements: spec.clone(),
            implications: default(),
        };

        let mut params = vec![params.into_iter().rev().last()];
        let condidates = self
            .implementations
            .iter()
            .filter(|i| Self::is_candidate(i, &question))
            .find_map(|i| {
                params.truncate(1);
                params.resize(i.predicates.param_count, None);

                Self::infer_params(&i.ty, &question.ty, &mut params)?;
                Self::infer_node_params(&i.spec, spec, &mut params)?;

                Some(i)
            });

        let Some(implementation) = condidates else {
            // we can safely ignore this, predicate checks will fail anyway
            return;
        };
        let params = params.into_iter().collect::<Option<Vec<_>>>().unwrap();
        self.collect_implications(&implementation.predicates, implied, &params);
    }

    fn infer_params(template: &Ty, from: &Ty, params: &mut [Option<Ty>]) -> Option<()> {
        match (template, from) {
            (Ty::Node(n1), Ty::Node(n2)) => Self::infer_node_params(n1, n2, params)?,
            (Ty::Param(p), ty) => {
                let param = params.get_mut(p.index)?;
                if let Some(ty) = param {
                    assert_eq!(ty, ty);
                } else {
                    *param = Some(ty.clone());
                }
            }
            _ => return None,
        }

        Some(())
    }

    fn infer_node_params<T: Eq>(
        template: &Node<T>,
        from: &Node<T>,
        params: &mut [Option<Ty>],
    ) -> Option<()> {
        match (template, from) {
            (Node::Instance(i1), Node::Instance(i2)) => {
                assert!(i1.base == i2.base);
                for (p1, p2) in i1.params.iter().zip(i2.params.iter()) {
                    Self::infer_params(p1, p2, params)?;
                }
            }
            (a, b) if a == b => {}
            _ => return None,
        }

        Some(())
    }

    fn check_implementation<'a>(
        &self,
        implementation: &'a Implementation,
        question: &Question,
        implied: &Capabilities,
    ) -> Result<&'a Implementation, Vec<Question>> {
        let mut unimplemented = vec![];
        let mut params = vec![None; implementation.predicates.param_count];
        params[0] = Some(question.ty.clone());
        Self::infer_params(&implementation.ty, &question.ty, &mut params).ok_or(vec![])?;
        Self::infer_node_params(&implementation.spec, &question.implements, &mut params)
            .ok_or(vec![])?;
        let mut params = params
            .into_iter()
            .collect::<Option<Vec<_>>>()
            .ok_or(vec![])?;

        self.check_predicates(
            &implementation.predicates,
            &params,
            implied,
            &mut unimplemented,
        );

        if let (Ty::Node(base), Ty::Node(reference_base)) = (&implementation.ty, &question.ty) {
            params.truncate(1);
            params.extend_from_slice(reference_base.params());
            self.check_predicates(
                &base.base().predicates,
                &params,
                implied,
                &mut unimplemented,
            );
        }

        params.truncate(1);
        params.extend_from_slice(question.implements.params());
        self.check_predicates(
            &question.implements.base().predicates,
            &params,
            implied,
            &mut unimplemented,
        );

        if unimplemented.is_empty() {
            Ok(implementation)
        } else {
            Err(unimplemented)
        }
    }

    fn check_predicates(
        &self,
        predicates: &Predicates,
        params: &[Ty],
        implied: &Capabilities,
        unimplemented: &mut Vec<Question>,
    ) {
        for pred in predicates.preds.iter() {
            let ty = match self.instantiate_ty(&pred.ty, &params, implied) {
                Ok(ty) => ty,
                Err(vec) => {
                    unimplemented.extend(vec);
                    continue;
                }
            };
            for spec in pred.specs.iter() {
                if let Ty::Param(ref param) = ty {
                    if !Self::param_implements(param, spec, implied) {
                        unimplemented.push(Question {
                            ty: ty.clone(),
                            implements: spec.clone(),
                            implications: default(),
                        });
                    }
                    continue;
                }
                let spec = match self.instantiate(spec, &params, implied) {
                    Ok(spec) => spec,
                    Err(vec) => {
                        unimplemented.extend(vec);
                        continue;
                    }
                };
                let question = Question {
                    ty: ty.clone(),
                    implements: spec,
                    implications: default(),
                };
                match self.answer(&question, Ok(implied)) {
                    Ok(_) => {}
                    Err(questions) => {
                        unimplemented.push(question);
                        unimplemented.extend(questions);
                    }
                }
            }
        }
    }
}

macro_rules! ty {
    (Self) => {
        Ty::Param(Param {
            index: 0,
            asocs: vec![],
        })
    };
    ($name:ident) => {
        Ty::from($name.clone())
    };

    (($name:ident $($param:tt)*)) => {
        Ty::from(Instance {
            base: $name.clone(),
            params: vec![$(ty!($param)),*],
        })
    };

    ({$param:ident $(::$spec:ident::$asoc:tt)*}) => {
        Ty::Param(Param {
            index: ty!(@asoc $param),
            asocs: vec![$(ty!(@asoc $spec::$asoc)),*],
        })
    };

    (@asoc Self) => {0};
    (@asoc $param:ident) => {$param.index};
    (@asoc $spec:ident::$name:ident) => {
        Node::from(Asoc {
            spec: stringify!($spec),
            name: stringify!($name),
            predicates: default(),
        })
    };
    (@asoc $spec:ident::($name:ident $($param:tt)*)) => {
        Node::from(Instance {
            base: $name.clone(),
            params: vec![$(ty!($param)),*],
        })
    };
}

macro_rules! spec {
    ([$($spec:tt)*]) => {vec![$(spec!(@inner $spec)),*]};
    ($spec:tt) => {vec![spec!(@inner $spec)]};

    (@inner $name:ident) => {
        Node::from($name.clone())
    };

    (@inner ($name:ident $($param:tt)*)) => {
        Node::from(Instance {
            base: $name.clone(),
            params: vec![$(ty!($param)),*],
        })
    };
}

macro_rules! generics {
    ($count:ident ($index:expr)) => {let $count = $index;};

    ($count:ident ($index:expr) $name:ident $($rest:ident)*) => {
        #[allow(unused)]
        let $name = Param {
            index: $index,
            asocs: vec![],
        };
        generics!($count ($index + 1) $($rest)*);
    };
}

macro_rules! predicates {
    ($generic_count:ident) => {predicates!($generic_count where ())};
    ($generic_count:ident where ($(
        $ty:tt: $spec:tt
    ),*$(,)?)) => {{
        let preds = vec![$(
            Predicate {
                ty: ty!($ty),
                specs: spec!($spec),
            }
        ),*];

        Arc::new(Predicates {
            param_count: $generic_count,
            preds,
        })
    }};
}

macro_rules! types {
    (let $group:ident = [$(
        $name:ident $([$($generics:ident)*])? $(where $pred:tt)?,
    )+]) => {
        $(
            generics!(generic_count (1) $($($generics)*)?);
            let $name = BaseTy {
                name: stringify!($name),
                predicates: predicates!(generic_count $(where $pred)?),
            };
        )*
        let $group = vec![$($name.clone()),+];
    };
}

macro_rules! asocs {
    ($generic_count:ident $spec:ident $(
        $name:ident
            $([$($generics:ident)*])?
            $(where $pred:tt)?
    ),* $(,)?) => {
        Arc::new(vec![$({
            generics!(generic_count ($generic_count) $($($generics)*)?);
            Asoc {
                name: stringify!($name),
                spec: stringify!($spec),
                predicates: predicates!(generic_count $(where $pred)?),
            }
        }),*])
    };

    (@impl $generic_count:ident $(
        $name:ident
            $([$($generics:ident)*])?
            = $ty:tt
    ),* $(,)?) => {
        vec![$({
            generics!(_generic_count ($generic_count) $($($generics)*)?);
            ty!($ty)
        }),*]
    };
}

macro_rules! specs {
    (let $group:ident = [$(
        $name:ident
            $([$($generics:ident)*])?
            $(where $pred:tt)?
            $({$($assoc:tt)*})?,
    )+]) => {
        $(
            generics!(generic_count (1) $($($generics)*)?);
            let $name = Spec {
                name: stringify!($name),
                predicates: predicates!(generic_count $(where $pred)?),
                asocs: asocs!(generic_count $name $($($assoc)*)?),
            };
        )*
        let $group = vec![$($name.clone()),+];
    };
}

macro_rules! implementations {
    (
        let $group:ident = [$(
            $name:ident $([$($generics:tt)*])? impl $spec:tt for $ty:tt
            $(where $pred:tt)?
            $({$($assoc:tt)*})?,
        )+]
    ) => {
        $(
            generics!(generic_count (1) $($($generics)*)?);
            let $name = Implementation {
                spec: spec!(@inner $spec),
                ty: ty!($ty),
                predicates: predicates!(generic_count $(where $pred)?),
                asocs: asocs!(@impl generic_count $($($assoc)*)?),
            };
        )+
        let $group = vec![$($name.clone()),+];
    };
}

macro_rules! question {
    (
        $([$($generics:ident)*])?
        impl $spec:tt for $ty:tt
        $(where $pred:tt)?
    ) => {{
        generics!(generic_count (1) $($($generics)*)?);
        Question {
            implements: spec!(@inner $spec),
            ty: ty!($ty),
            implications: predicates!(generic_count $(where $pred)?),
        }
    }};
}

macro_rules! tests {
    (
        $(
            $name:literal $(($($result:tt)*))? =>
                {$($question:tt)*},
        )*
    ) => {{
        vec![
            $(
                ($name, question!($($question)*), tests!(@result $($($result)*)?))
            ),*
        ]
    }};

    (@result) => {None};
    (@result $result:expr) => {Some($result)};
}

#[test]
fn test() {
    specs! {
        let specs = [
            bar,
            unim,
            store [t],
            baz where (Self: bar),
            qux [t] where (Self: bar, t: bar) {foo},
            kix [t] where (Self: (qux t), t: baz, {Self::qux::foo}: unim),
            cup {foo where (Self: bar)},
            gup where (Self: cup, {Self::cup::foo}: unim),
            model {id},
            reportitory [m] where (m: model),
            to_string,
            into [t],
        ]
    }

    types! {
        let _types = [
            foo,
            goo,
            lep,
            i32,
            my_model,
            pool [f],
            repo [t],
            wrap [t] where (t: bar),
        ]
    }

    implementations! {
        let facts = [
            foo_bar_fact impl bar for foo,
            wrap_bar_fact [t] impl bar for (wrap t) where (t: bar),
            wrap_qux_fact [t] impl (qux t) for (wrap t) where (t: baz) {foo = foo},
            baz_any_fact [t] impl baz for t where (t: bar),
            kix_any_fact [t] impl (kix t) for t,
            cup_fact impl cup for goo {foo = foo},
            cup_lep_fact impl cup for lep {foo = lep},
            unim_lep_fact impl unim for lep,
            store_for_pool [t f] impl (store t) for (pool f) where (f: (into t), t: to_string),
            model_for_my_model impl model for my_model {id = i32},
            repo_for_repo [f] impl (reportitory my_model) for (repo f) where (f: (store my_model)),
            into_bar_for_goo impl (into foo) for goo,
            to_string_for_bar impl to_string for foo,
            asoc_expansion [t] impl gup for (wrap t) where ((wrap {Self::cup::foo}): unim),
            cur_for_wrap [t] impl cup for (wrap t) {foo = i32},
            unim_for_wrap [t] impl unim for (wrap t) where (t: unim),
            unim_for_i32 impl unim for i32,
            bar_for_i32 impl bar for i32,
        ]
    }

    let prog = Solver::new(specs, facts);

    let tests = tests! {
        "Simple unimplemented" => {impl bar for goo},
        "Simple implemented" (foo_bar_fact) => {impl bar for foo},
        "Generic type impl" (wrap_bar_fact) => {impl bar for (wrap foo)},
        "Naked parameter" => {[t] impl bar for t},
        "Self proof" => {[t] impl bar for t where (t: bar)},
        "Indirect self proof" => {[t] impl bar for t where (t: baz)},
        "Complex generic" (wrap_qux_fact) => {impl (qux foo) for (wrap foo)},
        "Naked complex generic" => {[t e] impl (qux e) for t where (t: (qux e))},
        "Failed naked complex generic" => {[t e] impl (qux e) for t where (t: baz)},
        "Failed asociate type" => {impl (kix foo) for foo},
        "Success asociate type" (cup_fact) => {impl cup for goo},
        "Failed asociate type 2" => {impl gup for goo},
        "Something unique eh" (store_for_pool) => {impl (store foo) for (pool goo)},
        "Something unique failed" => {impl (store goo) for (pool goo)},
        "Asoc Expansion" (asoc_expansion) => {impl gup for (wrap foo)},
    };

    let mut failed = false;
    for (name, question, result) in tests {
        let mut cap = Capabilities::new();
        let res = prog.answer(&question, Err(&mut cap));
        if res.as_ref().ok().map(|&a| a) != result.as_ref() {
            failed = true;
            println!("Test {} failed", name);
            println!("Expected: {:#?}", result);
            println!("Got: {:#?}", res);
            if let Err(err) = res {
                println!("Test {} failed", name);
                println!("Error: {:#?}", err);
            }
        }
    }
    assert!(!failed);
}

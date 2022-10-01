use std::{
    hash::{BuildHasher, Hash, Hasher},
    mem::transmute,
};

use storage::*;

pub struct BoundSolver {
    predicates: PushMap<Predicate>,
    proofs: Map<Proof, VRef<SolSource>>,
    separators: Separators,
    goals: Goals,
    params: Params,
}

struct Predicate {
    separators: VSlice<u32>,
    params: VSlice<Goal>,
    term: Term,
    goal: Goal,
    source: VRef<SolSource>,
}

struct Term {
    term: VRef<SolTerm>,
    params: VSlice<Param>,
}

struct Goal {
    goal: VRef<SolGoal>,
    params: VSlice<Param>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct Proof {
    term: VRef<SolTerm>,
    goal: VRef<SolGoal>,
}

#[allow(clippy::derive_hash_xor_eq)]
impl Hash for Proof {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        unsafe {
            transmute::<_, u64>(*self).hash(state);
        }
    }
}

impl SpecialHash for Proof {
    type BuildHasher = SolBuildHasher;
}

#[derive(Clone, Copy, Default)]
struct SolBuildHasher;

impl BuildHasher for SolBuildHasher {
    type Hasher = SolHasher;

    fn build_hasher(&self) -> Self::Hasher {
        SolHasher(0)
    }
}

struct SolHasher(u64);

impl Hasher for SolHasher {
    fn finish(&self) -> u64 {
        self.0
    }

    fn write(&mut self, bytes: &[u8]) {
        self.0 = unsafe { *(bytes.as_ptr() as *const u64) }
    }
}

type Separators = BumpMap<u32>;
type Goals = BumpMap<Goal>;
type Params = BumpMap<Param>;

enum Param {
    General(u32),
    Concrete(),
}

#[rustc_layout_scalar_valid_range_end(4294967294)]
#[repr(transparent)]
pub struct A(u32);

pub struct SolTerm;
pub struct SolGoal;
pub struct SolSource;

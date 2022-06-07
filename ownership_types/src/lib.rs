use storage::*;
use typec_types::*;

pub struct OwnershipContext {
    pub move_lookup: Set,
    pub scopes: Vec<Tir>,
    pub spawned: EntitySet<Tir>,
    pub disjoint: FramedStack<Tir>,
    pub to_drop: FramedStack<Tir>,
    pub bound_ids: FramedStack<ID>,
}

impl OwnershipContext {
    pub fn new() -> Self {
        Self {
            move_lookup: Set::new(),
            scopes: Vec::new(),
            spawned: EntitySet::new(),
            disjoint: FramedStack::new(),
            to_drop: FramedStack::new(),
            bound_ids: FramedStack::new(),
        }
    }

    pub fn propagate_drop(&mut self, tir: Tir, data: &mut OwnershipData) {
        for &disjoint in self.disjoint.top_frame() {
            data.drops[disjoint].outer_drops.push(tir, &mut data.lists);
        }
    }

    pub fn start_scope(&mut self, root: Tir) {
        self.scopes.push(root);
        self.to_drop.mark_frame();
    }

    pub fn end_scope(&mut self) {        
        self.to_drop.pop_frame();
        self.scopes.pop().unwrap();
    }

    pub fn start_branch(&mut self, roots: &[Tir]) {
        self.disjoint.mark_frame();
        self.disjoint.extend(roots);
        self.bound_ids.mark_frame();
    }

    pub fn end_branch(&mut self) {
        self.disjoint.pop_frame();
        for &id in self.bound_ids.top_frame() {
            self.move_lookup.remove(id);
        }
    }

    pub fn pop_branches(&mut self, count: usize) {
        for _ in 0..count {
            for &id in self.bound_ids.top_frame() {
                self.move_lookup.insert(id);
            }   
            self.bound_ids.pop_frame();
        }
    }

    pub fn clear(&mut self) {
        self.move_lookup.clear();
        self.scopes.clear();
        self.spawned.clear();
        self.disjoint.clear();
        self.to_drop.clear();
        self.bound_ids.clear();
    }
}

pub struct OwnershipData {
    pub drops: SecondaryMap<Tir, Drops>,
    pub lists: ListPool<Tir>,
}

impl OwnershipData {
    pub fn new() -> Self {
        Self {
            drops: SecondaryMap::new(),
            lists: ListPool::new(),
        }
    }

    pub fn clear(&mut self) {
        self.drops.clear();
        self.lists.clear();
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Drops {
    pub outer_drops: EntityList<Tir>,
    pub inner_drops: EntityList<Tir>,
}

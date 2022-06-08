use storage::*;
use typec_types::*;

pub struct OwnershipContext {
    pub loop_depth: Vec<u32>,
    pub move_lookup: Set,
    pub scopes: Vec<(Tir, bool)>,
    pub spawned: SecondaryMap<Tir, u32>,
    pub disjoint: FramedStack<Tir>,
    pub to_drop: FramedStack<Tir>,
    pub bound_ids: FramedStack<ID>,
    pub attached_drops: SecondaryMap<Tir, EntityList<Tir>>,
    pub slices: ListPool<Tir>,
}

impl OwnershipContext {
    pub fn new() -> Self {
        Self {
            loop_depth: Vec::new(),
            move_lookup: Set::new(),
            scopes: Vec::new(),
            spawned: SecondaryMap::new(),
            disjoint: FramedStack::new(),
            to_drop: FramedStack::new(),
            bound_ids: FramedStack::new(),
            attached_drops: SecondaryMap::new(),
            slices: ListPool::new(),
        }
    }

    pub fn start_loop(&mut self) {
        self.loop_depth.push(self.scopes.len() as u32);
    }

    pub fn end_loop(&mut self) {
        self.loop_depth.pop().unwrap();
    }

    pub fn loop_depth(&self) -> Option<usize> {
        self.loop_depth.last().map(|&d| d as usize)
    }

    pub fn spawn(&mut self, tir: Tir) {
        self.spawned[tir] = self.scopes.len() as u32;
    }

    pub fn is_spawned(&self, tir: Tir) -> bool {
        self.spawned[tir] != 0
    }

    pub fn propagate_drop(&mut self, tir: Tir) {
        for &disjoint in self.disjoint.top_frame() {
            self.attached_drops[disjoint].push(tir, &mut self.slices);
        }
    }

    pub fn start_scope(&mut self, root: Tir, terminating: bool) {
        self.scopes.push((root, terminating));
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
        self.loop_depth.clear();
        self.move_lookup.clear();
        self.scopes.clear();
        self.spawned.clear();
        self.disjoint.clear();
        self.to_drop.clear();
        self.bound_ids.clear();
        self.attached_drops.clear();
        self.slices.clear();
    }
}
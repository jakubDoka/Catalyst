use std::collections::hash_map::Entry;

use storage::*;
use typec_types::*;

pub struct OwnershipContext {
    pub loop_depths: Vec<u32>,
    pub move_lookup: Map<(Validity, u32)>,
    pub scopes: Vec<Tir>,
    pub spawned: SecondaryMap<Tir, Spawn>,
    pub disjoint: FramedStack<Tir>,
    pub to_drop: FramedStack<Tir>,
    pub bound_ids: FramedStack<(ID, Validity)>,
    pub attached_drops: SecondaryMap<Tir, EntityList<Tir>>,
    pub slices: ListPool<Tir>,
}

impl OwnershipContext {
    pub fn new() -> Self {
        Self {
            loop_depths: Vec::new(),
            move_lookup: Map::new(),
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
        self.loop_depths.push(self.scopes.len() as u32);
    }

    pub fn end_loop(&mut self) {
        self.loop_depths.pop().unwrap();
    }

    pub fn terminating_since(&self, depth: usize, tir_data: &TirData) -> bool {
        let mut prev = self.scopes.len();
        for &loop_depth in self.loop_depths.iter().rev() {
            if depth > loop_depth as usize {
                break;
            }

            let flags = self.scopes[loop_depth as usize..prev].iter()
                .fold(TirFlags::empty(), |acc, &tir| acc | {
                    tir_data.ents[tir].flags
                });

            if flags.terminal_flags() != TirFlags::TERMINATING {
                return false;
            }

            prev = loop_depth as usize;
        } 

        true
    }

    pub fn loop_depth(&self) -> Option<usize> {
        self.loop_depths.last().map(|&d| d as usize)
    }

    pub fn spawn(&mut self, tir: Tir) {
        self.spawned[tir].level = self.scopes.len() as u32;
    }

    pub fn is_spawned(&self, tir: Tir) -> bool {
        self.spawned[tir].level != u32::MAX
    }

    pub fn propagate_drop(&mut self, tir: Tir) {
        for &disjoint in self.disjoint.top_frame() {
            self.attached_drops[disjoint].push(tir, &mut self.slices);
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
        for &(id, _) in self.bound_ids.top_frame() {
            self.move_lookup.remove(id);
        }
    }

    pub fn register(&mut self, id: ID, validity: Validity) {
        match self.move_lookup.entry(id) {
            Entry::Occupied(mut entry) => {
                entry.get_mut().0 = validity;
                self.bound_ids.top_frame_mut()[entry.get().1 as usize].1 = validity;
            },
            Entry::Vacant(entry) => {
                let len = self.bound_ids.top_frame().len();
                entry.insert((validity, len as u32));
                self.bound_ids.push((id, validity));
            },
        }
    }

    pub fn pop_branches(&mut self, count: usize) {
        let ids = (0..count)
            .flat_map(|_| self.bound_ids.nth_frame(count))
            .cloned()
            .collect::<Vec<_>>();  
        
        (0..count).for_each(|_| self.bound_ids.pop_frame());

        for (id, validity) in ids {
            self.register(id, validity);
        }
    }

    pub fn clear(&mut self) {
        self.loop_depths.clear();
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Validity {
    Valid,
    Partial,
    Invalid,
}

impl Validity {
    pub fn merge(&mut self, other: Self) {
        todo!()
    }

    pub fn is_valid(&self) -> bool {
        *self == Validity::Valid
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Spawn {
    pub level: u32,
}

impl Default for Spawn {
    fn default() -> Self {
        Self {
            level: u32::MAX,
        }
    }
}
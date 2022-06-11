#![feature(let_chains)]

pub mod error;

pub use error::OwError;

use storage::*;
use typec_types::*;

pub struct OwnershipContext {
    pub scope: OwnershipScope,
    pub ownerships: PrimaryMap<Ownership, OwnershipEnt>,
    pub drops: SecondaryMap<Tir, DropNodeList>,
    pub drop_nodes: StackMap<DropNodeList, DropNodeEnt, DropNode>,
    pub seen: SecondaryMap<Tir, ID>,
    pub currently_accessed: FramedStack<Access>,
    pub branch_ids: Vec<u32>,
    pub loops: Vec<(Tir, u32)>,
}

impl OwnershipContext {
    pub const NO_OWNERSHIP: Ownership = Ownership(0);

    pub fn new() -> Self {
        Self {
            scope: OwnershipScope::new(),
            ownerships: PrimaryMap::new(),
            drops: SecondaryMap::new(),
            drop_nodes: StackMap::new(),
            seen: SecondaryMap::new(),
            currently_accessed: FramedStack::new(),
            branch_ids: Vec::new(),
            loops: Vec::new(),
        }
    }

    pub fn start_loop(&mut self, tir: Tir) {
        self.loops.push((tir, self.scope.level() as u32));
    }

    pub fn end_loop(&mut self) {
        self.loops.pop();
    }

    pub fn loop_level(&self, tir: Tir) -> usize {
        self.loops
            .iter()
            .rev()
            .find_map(|&(pushed, level)| (pushed == tir).then_some(level))
            .unwrap() as usize
    }

    pub fn push_current_access(&mut self, ownership: Ownership, tir: Tir) {
        self.currently_accessed.push(Access {
            ownership,
            branch_id: self.branch_ids.last().unwrap_or(&0).clone(),
            tir,
        });
    }

    pub fn mark_current_access_frame(&mut self) {
        self.mark_current_access_frame_low(0);
    }

    pub fn mark_current_access_frame_low(&mut self, branch_id: u32) {
        self.currently_accessed.mark_frame();
        self.branch_ids.push(branch_id);
    }

    pub fn pop_current_access_frame(&mut self) {
        self.currently_accessed.pop_frame();
        self.branch_ids.pop();
    }

    pub fn push(&mut self, mut ent: OwnershipEnt) -> Ownership {
        ent.level = self.scope.level() as u32;
        ent.loop_level = self.currently_accessed.frame_count() as u32;
        self.ownerships.push(ent)
    }

    pub fn mark_frame(&mut self) {
        self.scope.mark_frame();
        // self.currently_accessed.mark_frame();
    }

    pub fn pop_frame(&mut self) {
        self.scope.pop_frame(&mut self.ownerships);
        // self.currently_accessed.pop_frame();
    }

    pub fn push_item(&mut self, types: &Types, ent: OwnershipEnt) {
        if !types.may_drop(ent.ty) {
            return;
        }

        let id = ent.id;
        let ownership = self.push(ent);
        self.scope.push(id, ownership);
    }

    pub fn clear(&mut self) {
        self.scope.clear();
        self.ownerships.clear();
        self.ownerships.push(OwnershipEnt {
            ty: Ty(0),
            ..Default::default()
        });
        self.drops.clear();
        self.drop_nodes.clear();
        self.seen.clear();
        self.currently_accessed.clear();
        self.branch_ids.clear();
        self.loops.clear();
    }
}

#[derive(Default, Clone, Copy, Debug)]
pub struct Access {
    pub ownership: Ownership,
    pub branch_id: u32,
    pub tir: Tir,
}

#[derive(Clone, Copy, Debug)]
pub struct DropNodeEnt {
    pub meta: Result<Tir, TyComp>,
    pub children: DropNodeList,
}

gen_entity!(DropNode);
gen_entity!(DropNodeList);

pub struct OwnershipScope {
    map: Map<Ownership>,
    items: FramedStack<Item>,
}

impl OwnershipScope {
    pub fn new() -> Self {
        Self {
            map: Map::new(),
            items: FramedStack::new(),
        }
    }

    pub fn all_items_from_frame(&self, n: usize) -> impl Iterator<Item = Ownership> + '_ {
        self.items.iter_from_frame_inv(n).map(|i| i.ownership)
    }

    pub fn all_items(&self) -> impl Iterator<Item = Ownership> + '_ {
        self.items.iter().map(|i| i.ownership)
    }

    pub fn top_items(&self) -> impl Iterator<Item = Ownership> + '_ {
        self.items.top_frame().iter().map(|i| i.ownership)
    }

    pub fn level(&self) -> usize {
        self.items.frame_count()
    }

    pub fn get(&self, id: ID) -> Ownership {
        self.map
            .get(id)
            .copied()
            .unwrap_or(OwnershipContext::NO_OWNERSHIP)
    }

    pub fn get_intermediate(&self, id: ID) -> Option<Ownership> {
        self.map.get(id).cloned()
    }

    pub fn mark_frame(&mut self) {
        self.items.mark_frame();
    }

    fn push(&mut self, id: ID, ownership: Ownership) {
        self.items.push(Item {
            id,
            ownership,
            shadow: self.map.insert(id, ownership),
        });
    }

    pub fn pop_frame(&mut self, ownerships: &mut PrimaryMap<Ownership, OwnershipEnt>) {
        for &Item {
            id,
            shadow,
            ownership,
            ..
        } in self.items.top_frame()
        {
            if let Some(shadow) = shadow {
                if !ownerships[ownership].variable {
                    ownerships[shadow].last_move = ownerships[ownership].last_move;
                }
                self.map.insert(id, shadow);
            } else {
                self.map.remove(id);
            }
        }
        self.items.pop_frame();
    }

    pub fn clear(&mut self) {
        self.map.clear();
        self.items.clear();
    }
}

struct Item {
    id: ID,
    ownership: Ownership,
    shadow: Option<Ownership>,
}

#[derive(Default, Debug)]
pub struct OwnershipEnt {
    pub tir: PackedOption<Tir>,
    pub ty: Ty,
    pub last_move: PackedOption<Tir>,
    pub behind_pointer: bool,
    pub variable: bool,
    pub level: u32,
    pub loop_level: u32,
    pub id: ID,
}

gen_entity!(Ownership);

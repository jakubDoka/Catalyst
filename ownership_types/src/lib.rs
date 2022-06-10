use storage::*;
use typec_types::{Tir, Ty};

pub struct OwnershipContext {
    pub scope: OwnershipScope,
    pub ownerships: PrimaryMap<Ownership, OwnershipEnt>,
    pub drops: SecondaryMap<Tir, DropNodeList>,
    pub drops_nodes: StackMap<DropNodeList, DropNodeEnt, DropNode>,
    pub seen: SecondaryMap<Tir, ID>,
    pub currently_accessed: FramedStack<(Ownership, u32)>,
    pub branch_ids: Vec<u32>,
}

impl OwnershipContext {
    pub fn new() -> Self {
        Self {
            scope: OwnershipScope::new(),
            ownerships: PrimaryMap::new(),
            drops: SecondaryMap::new(),
            drops_nodes: StackMap::new(),
            seen: SecondaryMap::new(),
            currently_accessed: FramedStack::new(),
            branch_ids: Vec::new(),
        }
    }

    pub fn push_current_access(&mut self, ownership: Ownership) {
        self.currently_accessed
            .push((ownership, self.branch_ids.last().unwrap_or(&0).clone()));
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

    pub fn push_item(&mut self, ent: OwnershipEnt, intermediate: bool) {
        let id = ent.id;
        let ownership = self.push(ent);
        self.scope.push(id, ownership, intermediate);
    }

    pub fn clear(&mut self) {
        self.scope.clear();
        self.ownerships.clear();
        self.drops.clear();
        self.drops_nodes.clear();
        self.seen.clear();
        self.currently_accessed.clear();
    }
}

#[derive(Default, Clone, Copy, Debug)]
pub struct DropNodeEnt {
    pub drop: bool,
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
        self.items
            .iter_from_frame_inv(n)
            .filter(|i| !i.intermediate)
            .map(|i| i.ownership)
    }

    pub fn all_items(&self) -> impl Iterator<Item = Ownership> + '_ {
        self.items
            .iter()
            .filter(|i| !i.intermediate)
            .map(|i| i.ownership)
    }

    pub fn top_items(&self) -> impl Iterator<Item = Ownership> + '_ {
        self.items
            .top_frame()
            .iter()
            .filter(|i| !i.intermediate)
            .map(|i| i.ownership)
    }

    pub fn level(&self) -> usize {
        self.items.frame_count()
    }

    pub fn get(&self, id: ID) -> Option<Ownership> {
        self.map.get(id).cloned()
    }

    pub fn mark_frame(&mut self) {
        self.items.mark_frame();
    }

    fn push(&mut self, id: ID, ownership: Ownership, intermediate: bool) {
        self.items.push(Item {
            id,
            ownership,
            intermediate,
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
                ownerships[shadow].moved = ownerships[ownership].moved;
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
    intermediate: bool,
}

#[derive(Default, Debug)]
pub struct OwnershipEnt {
    pub tir: PackedOption<Tir>,
    pub ty: Ty,
    pub moved: bool,
    pub behind_pointer: bool,
    pub level: u32,
    pub loop_level: u32,
    pub id: ID,
}

gen_entity!(Ownership);

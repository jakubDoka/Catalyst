/*
    let st = { a: 1, b: 2 };

    let res = if true {
        if false {
            st.a
        } else {
            st.b
        }
    } else {
        st.a
    };
*/

use storage::*;
use typec_types::*;

use crate::state::OwnershipSolver;

impl OwnershipSolver<'_> {
    pub fn spawn(&mut self, target: Tir) -> Value {
        let ty = self.tir_data.ents[target].ty;
        todo!("spawn({})", ty);
    }

    pub fn child(&mut self, branch: Tir, on: Value, index: usize, sector: u32) -> Value {
        let oc = &mut self.ownership_context;
        if oc.values[on].children.is_reserved_value() {
            let size = self.types.item_count(oc.values[on].ty, self.ty_comps);
            oc.values[on].children = oc.values.alloc(size, Default::default());
        }

        let value = oc.values.key_of(oc.values[on].children, index).unwrap();

        if oc.values[value].ty.is_reserved_value() {
            let ty = self.types.item_ty(oc.values[on].ty, index, self.ty_comps);
            oc.values[value].ty = ty;
            let ownership = Ownership {
                sector,
                level: oc.level,
                taken: 0,
                branch,
            };
            oc.values[value].ownership.push(ownership, &mut oc.ownerships);
        }

        value
    }

    pub fn ownership_of(&mut self, tir: Tir) -> Value {
        self.ownership_context.mapping[tir]
    }

    pub fn move_out(&mut self, branch: Tir, value: Value, sector: u32) -> Result<(), OwnershipError> {
        let max = self.ownership_context.values
            .len_of(self.ownership_context.values[value].children);
        let current = self.current_ownership(value, branch, sector);
        current.move_out(max as u32)?;

        let mut value = self.ownership_context.values[value].parent.expand();
        while let Some(current) = value {
            value = self.ownership_context.values[current].parent.expand();
            let current = self.current_ownership(current, branch, sector);
            if current.partially_move_out() {
                break;
            }
        }

        Ok(())
    }

    // pub fn put_back(&mut self, value: Value, sector: u32) -> OwnershipAction {
    //     let max = self.values.len_of(self.values[value].children);
    //     let current = self.current_ownership(value, sector);
    //     let (ownership_result, done) = current.move_in(max as u32);

    //     if !done {
    //         let mut value = self.values[value].parent.expand();
    //         while let Some(current) = value {
    //             value = self.values[current].parent.expand();
    //             let current = self.current_ownership(current, sector);
    //             if current.partially_move_in() {
    //                 break;
    //             }
    //         }
    //     }

    //     ownership_result
    // }

    fn current_ownership(&mut self, value: Value, branch: Tir, sector: u32) -> &mut Ownership {
        let oc = &mut self.ownership_context;
        let ownership = oc.values[value]
            .ownership
            .as_slice(&mut oc.ownerships)
            .last()
            .unwrap()
            .clone();
        if ownership.sector != sector || ownership.level != oc.level || ownership.branch != branch {
            let value_ent = &mut oc.values[value];
            let parent = value_ent
                .ownership
                .as_slice(&mut oc.ownerships)
                .iter()
                .rev()
                .find(|ownership| ownership.level + 1 == oc.level)
                .unwrap()
                .clone();

            let ownership = Ownership {
                sector,
                level: oc.level,
                ..parent
            };
            value_ent.ownership.push(ownership, &mut oc.ownerships);
        }

        oc.values[value]
            .ownership
            .as_mut_slice(&mut oc.ownerships)
            .last_mut()
            .unwrap()
    }
}

pub struct OwnershipContext {
    pub level: u32,
    pub mapping: SecondaryMap<Tir, Value>,
    pub values: StackMap<ValueList, ValueEnt, Value>,
    pub ownerships: ListPool<Ownership>,
}

impl OwnershipContext {
    pub fn new() -> Self {
        Self {
            level: 0,
            mapping: SecondaryMap::new(),
            values: StackMap::new(),
            ownerships: ListPool::new(),
        }
    }

    pub fn clear(&mut self) {
        self.level = 0;
        self.mapping.clear();
        self.values.clear();
        self.ownerships.clear();
    }
}

pub enum OwnershipAction {
    Drop,
    PartialDrop,
    Nothing,
}

pub enum OwnershipError {
    Moved,
    PartiallyMoved,
}

#[derive(Clone, Copy, Debug, Default)]
pub struct ValueEnt {
    pub ty: Ty,
    pub ownership: EntityList<Ownership>,
    pub parent: PackedOption<Value>,
    pub children: ValueList,
}

#[derive(Debug, Clone, Default, Copy, PartialEq, Eq)]
pub struct Ownership {
    sector: u32,
    level: u32,
    taken: u32,
    branch: Tir,
}

impl Ownership {
    fn move_out(&mut self, max: u32) -> Result<(), OwnershipError> {
        if self.taken != 0 {
            if self.taken == max {
                Err(OwnershipError::Moved)
            } else {
                Err(OwnershipError::PartiallyMoved)
            }
        } else {
            self.taken = max;
            Ok(())
        }
    }

    fn partially_move_out(&mut self) -> bool {
        self.taken += 1;
        self.taken - 1 == 0
    }

    fn move_in(&mut self, max: u32) -> (OwnershipAction, bool) {
        if self.taken == 0 {
            (OwnershipAction::Drop, true)
        } else {
            let res = if self.taken == max {
                OwnershipAction::Nothing
            } else {
                OwnershipAction::PartialDrop
            };
            self.taken = 0;
            (res, false)
        }
    }

    fn partially_move_in(&mut self) -> bool {
        self.taken -= 1;
        self.taken != 0
    }
}

impl EntityRef for Ownership {
    fn new(index: usize) -> Self {
        Self {
            level: index as u32,
            ..Default::default()
        }
    }

    fn index(self) -> usize {
        self.level as usize
    }
}

impl ReservedValue for Ownership {
    fn reserved_value() -> Self {
        Self {
            level: u32::MAX,
            ..Default::default()
        }
    }

    fn is_reserved_value(&self) -> bool {
        self.level == u32::MAX
    }
}

gen_entity!(ValueList);
gen_entity!(Value);

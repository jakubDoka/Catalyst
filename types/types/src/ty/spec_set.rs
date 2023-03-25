use crate::*;
use storage::*;

pub type SpecSetParamRepr = TyParamIdx;

#[derive(Default)]
pub struct SpecSet {
    storage: Vec<SpecSetItem>,
    temp: Vec<SpecSetItem>,
    mapping: Map<SpecSetParamRepr, SpecSetParamRepr>,
}

impl SpecSet {
    pub fn extend(
        &mut self,
        dest: SpecSetParamRepr,
        source: SpecSetParamRepr,
        asoc_ty: OptFragRef<AsocTy>,
        specs: impl IntoIterator<Item = Spec>,
    ) {
        specs
            .into_iter()
            .map(|spec| SpecSetItem {
                index: dest as SpecSetParamRepr,
                asoc_ty,
                spec,
            })
            .collect_into(&mut self.storage);
        self.mapping.insert(source, dest);
    }

    pub fn project(&mut self, foreign_param: SpecSetParamRepr) -> Option<SpecSetParamRepr> {
        self.mapping
            .get(&(foreign_param as SpecSetParamRepr))
            .copied()
    }

    pub fn start_frame(&mut self) -> SpecSetFrame {
        SpecSetFrame(self.storage.len())
    }

    pub fn end_frame(&mut self, frame: SpecSetFrame) {
        self.storage.truncate(frame.0);
    }

    pub fn iter(&mut self) -> impl DoubleEndedIterator<Item = SpecSetGroup<'_>> {
        self.temp.clear();
        self.temp.extend(&self.storage);
        self.temp.sort_unstable_by(SpecSetItem::order);
        self.temp.dedup();

        self.temp
            .group_by(SpecSetItem::common_group)
            .map(|group| SpecSetGroup { group })
    }

    pub fn clear(&mut self) {
        self.storage.clear();
    }
}

pub struct SpecSetFrame(usize);

pub struct SpecSetGroup<'a> {
    group: &'a [SpecSetItem],
}

impl<'a> SpecSetGroup<'a> {
    pub fn index(&self) -> SpecSetParamRepr {
        self.group[0].index
    }

    pub fn asoc_ty(&self) -> OptFragRef<AsocTy> {
        self.group[0].asoc_ty
    }

    pub fn specs(&self) -> impl ExactSizeIterator<Item = Spec> + '_ + Clone {
        self.group.iter().map(|item| item.spec)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
struct SpecSetItem {
    index: SpecSetParamRepr,
    asoc_ty: OptFragRef<AsocTy>,
    spec: Spec,
}

impl SpecSetItem {
    fn common_group(&self, other: &Self) -> bool {
        self.index == other.index && self.asoc_ty == other.asoc_ty
    }

    fn order(&self, other: &Self) -> std::cmp::Ordering {
        // To ensure that the root params are ordered before the associated types and instances
        match (self.asoc_ty, other.asoc_ty) {
            (Some(_), None) => std::cmp::Ordering::Greater,
            (None, Some(_)) => std::cmp::Ordering::Less,
            _ => self
                .index
                .cmp(&other.index)
                .then(self.spec.cmp(&other.spec)),
        }
    }
}

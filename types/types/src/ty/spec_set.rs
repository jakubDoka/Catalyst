use std::iter;

use crate::*;
use storage::*;

pub type SpecSetParamRepr = TyParamIdx;

#[derive(Default)]
pub struct SpecSet<'a> {
    storage: Vec<SpecSetItem<'a>>,
    temp: Vec<SpecSetItem<'a>>,
    mapping: Map<SpecSetParamRepr, SpecSetParamRepr>,
}

impl<'a> SpecSet<'a> {
    pub fn extend(
        &mut self,
        dest: SpecSetParamRepr,
        source: Option<SpecSetParamRepr>,
        asoc_ty: OptFragRef<AsocTy>,
        specs: impl IntoIterator<Item = Spec<'a>>,
    ) {
        specs
            .into_iter()
            .map(|spec| SpecSetItem {
                index: dest as SpecSetParamRepr,
                asoc_ty,
                spec,
            })
            .collect_into(&mut self.storage);
        self.mapping.insert(source.unwrap_or(dest), dest);
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

    pub fn iter(&mut self) -> impl Iterator<Item = SpecSetGroup<'_>> {
        self.temp.clear();
        self.temp.extend(&self.storage);
        self.temp.sort_unstable_by(SpecSetItem::order);
        self.temp.dedup();

        // its complicated since we want to insert empty groups
        let mut params = TyParamIter::default();
        let mut progress = self.temp.as_slice();
        iter::from_fn(move || {
            let (initial, rest) = progress.split_first()?;
            let param = params.next()?;
            if param != initial.index && initial.asoc_ty.is_none() {
                return Some(SpecSetGroup {
                    index: param,
                    asoc_ty: None,
                    bounds: &[],
                });
            }
            let len = rest
                .iter()
                .take_while(|&item| item.common_group(initial))
                .count();
            let (group, rest) = progress.split_at(len + 1);
            progress = rest;

            Some(SpecSetGroup {
                index: initial.index,
                asoc_ty: initial.asoc_ty,
                bounds: group,
            })
        })
    }

    pub fn clear(&mut self) {
        self.storage.clear();
    }
}

pub struct SpecSetFrame(usize);

impl SpecSetFrame {
    pub fn base() -> Self {
        SpecSetFrame(0)
    }
}

pub struct SpecSetGroup<'a> {
    pub index: SpecSetParamRepr,
    pub asoc_ty: OptFragRef<AsocTy>,
    bounds: &'a [SpecSetItem<'a>],
}

impl<'a> SpecSetGroup<'a> {
    pub fn specs(&self) -> impl ExactSizeIterator<Item = Spec> + Clone + 'a {
        self.bounds.iter().map(|item| item.spec)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct SpecSetItem<'a> {
    pub index: SpecSetParamRepr,
    pub asoc_ty: OptFragRef<AsocTy>,
    pub spec: Spec<'a>,
}

impl SpecSetItem<'_> {
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

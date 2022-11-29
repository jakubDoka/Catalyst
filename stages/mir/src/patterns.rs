use std::{fmt::Display, hint::unreachable_unchecked, iter};

use storage::*;

pub fn find_gaps(tree: PatTree) -> Vec<Vec<Range>> {
    let mut missing = vec![];
    let mut current = UpperBound::Inside(0);
    for group in tree.nodes.group_by(|a, b| a.range.start == b.range.start) {
        let &[PatNode { range, .. }, ..] = group else { unsafe { unreachable_unchecked() } };
        for &PatNode { children, .. } in group {
            if let PatNodeChildren::More(sub_tree) = children {
                if sub_tree.has_missing {
                    for sub in find_gaps(sub_tree) {
                        missing.push(iter::once(range).chain(sub).collect());
                    }
                }
            }
        }
        if current != UpperBound::Inside(range.start) && let UpperBound::Inside(start) = current {
            missing.push(vec![Range { start, end: range.start.into() }]);
        }
        current = range.end;
    }

    if let UpperBound::Inside(current) = current {
        missing.push(vec![Range {
            start: current,
            end: UpperBound::Outside,
        }]);
    }

    missing.sort_unstable();
    missing.dedup();

    missing
}

pub fn as_tree<'a>(arena: &'a Arena, input: &[Branch<'a>], reachable: &mut [bool]) -> PatTree<'a> {
    let intersected = intersect_branches(input.iter().copied());

    let grouped = || intersected.group_by(|a, b| a.0.start == b.0.start);
    let mut branches = bumpvec![cap grouped().count()];
    let mut branch_buffer = bumpvec![];
    let mut final_missing = false;
    let mut current = UpperBound::Inside(0);
    for group in grouped() {
        // SAFETY: group has at least one element, otherwise group by would not supply it
        let &[(range, ..), ..] = group else { unsafe { unreachable_unchecked() } };
        let mut group_iter = group.iter().copied();
        let missing = loop {
            let Some((.., res)) = group_iter.next() else {
                break true;
            };

            let (children, missing) = match res {
                Ok(..) => {
                    branch_buffer.clear();
                    branch_buffer.extend(group.iter().filter_map(|(_, res)| res.ok()));
                    let sub_tree = as_tree(arena, &branch_buffer, reachable);
                    (PatNodeChildren::More(sub_tree), sub_tree.has_missing)
                }
                Err(branch) => {
                    reachable[branch] = true;
                    (PatNodeChildren::End(branch), false)
                }
            };
            branches.push(PatNode { range, children });
            if !missing {
                break false;
            }
        };

        final_missing |= (UpperBound::Inside(range.start) != current) | missing;
        current = range.end;
    }

    final_missing |= current != UpperBound::Outside;

    PatTree {
        has_missing: final_missing,
        nodes: arena.alloc_iter(branches),
    }
}

#[allow(clippy::type_complexity)]
fn intersect_branches<'a>(
    input: impl Iterator<Item = Branch<'a>> + Clone,
) -> BumpVec<(Range, Result<Branch<'a>, usize>)> {
    let sorted = sorted_ranges(input.clone());

    let mut intersected = bumpvec![];
    for Branch { start, nodes, ord } in input {
        let mut holder = [Range::empty()];
        let ranges = match start {
            Node::Scalar(range) => {
                holder[0] = range;
                &holder[..]
            }
            Node::Or(options) => options,
        };

        for overlap in ranges.iter().flat_map(|r| sorted.overlaps(r)) {
            let branch = match *nodes {
                [first, ref rest @ ..] => Ok(Branch {
                    start: first,
                    nodes: rest,
                    ord,
                }),
                [] => Err(ord),
            };
            intersected.push((overlap, branch));
        }
    }
    intersected.sort_unstable_by_key(|(r, b)| (r.start, b.map_or_else(|o| o, |b| b.ord)));
    intersected
}

fn sorted_ranges<'a>(input: impl Iterator<Item = Branch<'a>>) -> SortedRanges {
    let mut ranges = bumpvec![UpperBound::Inside(0), UpperBound::Outside];
    input
        .map(|branch| branch.start)
        .for_each(|node| match node {
            Node::Or(options) => {
                ranges.extend(options.iter().flat_map(|&option| option.to_array()))
            }
            Node::Scalar(option) => ranges.extend(option.to_array()),
        });
    SortedRanges::new(ranges)
}

#[derive(Debug, Clone, Copy)]
pub struct PatTree<'a> {
    pub has_missing: bool,
    pub nodes: &'a [PatNode<'a>],
}

#[derive(Debug, Clone, Copy)]
pub struct PatNode<'a> {
    pub range: Range,
    pub children: PatNodeChildren<'a>,
}

#[derive(Debug, Clone, Copy)]
pub enum PatNodeChildren<'a> {
    End(usize),
    More(PatTree<'a>),
}

#[derive(Copy, Clone, Debug)]
pub struct Branch<'a> {
    pub start: Node<'a>,
    pub nodes: &'a [Node<'a>],
    pub ord: usize,
}

#[derive(Copy, Clone, Debug)]
pub enum Node<'a> {
    Scalar(Range),
    Or(&'a [Range]),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Range {
    pub start: u128,
    pub end: UpperBound,
}

impl Range {
    pub fn new(start: u128, end: impl Into<UpperBound>) -> Self {
        Self {
            start,
            end: end.into(),
        }
    }

    pub const fn empty() -> Self {
        Self {
            start: 0,
            end: UpperBound::Inside(0),
        }
    }

    pub const fn full() -> Self {
        Self {
            start: 0,
            end: UpperBound::Outside,
        }
    }

    fn to_array(self) -> [UpperBound; 2] {
        [UpperBound::Inside(self.start), self.end]
    }

    pub fn at(int: u128) -> Range {
        Self {
            start: int,
            end: int
                .checked_add(1)
                .map_or(UpperBound::Outside, UpperBound::Inside),
        }
    }
}

impl Display for Range {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl<T: Into<UpperBound>> From<(u128, T)> for Range {
    fn from((start, end): (u128, T)) -> Self {
        Self::new(start, end)
    }
}

struct SortedRanges {
    ranges: BumpVec<UpperBound>,
}

impl SortedRanges {
    pub fn new(mut ranges: BumpVec<UpperBound>) -> Self {
        ranges.sort_unstable();
        ranges.dedup();
        Self { ranges }
    }

    pub fn overlaps(&self, other: &Range) -> impl Iterator<Item = Range> + '_ {
        let start = self.ranges.binary_search(&UpperBound::Inside(other.start));

        let end = self.ranges.binary_search(&other.end);

        let (Ok(start), Ok(end)) = (start, end) else {
            return None.into_iter().flatten();
        };

        if start >= end {
            return None.into_iter().flatten();
        }

        // SAFETY: due to previous checks, we know that the range is not empty and valid
        Some(
            unsafe { self.ranges.get_unchecked(start..end + 1) }
                .windows(2)
                .map(|window| {
                    // SAFETY: `window` is guaranteed to have at least 2 elements
                    // We also know for sure first element is in range since 2 outside
                    // elements cannot exist
                    let &[UpperBound::Inside(start), end] = window else {
                unsafe { unreachable_unchecked() }
            };
                    Range { start, end }
                }),
        )
        .into_iter()
        .flatten()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum UpperBound {
    Inside(u128),
    Outside,
}

impl Display for UpperBound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Inside(int) => write!(f, "{}", int),
            Self::Outside => write!(f, "max"),
        }
    }
}

impl From<u128> for UpperBound {
    fn from(value: u128) -> Self {
        Self::Inside(value)
    }
}

impl From<()> for UpperBound {
    fn from((): ()) -> Self {
        Self::Outside
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn build_branches<'a>(
        arena: &'a Arena,
        repr: &[&[&[impl Into<Range> + Copy]]],
    ) -> &'a [Branch<'a>] {
        fn alloc_node<'a>(arena: &'a Arena, range: &[impl Into<Range> + Copy]) -> Node<'a> {
            match range {
                &[range] => Node::Scalar(range.into()),
                _ => Node::Or(arena.alloc_iter(range.iter().map(|&r| r.into()))),
            }
        }

        arena.alloc_iter(repr.iter().enumerate().map(|(ord, nodes)| Branch {
            start: alloc_node(arena, nodes[0]),
            nodes: arena.alloc_iter(nodes.iter().skip(1).copied().map(|n| alloc_node(arena, n))),
            ord,
        }))
    }

    #[test]
    fn test_exhaustive() {
        use UpperBound::*;
        let arena = Arena::new();
        let branches = build_branches(
            &arena,
            &[
                &[&[(0, Outside)], &[(0, Inside(1))]],
                &[&[(0, Outside)], &[(0, Outside)]],
            ],
        );

        let res = as_tree(&arena, branches, &mut [false; 2]);
        println!("{:#?}", res)
    }
}

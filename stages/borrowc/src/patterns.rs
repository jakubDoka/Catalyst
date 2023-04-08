use std::{
    fmt::{self, Display},
    hint::unreachable_unchecked,
    iter,
};

use storage::*;
use types::*;

use crate::ExternalMirCtx;

pub(crate) fn find_gaps(tree: PatTree) -> Vec<Vec<Range>> {
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

pub(crate) fn as_tree<'a>(
    arena: &ProxyArena<'a>,
    input: &[Branch<'a>],
    reachable: &mut [bool],
) -> PatTree<'a> {
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
            NodeRanges::Scalar(range) => {
                holder[0] = range;
                &holder[..]
            }
            NodeRanges::Or(options) => options,
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
            NodeRanges::Or(options) => {
                ranges.extend(options.iter().flat_map(|&option| option.to_array()))
            }
            NodeRanges::Scalar(option) => ranges.extend(option.to_array()),
        });
    SortedRanges::new(ranges)
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct PatTree<'a> {
    pub(crate) has_missing: bool,
    pub(crate) nodes: &'a [PatNode<'a>],
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct PatNode<'a> {
    pub(crate) range: Range,
    pub(crate) children: PatNodeChildren<'a>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum PatNodeChildren<'a> {
    End(usize),
    More(PatTree<'a>),
}

#[derive(Copy, Clone, Debug)]
pub(crate) struct Branch<'a> {
    pub(crate) start: NodeRanges<'a>,
    pub(crate) nodes: &'a [NodeRanges<'a>],
    pub(crate) ord: usize,
}

#[derive(Copy, Clone, Debug)]
pub(crate) enum NodeRanges<'a> {
    Scalar(Range),
    #[allow(unused)]
    Or(&'a [Range]),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Range {
    pub(crate) start: u128,
    pub(crate) end: UpperBound,
}

impl Range {
    pub(crate) fn new(start: u128, end: impl Into<UpperBound>) -> Self {
        Self {
            start,
            end: end.into(),
        }
    }

    pub(crate) const fn empty() -> Self {
        Self {
            start: 0,
            end: UpperBound::Inside(0),
        }
    }

    pub(crate) const fn full() -> Self {
        Self {
            start: 0,
            end: UpperBound::Outside,
        }
    }

    fn to_array(self) -> [UpperBound; 2] {
        [UpperBound::Inside(self.start), self.end]
    }

    pub(crate) fn at(int: u128) -> Range {
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
    pub(crate) fn new(mut ranges: BumpVec<UpperBound>) -> Self {
        ranges.sort_unstable();
        ranges.dedup();
        Self { ranges }
    }

    pub(crate) fn overlaps(&self, other: &Range) -> impl Iterator<Item = Range> + '_ {
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

pub(crate) fn display_pat(pat: &[Range], ty: Ty, ext: &ExternalMirCtx) -> String {
    let mut res = String::new();
    let mut frontier = pat;
    display_pat_low(ty, &[], &mut res, &mut frontier, ext).unwrap();
    res
}

fn display_pat_low(
    ty: Ty,
    params: &[Ty],
    res: &mut String,
    frontier: &mut &[Range],
    ext: &ExternalMirCtx,
) -> fmt::Result {
    use fmt::Write;

    let mut advance = || {
        let (&first, others) = frontier.split_first().unwrap_or((&Range::full(), &[]));
        *frontier = others;
        first
    };

    use Builtin::*;
    match ty {
        Ty::Node(Node::Base(BaseTy::Struct(s))) => {
            let Struct { fields, .. } = ext.types[s];
            res.push_str("\\{ ");
            if let Some((&first, rest)) = ext.types[fields].split_first() {
                write!(res, "{}: ", first.name.get(ext.interner))?;
                display_pat_low(first.ty, params, res, frontier, ext)?;
                for &field in rest {
                    res.push_str(", ");
                    write!(res, "{}: ", field.name.get(ext.interner))?;
                    display_pat_low(field.ty, params, res, frontier, ext)?;
                }
            }
            res.push_str(" }");
        }
        Ty::Node(Node::Instance(inst)) => {
            let Instance { args, base } = ext.types[inst];
            let params = &ext.types[args];
            display_pat_low(base.as_ty(), params, res, frontier, ext)?;
        }
        Ty::Pointer(ptr) => {
            let base = ext.types[ptr.ty()];
            res.push('^');
            write!(res, "{}", ptr.mutability.to_mutability())?;
            display_pat_low(base, params, res, frontier, ext)?;
        }
        Ty::Param(index) => {
            let ty = params[index as usize];
            display_pat_low(ty, params, res, frontier, ext)?;
        }
        Ty::Builtin(b) => match b {
            Unit | Terminal | Uint | U32 | Mutable | Immutable | U16 | U8 | Short | Cint | Long
            | LongLong => {
                let first = advance();
                write!(res, "{first}")?;
            }
            Char => todo!(),
            Bool => {
                let first = advance();
                if first == Range::full() {
                    res.push('_');
                } else {
                    write!(res, "{}", first.start == 1)?;
                }
            }
            F32 | F64 => unreachable!(),
        },
        Ty::Node(Node::Base(BaseTy::Enum(r#enum))) => {
            let Enum { variants, .. } = ext.types[r#enum];
            let variant = if ext.types.enum_flag_ty(r#enum) != Uint {
                let flag = advance();
                let index = flag.start as usize;
                ext.types[variants][index]
            } else {
                ext.types[variants][0]
            };
            write!(res, "\\{}", variant.name.get(ext.interner))?;
            if variant.ty != Ty::UNIT {
                res.push('~');
                display_pat_low(variant.ty, params, res, frontier, ext)?;
            }
        }
        Ty::Array(_) => todo!(),
    }

    Ok(())
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum UpperBound {
    Inside(u128),
    Outside,
}

impl Display for UpperBound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Inside(int) => write!(f, "{int}"),
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
        arena: &ProxyArena<'a>,
        repr: &[&[&[impl Into<Range> + Copy]]],
    ) -> &'a [Branch<'a>] {
        fn alloc_node<'a>(
            arena: &ProxyArena<'a>,
            range: &[impl Into<Range> + Copy],
        ) -> NodeRanges<'a> {
            match range {
                &[range] => NodeRanges::Scalar(range.into()),
                _ => NodeRanges::Or(arena.alloc_iter(range.iter().map(|&r| r.into()))),
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
        proxy_arena!(let arena);
        let branches = build_branches(
            &arena,
            &[
                &[&[(0, Outside)], &[(0, Inside(1))]],
                &[&[(0, Outside)], &[(0, Outside)]],
            ],
        );

        let res = as_tree(&arena, branches, &mut [false; 2]);
        println!("{res:#?}")
    }
}

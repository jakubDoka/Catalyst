import bisect

def dbg(x):
    print(x)
    return x

def partition_ranges(ranges):
    return sorted(set(sum(map(list, ranges), [])))


assert partition_ranges(
    ((1, 10), (2, 4), (5, 14))
) == [1, 2, 4, 5, 10, 14]

def partition_pattern(pattern):
    if len(pattern) == 0 or len(pattern[0]) == 0: return []
    partition = partition_ranges(map(lambda x: x[0], pattern))
    result = []
    for ((s_start, r_end), *others) in pattern:
        start = bisect.bisect_left(partition, s_start)
        end = bisect.bisect_right(partition, r_end)
        for i in range(start + 1, end):
            span = (partition[i - 1], partition[i])
            result.append((span, tuple(others)))
    return result

assert partition_pattern(
    [
        ((0, 6), (0, 30)),
        ((2, 50), (0, 64)),
        ((50, 255), (0, 255)),
    ],
) == [
    ((0, 2), ((0, 30),)),
    ((2, 6), ((0, 30),)),
    ((2, 6), ((0, 64),)),
    ((6, 50), ((0, 64),)),
    ((50, 255), ((0, 255),)),
]

def build_pattern_tree(pattern):
    part_pattern = partition_pattern(pattern)
    branches = {}
    for (span, others) in part_pattern:
        if span not in branches:
            branches[span] = []
        branches[span].append(others)
    for span, others in branches.items():
        branches[span] = build_pattern_tree(others)
    return sorted(((k, v) for k, v in branches.items()))

assert build_pattern_tree(
    [
        ((0, 6), (0, 30)),
        ((2, 50), (0, 64)),
        ((50, 255), (0, 255)),
    ],
) == [
    ((0, 2), [((0, 30), [])]),
    ((2, 6), [((0, 30), []), ((30, 64), [])]),
    ((6, 50), [((0, 64), [])]),
    ((50, 255), [((0, 255), [])]),
]


def is_exhaustive_tree(type, pattern_tree):
    if len(type) == 0: return []
    arr = []
    (current, *others) = type
    ranges = partition_ranges(list(map(lambda x: x[0], pattern_tree)) + [current])
    for ((start, end), children) in pattern_tree:
        missing = is_exhaustive_tree(others, children)
        for m in missing: arr.append([(start, end)] + m)
        front = ranges.pop(0)
        if start != front:
            arr.append([(front, start)])
            assert start == ranges.pop(0)
    if len(ranges) != 1:
        arr.append([tuple(ranges)])
    return arr

def is_exhaustive(type, pattern):
    pattern_tree = build_pattern_tree(pattern)
    return is_exhaustive_tree(type, pattern_tree)


assert is_exhaustive(
    ((0, 255), (0, 255)),
    [
        ((0, 2), (0, 30)),
        ((2, 50), (0, 64)),
        ((0, 50), (30, 255)),
        ((50, 255), (0, 255)),
    ],
) == []
assert is_exhaustive(
    ((0, 255), (0, 255)),
    [
        ((0, 2), (0, 30)),
        ((5, 50), (0, 2)),
    ],
) == [
    [(0, 2), (30, 255)],
    [(5, 50), (2, 255)],
    [(2, 5)],
    [(50, 255)],
]
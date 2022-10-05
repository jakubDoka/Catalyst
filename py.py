
from gettext import find


CLONE = "clone"
ARC   = "arc"
U32   = "u32"

def dbg(val):
    print(val)
    return val

def compatible_types(a, b, mapping = None):
    if mapping is None: mapping = {}
    match (a, b):
        case ([*a_elements], [*b_elements]):
            return all(compatible_types(a, b, mapping) for a, b in zip(a_elements, b_elements))
        case _ if isinstance(b, int):
            if b in mapping:
                return a == mapping[b]
            else:
                mapping[b] = a
                return True
        case _: return a == b

assert compatible_types([U32], [U32])
assert compatible_types([U32], 0)
assert compatible_types([U32], [0])
assert not compatible_types([U32], [CLONE])
assert not compatible_types([U32], [ARC, [0]])
assert compatible_types([ARC, [U32]], [ARC, [1]])

def instantiate(type, mapping):
    match type:
        case [*types]:
            return [instantiate(t, mapping) for t in types]
        case _ if isinstance(type, int):
            return mapping[type]
        case _: return type

assert instantiate([U32], {}) == [U32]
assert instantiate([0], {0: U32}) == [U32]
assert instantiate([ARC, [0]], {0: U32}) == [ARC, [U32]]

def implements(impls, type, trait):
    mapping = {}
    matching = list(filter(lambda impl: compatible_types(type, impl[0], mapping) and compatible_types(trait, impl[1], mapping), impls))

    if len(matching) == 0: return

    matching_impl = list(filter(lambda cond: all(implements(impls, instantiate(type, mapping), trait) for (type, trait) in cond[2]), matching))

    if not matching_impl: return

    trait_instance = instantiate(matching_impl[0][1], mapping)

    if trait_instance != trait: return

    return True

def collides(impl1, impl2):
    mapping = {}
    return compatible_types(impl1[0], impl2[0], mapping) and compatible_types(impl1[1], impl2[1], mapping)

HASHMAP = "hashmap"
HASH = "hash"
EQ = "eq"
INSERT = "insert"

IMPLS = [
    ([U32], [CLONE], []),
    ([ARC, 0], [CLONE], [(0, [CLONE])]),
    ([U32], [HASH], []),
    ([U32], [EQ], []),
    ([ARC, 0], [HASH], [(0, [HASH])]),
    ([ARC, 0], [EQ], [(0, [EQ])]),
    ([HASHMAP, 0, 1], [CLONE], [(0, [CLONE]), (1, [CLONE])]),
    ([HASHMAP, 0, 1], [INSERT, 0, 1], [(0, [HASH]), (0, [EQ])]),
]

assert implements(IMPLS, [U32], [CLONE])
assert implements(IMPLS, [ARC, [U32]], [CLONE])
assert implements(IMPLS, [HASHMAP, [ARC, [U32]], [U32]], [CLONE])
assert implements(IMPLS, [HASHMAP, [ARC, [U32]], [U32]], [INSERT, [ARC, [U32]], [U32]])
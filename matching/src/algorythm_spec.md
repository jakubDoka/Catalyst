# Pattern exhaustion algorithm

## V1 - does not work
```
    input:
        (0..=20, 0..=20)
        (0..=50, 0..=50)
        (0..=255, 0..=255)

    1. build a graph
    root
        0..=20 1
            0..=20 1
        0..=50 2
            0..=50 2
        0..=255 3
            0..=255 3
    2. partition the graph into disjoint ranges
    root
        0..=20 1
            0..=20 1
        0..=20 2
            0..=50 2    
        21..=50 2
            0..=50 2
        0..=20 3
            0..=255 3
        21..=50 3
            0..=255 3
        51..=255 3
            0..=255 3
    3. Group duplicates and combine their children
    root    
        0..=20 1
            0..=20 1
            0..=50 2
            0..=255 3
        21..=50 2
            0..=50 2
            0..=255 3
        51..=255 3
            0..=255 3
    4. repeat recursively for each node that has children
    root
        0..=20 1 r
            0..=20 1 r
            21..=50 2 r
            51..=255 3 u
        21..=50 2 r
            0..=50 2 r
            51..=255 3 u
        51..=255 3 u
            0..=255 3 u
    5. check that all branches cover full range and
    6. mark unreachable and unconditional nodes, node 
    is unreachable by default
    7. take original patterns and match them against final 
    graph. The branch where pattern got fully exhausted is 
    the branch we care about. If the branch has different id,
    then the original branch is useless. Otherwise, inherit 
    whether segment is unconditional.

    test cases:
    0.
       (0..=20, 0..=20)
       (0..=255, 0..=255)
       (0..=20, 0..=255)
    1. 
        root
            0..=20 1
                0..=20 1
            0..=255 2
                0..=255 2
            0..=20 3
                0..=255 3
    3.
        root
            0..=20 1
                0..=20 1
            0..20 2
                0..=255 2
            21..=255 2
                0..=255 2
            0..=20 3
                0..=255 3
    4.
        root
            0..=20 1 r
                0..=20 1 r
                21..=255 2 u
            21..=50 2 u
                0..=255 2 u

        1 = (r, r)
        2 = (u, u)
        3 = (x, x)
```

# V2 - Failure
```
    input:
        (Some(a), Some(a))
        (None,    Some(a))
        (Some(a), None   )
        _
    
    1. deconstruct patterns
        1 - 0 1 | _ 2 | 0 1 | _ 2
        2 - 1 1       | 0 1 | _ 2
        3 - 0 1 | _ 2 | 1 1
        4 - _ 0
    
    2. group the same and split overlaps
        1 - 0 1 | _ 2 | 0 1 | _ 2
        3 - 0 1 | _ 2 | 1 1
        4 - _ 0

        2 - 1 1 | 0 1 | _ 2
        4 - _ 0 
    3. For each group, pop first element while its same in all patterns, expand the group
    into sub patterns, otherwise split
        1 - _ 2 | 0 1 | _ 2
        2 - _ 2 | 1 1 |
        4 - _ 0

        2 - 0 1 | _ 2
        4 - _ 0
    ---
        1 - 0 1 | _ 2
        4 - _ 0

        2 - 1 1
        4 - _ 0

        2 - _ 2
        4 - _ 0
    ---
        1 - _ 2
        4 - _ 0
    ---

    4. Go back and collect witnesses. If there are no 
    witnesses bound to some pattern, the pattern
    is unreachable (unr). If there are some witnesses not matching any pattern, 
    the pattern is not exhaustive and witnesses are reported. If pattern consumes 
    All remaining witnesses, then it is unconditional (unc). Otherwise pattern is 
    conditional (con).
        1 - _ 2 > _ unc
        4 - _ 0 > _ unr
    ---
        1 - 0 1 | _ 2 > 0 con  _ unc
        3 - 1 1       > 1 unc
        4 - _ 0       > _unr _ unr

        2 - _ 2 > _ unc
        4 - _ 0 > _ unr
    ---
        1 - _ 2 | 0 1 | _ 2 > _ unc 0 con _ unc
        3 - _ 2 | 1 1       > _ unr 1 unc
        4 - _ 0             > _ unr _ unr _ unr

        2 - 0 1 | _ 2 > 0 con _ unc
        4 - _ 0       > 1 unc _ unr
    ---
        1 - 0 1 | _ 2 | 0 1 | _ 2 > 0 con _ unc 0 con _ unc
        3 - 0 1 | _ 2 | _ 2       > _ unr _ unr 1 unc
        2 - 1 1       | 0 1 | _ 2 > 1 con       0 con _ unc
        4 - _ 0                   > 1 unc _ unr _ unr _ unr
        4 - _ 0                   > 0 unc       1 unc _ unr
    ---
        1 - 0 1 | _ 2 | 0 1 | _ 2 > 0 con _ unc 0 con _ unc
        3 - 0 1 | _ 2 | _ 2       > _ unr _ unr 1 unc
        2 - 1 1       | 0 1 | _ 2 > 1 con       0 con _ unc
        4 - _ 0                   > _ unc _ unr 1 unc _ unr

    output:
        1 - (Some(_), Some(_))
        2 - (None, Some(_))
        3 - (Some(_), None)
        4 - (None | Some(_), None)
```

# V3 - unmanageable
```
    input:
        (Some(a), Some(a))
        (None,    Some(a))
        (Some(a), None   )
        _
    2. build a graph
        root
            tuple:
                enum:
                    0 > 1
                    tuple:
                        _ > 1
                enum: 
                    0 > 1
                    tuple:
                        _ > 1
            tuple:
                enum: 
                    1 > 2
                enum:
                    0 > 2
                    tuple: 
                        _ > 2
            tuple:
                enum:
                    0 > 3
                    tuple:
                        _ > 3
                enum:
                    1 > 3
            _ > 4
    3. group duplicates and combine their children
        or:
            tuple:
                enum:
                    0 > 1
                    tuple:
                        _ > 1
                or:
                    enum: 
                        0 > 1
                        tuple:
                            _ > 1
                    enum:
                        1 > 3
            tuple:
                enum:
                    1 > 2
                enum:
                    0 > 2
                    tuple: 
                        _ > 2
            _ > 4
    4. handle wildcards
        
        
```

# V4
```
    input:
        (Some(a), Some(a))
        (None,    Some(a))
        (Some(a), None   )
        _
   
    1. represent as sequence of ranges with depth

        1. _ 0 0 1 _ 2 0 1 _ 2
        2. _ 0 1 1     0 1 _ 2
        3. _ 0 0 1 _ 2 1 1
        4. _ 0

    2. align the columns based of depth. While depth is smaller then the max depth,
    expand wor with wildcards. Do this for all columns.
        1. _ 0 0 1 _ 2 0 1 _ 2
        2. _ 0 1 1 _ 2 0 1 _ 2
        3. _ 0 0 1 _ 2 1 1 _ 2
        4. _ 0 _ 1 _ 2 _ 1 _ 2
    3. build a graph, split overlaps and group the nodes, do this recursively
        _ 0 1
            0 1 1
                _ 2 1
                    0 1 1
                        _ 2 1
        _ 0 2
            1 1 2
                _ 2 2
                    0 1 2
                        _ 2 2
        _ 0 3
            0 1 3
                _ 2 3
                    1 1 3
                        _ 2 3
        _ 0 4
            _ 1 4
                _ 2 4
                    _ 1 4
                        _ 2 4
    ---
        _ 0 1
            0 1 1
                _ 2 1
                    0 1 1
                        _ 2 1
            1 1 2
                _ 2 2
                    0 1 2
                        _ 2 2
            0 1 3
                _ 2 3
                    1 1 3
                        _ 2 3
            _ 1 4
                _ 2 4
                    _ 1 4
                        _ 2 4
    ---
        _ 0 1
            0 1 1
                _ 2 1
                    0 1 1
                        _ 2 1
                _ 2 3
                    1 1 3
                        _ 2 3
                _ 2 4
                    _ 1 4
                        _ 2 4
            1 1 2
                _ 2 2
                    0 1 2
                        _ 2 2
                _ 2 4
                    _ 1 4
                        _ 2 4
    ---
        _ 0 1
            0 1 1
                _ 2 1
                    0 1 1
                        _ 2 1
                    1 1 3
                        _ 2 3
                    _ 1 4
                        _ 2 4
            1 1 2
                _ 2 2
                    0 1 2
                        _ 2 2
                    _ 1 4
                        _ 2 4
    ---
        _ 0 1
            0 1 1
                _ 2 1
                    0 1 1
                        _ 2 1
                        _ 2 4
                    1 1 3
                        _ 2 3
                        _ 2 4
            1 1 2
                _ 2 2
                    0 1 2
                        _ 2 2
                        _ 2 4
                    1 1 4
                        _ 2 4
    ---
        _ 0 1
            0 1 1
                _ 2 1
                    0 1 1
                        _ 2 1
                    1 1 3
                        _ 2 3
            1 1 2
                _ 2 2
                    0 1 2
                        _ 2 2
                    1 1 4
                        _ 2 4
    4. On the way back, check if all cases are covered and build a error graph if not.
    5. Match expanded branches onto the graph. Every branch that is contained within the 
    branch range will be expanded and if noe with branch ide is found, the branch is 
    reachable. Last reachable branch in node is unconditional branch.
    6. We are done.
```
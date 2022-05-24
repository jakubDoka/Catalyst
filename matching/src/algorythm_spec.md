# Pattern exhaustion algorithm

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
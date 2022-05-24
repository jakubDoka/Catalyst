import copy

def bruteforce(matrix: list[list[float]], init_state: list[float], iters: int) -> list[float]:
    temp = [0.0] * len(init_state)
    for _ in range(iters):
        for i in range(len(matrix)):
            for k in range(len(init_state)):
                temp[i] +=  matrix[k][i] * init_state[k]
        init_state, temp = temp, init_state
        for i in range(len(temp)): temp[i] = 0.0
    return init_state

def main():
    matrix = [
        [0.8,   0.2],
        [0.65, 0.35],
    ]

    init_state = [1, 0]
    res = bruteforce(matrix, init_state, 100)
    print(res)

    matrix = [
        [0.55, 0.15, 0.3],
        [0.8, 0.2, 0],
        [1, 0, 0],
    ]

    init_state = [1, 0, 0]
    res = bruteforce(matrix, init_state, 2)
    print(res)

    res = bruteforce(matrix, init_state, 100)
    print(res)

    """
        [x1, x2, x3]

        x1 = 0.55 * x1 + 0.8 * x2 + 1 * x3
        x2 = 0.15 * x1 + 0.2 * x2 + 0 * x3
        x3 = 0.3 * x1

        0 = -0.45 * x1 +  0.8 * x2 + 1 *  x3
        0 = 0.15  * x1 + -0.8 * x2 + 0 *  x3
        0 = 0.3   * x1 +  0   * x2 +     -x3
        
        0.8 * x2 = 0.45 * x1 + -1 *  x3
        0.8 * x2 = 0.15 * x1
        0.3 * x1 = 1 * x3

        1 = x1 + 0.15/0.8 * x1 +0.3 * x1
        1 = x1 * (1 + 0.15/0.8 + 0.3)
        1/(1 + 0.15/0.8 + 0.3) = x1
    """

main()
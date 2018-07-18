import sys

NOMINATORS = (2, 3)


def find_previous_minimum(operations, k):
    m = k - 2
    previous_minimum = operations[m]
    min_arg = m
    for nominator in NOMINATORS:
        if k % nominator == 0:
            m = k // nominator - 1
            nr_ops = operations[m]
            if nr_ops < previous_minimum:
                previous_minimum = nr_ops
                min_arg = m
    return previous_minimum, min_arg


def backtrack_calculation(backtrack, n):
    path = [n]
    k = n - 1
    while k > 1:
        k = backtrack[k]
        path.append(k+1)
    return path[::-1]


def run_calculator(n):
    min_operations = [0] * n
    backtrack = [0] * n
    for k in range(2, n+1):
        previous_minimum, arg = find_previous_minimum(min_operations, k)
        min_operations[k-1] = previous_minimum + 1
        backtrack[k-1] = arg
    return backtrack_calculation(backtrack, n)


def main():
    data = sys.stdin.read().splitlines()
    n = int(data[0])
    result = run_calculator(n)
    print(len(result)-1)
    print(" ".join([str(elem) for elem in result]))


if __name__ == "__main__":
    main()

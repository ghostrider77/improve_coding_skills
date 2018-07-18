import sys


def convert_to_intlist(line):
    return [int(item) for item in line.split()]


def solve_knapsack_problem(weights, nr_weights, capacity):
    knapsack = [[-1] * nr_weights for _ in range(capacity)]
    def solve(current_capacity, n):
        if current_capacity == 0 or n == 0:
            return 0
        if knapsack[current_capacity-1][n-1] != -1:
           return knapsack[current_capacity-1][n-1]

        weight = weights[n-1]
        if current_capacity < weight:
            optimal_weight = solve(current_capacity, n - 1)
        else:
            optimal_weight = max(solve(current_capacity - weight, n - 1) + weight, solve(current_capacity, n - 1))
        knapsack[current_capacity-1][n-1] = optimal_weight
        return optimal_weight

    return solve(capacity, nr_weights)


def main():
    data = sys.stdin.read().splitlines()
    capacity, nr_weights = convert_to_intlist(data[0])
    weights = tuple(convert_to_intlist(data[1]))
    result = solve_knapsack_problem(weights, nr_weights, capacity)
    print(result)


if __name__ == "__main__":
    main()

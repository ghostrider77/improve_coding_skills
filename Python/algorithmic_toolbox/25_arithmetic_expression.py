import sys
import operator
from math import inf

OPS = {"+": operator.add,
       "-": operator.sub,
       "*": operator.mul}


def read_input_data(line):
    digits = tuple(int(elem) for elem in line[0::2])
    operations = tuple(line[1::2])
    return digits, operations


def initialize_matrix(row, col):
    return [[0] * col for _ in range(row)]


def calc_min_max(ix, jy, operations, minimum_of_subexpressions, maximum_of_subexpressions):
    subexpression_min = inf
    subexpression_max = -inf
    for k in range(ix, jy):
        op = OPS[operations[k]]
        a = op(maximum_of_subexpressions[ix][k], maximum_of_subexpressions[k+1][jy])
        b = op(maximum_of_subexpressions[ix][k], minimum_of_subexpressions[k+1][jy])
        c = op(minimum_of_subexpressions[ix][k], maximum_of_subexpressions[k+1][jy])
        d = op(minimum_of_subexpressions[ix][k], minimum_of_subexpressions[k+1][jy])
        subexpression_min = min(subexpression_min, a, b, c, d)
        subexpression_max = max(subexpression_max, a, b, c, d)
    return subexpression_min, subexpression_max


def maximize_an_arithmetic_expression(digits, operations):
    n = len(digits)
    minimum_of_subexpressions = initialize_matrix(n, n)
    maximum_of_subexpressions = initialize_matrix(n, n)
    for ix, digit in enumerate(digits):
        minimum_of_subexpressions[ix][ix] = digit
        maximum_of_subexpressions[ix][ix] = digit

    for s in range(1, n):
        for ix in range(n-s):
            jy = ix + s
            sub_min, sub_max = calc_min_max(ix, jy, operations, minimum_of_subexpressions, maximum_of_subexpressions)
            minimum_of_subexpressions[ix][jy] = sub_min
            maximum_of_subexpressions[ix][jy] = sub_max
    return maximum_of_subexpressions[0][n-1]


def main():
    data = sys.stdin.read().splitlines()
    digits, operations = read_input_data(data[0])
    result = maximize_an_arithmetic_expression(digits, operations)
    print(result)


if __name__ == "__main__":
    main()

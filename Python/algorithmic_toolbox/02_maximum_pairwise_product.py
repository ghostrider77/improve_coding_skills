import sys
from collections import namedtuple
from functools import reduce
from math import inf

LargestElems = namedtuple("LargestElems", ["largest", "second_largest"])

def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def calc_maximum_pairwise_product(lst):
    def process_next_elem(acc, elem):
        if elem > acc.largest:
            return LargestElems(largest=elem, second_largest=acc.largest)
        if elem > acc.second_largest:
            return LargestElems(largest=acc.largest, second_largest=elem)
        return acc

    result = reduce(process_next_elem, lst, LargestElems(-inf, -inf))
    return result.largest * result.second_largest


def main():
    lines = sys.stdin.read().splitlines()
    lst = convert_to_intlist(lines[1])
    result = calc_maximum_pairwise_product(lst)
    print(result)


if __name__ == "__main__":
    main()

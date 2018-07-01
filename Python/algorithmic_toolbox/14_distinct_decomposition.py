import sys


def get_decomposition_to_maximal_number_of_distinct_elems(n):
    summands = []
    smallest_summand = 1
    while n > 0:
        if n > 2 * smallest_summand:
            elem = smallest_summand
        else:
            elem = n
        summands.append(elem)
        n -= elem
        smallest_summand += 1
    return summands


def main():
    data = sys.stdin.read().splitlines()
    n = int(data[0])
    result = get_decomposition_to_maximal_number_of_distinct_elems(n)
    print(len(result))
    print(" ".join([str(elem) for elem in result]))


if __name__ == "__main__":
    main()

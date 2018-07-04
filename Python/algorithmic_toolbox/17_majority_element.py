import sys
from collections import Counter


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def has_majority_elem(lst, length):
    counter = Counter(lst)
    _, count = counter.most_common(n=1)[0]
    if count > length // 2:
        return 1
    return 0


def main():
    data = sys.stdin.read().splitlines()
    n = int(data[0])
    lst = convert_to_intlist(data[1])
    result = has_majority_elem(lst, n)
    print(result)


if __name__ == "__main__":
    main()

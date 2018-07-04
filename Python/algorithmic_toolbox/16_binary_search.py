import sys


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def get_list_and_size(line):
    lst = convert_to_intlist(line)
    return lst[1:], lst[0]


class SortedList(object):
    def __init__(self, sorted_list, length=None):
        self._lst = sorted_list
        self._length = len(self._lst) if length is None else length

    def _binary_search(self, elem, left, right):
        if left > right:
            return -1
        middle_ix = (left + right) // 2
        middle_elem = self._lst[middle_ix]
        if middle_elem == elem:
            return middle_ix
        if middle_elem < elem:
            return self._binary_search(elem, middle_ix + 1, right)
        return self._binary_search(elem, left, middle_ix - 1)

    def find_elem(self, elem):
        return self._binary_search(elem, 0, self._length - 1)


def find_elems_in_list(lst, length, queries):
    sorted_list = SortedList(lst, length)
    return [sorted_list.find_elem(elem) for elem in queries]


def main():
    data = sys.stdin.read().splitlines()
    lst, n = get_list_and_size(data[0])
    queries, _ = get_list_and_size(data[1])
    result = find_elems_in_list(lst, n, queries)
    print(" ".join([str(ix) for ix in result]))


if __name__ == "__main__":
    main()

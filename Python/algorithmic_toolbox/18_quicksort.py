import sys
from random import randint, seed


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


class InplaceSorting(object):
    def __init__(self, array, n):
        self._array = array
        self._length = n
        seed(2112)

    @property
    def array(self):
        return self._array

    def _three_way_partitioning(self, pivot, start, end):
        ix = start
        while ix <= end:
            elem = self._array[ix]
            if elem < pivot:
                if ix != start:
                    self._array[ix], self._array[start] = self._array[start], elem
                ix += 1
                start += 1
            elif elem > pivot:
                self._array[ix], self._array[end] = self._array[end], elem
                end -= 1
            else:
                ix += 1
        return start, end

    def quicksort(self):
        stack = [(0, self._length-1)]
        while stack:
            left_end, right_end = stack.pop(-1)
            if left_end < right_end:
                random_ix = randint(left_end, right_end)
                pivot = self._array[random_ix]
                middle_start, middle_end = self._three_way_partitioning(pivot, left_end, right_end)
                stack.append((left_end, middle_start-1))
                stack.append((middle_end+1, right_end))


def main():
    data = sys.stdin.read().splitlines()
    n = int(data[0])
    lst = convert_to_intlist(data[1])
    sorting = InplaceSorting(lst, n)
    sorting.quicksort()
    print(" ".join([str(elem) for elem in sorting.array]))


if __name__ == "__main__":
    main()

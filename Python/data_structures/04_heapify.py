import sys


def convert_to_intlist(line):
    return [int(item) for item in line.split()]


def get_index_of_parent_children_minimum(array, parent_index, size):
    min_index = parent_index
    left_child_ix = 2*parent_index + 1
    if left_child_ix < size and array[left_child_ix] < array[min_index]:
        min_index = left_child_ix
    right_child_ix = left_child_ix + 1
    if right_child_ix < size and array[right_child_ix] < array[min_index]:
        min_index = right_child_ix
    return min_index


def sift_down(array, parent_index, size, swaps):
    min_index = get_index_of_parent_children_minimum(array, parent_index, size)
    while min_index != parent_index:
        array[min_index], array[parent_index] = array[parent_index], array[min_index]
        swaps.append((parent_index, min_index))
        parent_index = min_index
        min_index = get_index_of_parent_children_minimum(array, parent_index, size)
    return


def heapify(array, size):
    swaps = []
    parent_index = (size // 2) - 1
    while parent_index >= 0:
        sift_down(array, parent_index, size, swaps)
        parent_index -= 1
    return swaps


def main():
    reader = sys.stdin
    n = int(next(reader))
    array = convert_to_intlist(next(reader))
    swaps = heapify(array, n)
    print(len(swaps))
    for swap in swaps:
        print(" ".join([str(item) for item in swap]))


if __name__ == "__main__":
    main()

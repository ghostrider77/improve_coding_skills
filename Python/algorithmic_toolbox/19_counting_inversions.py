import sys


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def merge_sorted_arrays(first, second, length_1, length_2, inversions):
    merged_list = []
    total_inversions = inversions
    ix = 0
    jy = 0
    while ix < length_1 or jy < length_2:
        if ix == length_1 and jy < length_2:
            merged_list.append(second[jy])
            jy += 1
        elif ix < length_1 and jy == length_2:
            merged_list.append(first[ix])
            ix += 1
        else:
            x = first[ix]
            y = second[jy]
            if x <= y:
                merged_list.append(x)
                ix += 1
            else:
                merged_list.append(y)
                jy += 1
                total_inversions += (length_1 - ix)
    return merged_list, total_inversions


def count_inversions(array, length):
    if length <= 1:
        return array, 0
    middle = length // 2
    first, second = array[:middle], array[middle:]
    length_1, length_2 = middle, length - middle
    sorted_first, inversions_first = count_inversions(first, length_1)
    sorted_second, inversions_second = count_inversions(second, length_2)
    return merge_sorted_arrays(sorted_first, sorted_second, length_1, length_2, inversions_first+inversions_second)


def main():
    data = sys.stdin.read().splitlines()
    n = int(data[0])
    lst = convert_to_intlist(data[1])
    _, result = count_inversions(lst, n)
    print(result)


if __name__ == "__main__":
    main()

import sys


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def read_itervals(lines, nr_segments):
    return zip(*[tuple(convert_to_intlist(next(lines))) for _ in range(nr_segments)])


def get_number_of_suitable_endpoints(endpoints, length, point):
    def binary_search(a, b):
        if a == b:
            return a
        mid = (a + b) // 2
        if endpoints[mid] <= point:
            return binary_search(mid+1, b)
        return binary_search(a, mid)

    if endpoints[-1] <= point:
        return length
    return binary_search(0, length-1)


def calc_intersection_size(sorted_left, sorted_negated_right, nr_segments, point):
    nr_good_left_ends = get_number_of_suitable_endpoints(sorted_left, nr_segments, point)
    nr_good_right_ends = get_number_of_suitable_endpoints(sorted_negated_right, nr_segments, -point)
    return nr_good_left_ends + nr_good_right_ends - nr_segments


def number_of_segments_containing_points(left_endpoints, right_endpoints, nr_segments, points):
    sorted_left = sorted(left_endpoints)
    sorted_negated_right = sorted(tuple([-endpoint for endpoint in right_endpoints]))
    return (calc_intersection_size(sorted_left, sorted_negated_right, nr_segments, p) for p in points)


def main():
    lines = sys.stdin
    nr_segments, _ = convert_to_intlist(next(lines))
    left_endpoints, right_endpoints = read_itervals(lines, nr_segments)
    points = convert_to_intlist(next(lines))
    result = number_of_segments_containing_points(left_endpoints, right_endpoints, nr_segments, points)
    print(" ".join([str(elem) for elem in result]))


if __name__ == "__main__":
    main()

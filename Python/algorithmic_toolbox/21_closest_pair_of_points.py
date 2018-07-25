import sys
from math import sqrt, inf
from collections import namedtuple

Point = namedtuple("Point", ["x", "y"])


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def read_points(lines, nr_points):
    return tuple(Point(*convert_to_intlist(next(lines))) for _ in range(nr_points))


def distance(p, q):
    return sqrt((p.x - q.x)**2  + (p.y - q.y)**2)


def find_points_in_stripe(first, second, median, delta):
    stripe = []
    def add_points_to_stripe(half_plane):
        for p in half_plane:
            if abs(p.x - median) <= delta:
                stripe.append(p)

    add_points_to_stripe(first)
    add_points_to_stripe(second)
    return stripe


def calc_minimum_distance_in_stripe(first, second, median, delta):
    stripe = sorted(find_points_in_stripe(first, second, median, delta), key=lambda p: p.y)
    return get_smallest_pairwise_distances(stripe, min_dist=delta, nr_points_to_compare_with=7)


def get_smallest_pairwise_distances(points, min_dist, nr_points_to_compare_with):
    for ix, p in enumerate(points):
        for q in points[ix+1:ix+nr_points_to_compare_with+1]:
            dist = distance(p, q)
            if dist < min_dist:
                min_dist = dist
    return min_dist


def find_closest_points(sorted_points, length):
    if length <= 3:
        return get_smallest_pairwise_distances(sorted_points, min_dist=inf, nr_points_to_compare_with=2)
    middle = length // 2
    median_x = sorted_points[middle][0]
    first, second = sorted_points[:middle], sorted_points[middle:]
    delta_1 = find_closest_points(first, middle)
    delta_2 = find_closest_points(second, length-middle)
    delta = min(delta_1, delta_2)
    if abs(delta) < 1e-14:
        return 0.0
    return calc_minimum_distance_in_stripe(first, second, median_x, delta)


def find_closest_pair_of_points(points, n):
    sorted_points = sorted(points, key=lambda p: p.x)
    return find_closest_points(sorted_points, n)


def main():
    lines = sys.stdin
    n = int(next(lines))
    points = read_points(lines, n)
    dist = find_closest_pair_of_points(points, n)
    print(dist)


if __name__ == "__main__":
    main()

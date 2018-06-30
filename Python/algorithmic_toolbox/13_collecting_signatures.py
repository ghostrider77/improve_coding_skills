import sys
from collections import namedtuple

Segment = namedtuple("Segment", ["left", "right"])


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def get_segments(lines):
    return [Segment(*convert_to_intlist(line)) for line in lines]


def calc_minimum_number_of_points_covering_segments(segments):
    sorted_segments = sorted(segments, key=lambda segment: segment.right)
    points = []
    while sorted_segments:
        _, b = sorted_segments[0]
        points.append(b)
        sorted_segments = [segment for segment in sorted_segments[1:] if segment.left > b]
    return points


def main():
    data = sys.stdin.read().splitlines()
    nr_segments = int(data[0])
    segments = get_segments(data[1:1+nr_segments])
    covering = calc_minimum_number_of_points_covering_segments(segments)
    print(len(covering))
    print(" ".join([str(elem) for elem in covering]))


if __name__ == "__main__":
    main()

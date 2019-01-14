import sys
from math import inf, hypot
from collections import namedtuple

Point = namedtuple("Point", ["x", "y"])


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def read_data_points(reader, nr_points):
    points = []
    for _ in range(nr_points):
        point = Point(*convert_to_intlist(next(reader)))
        points.append(point)
    return points


def calc_distance(p, q):
    return hypot(p.x - q.x, p.y - q.y)


def calc_minimal_spanning_tree(points):
    nodes_with_cost = {point: inf for point in points}
    start_node = points[0]
    nodes_with_cost[start_node] = 0
    total_cost = 0
    while nodes_with_cost:
        v = min(nodes_with_cost, key=nodes_with_cost.get)
        cost_of_adding_v = nodes_with_cost.pop(v)
        total_cost += cost_of_adding_v
        for z, cost_of_adding_z in nodes_with_cost.items():
            dist = calc_distance(v, z)
            if cost_of_adding_z > dist:
                nodes_with_cost[z] = dist
    return total_cost


def main():
    reader = sys.stdin
    nr_points = int(next(reader))
    points = read_data_points(reader, nr_points)
    result = calc_minimal_spanning_tree(points)
    print(result)


if __name__ == "__main__":
    main()

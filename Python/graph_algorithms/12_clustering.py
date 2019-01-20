import sys
from math import hypot
from collections import namedtuple

Point = namedtuple("Point", ["x", "y"])
DistanceBetweenTwoPoints = namedtuple("DistanceBetweenTwoPoints", ["p_ix", "q_ix", "dist"])


class UnionFind(object):
    def __init__(self, nr_points):
        self._nr_points = nr_points
        self._parent_indices = self._makeset()
        self._rank = [0] * nr_points

    def _makeset(self):
        return list(range(self._nr_points))

    def find(self, child_index):
        parent_index = self._parent_indices[child_index]
        node_indices_on_path = []
        while child_index != parent_index:
            node_indices_on_path.append(child_index)
            child_index = parent_index
            parent_index = self._parent_indices[child_index]
        root = parent_index
        for ix in node_indices_on_path:
            self._parent_indices[ix] = root
        return root

    def union(self, p_ix, q_ix, parent_p=None, parent_q=None):
        if parent_p is None:
            parent_p = self._find(p_ix)
        if parent_q is None:
            parent_q = self._find(q_ix)

        if parent_p == parent_q:
            return

        if self._rank[parent_p] > self._rank[parent_q]:
            self._parent_indices[parent_q] = parent_p
        else:
            self._parent_indices[parent_p] = parent_q
            if self._rank[parent_p] == self._rank[parent_q]:
                self._rank[parent_q] = self._rank[parent_q] + 1


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


def calc_sorted_pairwise_distances(points):
    distances = []
    for ix, p in enumerate(points):
        for jy, q in enumerate(points[ix+1:]):
            distances.append(DistanceBetweenTwoPoints(ix, ix+jy+1, calc_distance(p, q)))
    return sorted(distances, key=lambda x: x.dist)


def calc_optimal_clustering(nr_points, points, k):
    distances = calc_sorted_pairwise_distances(points)
    clusters = UnionFind(nr_points)
    nr_clusters = nr_points
    for p_ix, q_ix, dist in distances:
        cluster_of_p = clusters.find(p_ix)
        cluster_of_q = clusters.find(q_ix)
        if cluster_of_p != cluster_of_q:
            clusters.union(p_ix, q_ix, cluster_of_p, cluster_of_q)
            nr_clusters -= 1
            if nr_clusters == k - 1:
                return dist


def main():
    reader = sys.stdin
    nr_points = int(next(reader))
    points = read_data_points(reader, nr_points)
    k = int(next(reader))
    d = calc_optimal_clustering(nr_points, points, k)
    print(d)


if __name__ == "__main__":
    main()

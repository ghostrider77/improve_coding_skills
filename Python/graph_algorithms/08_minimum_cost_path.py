import sys
from math import inf, isinf
from collections import defaultdict


class DirectedGraph(object):
    def __init__(self, edge_list, nr_nodes):
        self._adjacency_list = self._convert_edgelist_to_adjacency_list(edge_list)
        self._nr_nodes = nr_nodes

    def _convert_edgelist_to_adjacency_list(self, edge_list):
        adjacency_list = defaultdict(list)
        for node_from, node_to, weight in edge_list:
            adjacency_list[node_from].append((node_to, weight))
        return dict(adjacency_list)

    def _update_distances(self, u, dist_u, distances, T):
        neighbours = self._adjacency_list.get(u, [])
        for v, weight in neighbours:
            distance_through_u = dist_u + weight
            if distances[v-1] > distance_through_u:
                distances[v-1] = distance_through_u
                T[v] = distance_through_u

    def dijkstra_algorithm(self, start_node):
        distances = [inf] * self._nr_nodes
        distances[start_node-1] = 0
        T = {node: distances[node-1] for node in range(1, self._nr_nodes+1)}
        while T:
            u = min(T, key=T.get)
            dist_u = T.pop(u)
            self._update_distances(u, dist_u, distances, T)
        return distances


def find_cheapest_path(edge_list, nr_nodes, s, t):
    graph = DirectedGraph(edge_list, nr_nodes)
    distances_from_s = graph.dijkstra_algorithm(s)
    result = distances_from_s[t-1]
    if isinf(result):
        return -1
    return result


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def read_weighted_edges(reader, nr_edges):
    edges = []
    for _ in range(nr_edges):
        edge = tuple(convert_to_intlist(next(reader)))
        edges.append(edge)
    return edges


def main():
    reader = sys.stdin
    nr_nodes, nr_edges = convert_to_intlist(next(reader))
    edge_list = read_weighted_edges(reader, nr_edges)
    s, t = convert_to_intlist(next(reader))
    result = find_cheapest_path(edge_list, nr_nodes, s, t)
    print(result)


if __name__ == "__main__":
    main()

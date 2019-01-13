#python3
import sys
from math import inf, isfinite
from collections import defaultdict, deque


class DirectedGraph(object):
    def __init__(self, edge_list, nr_nodes):
        self._adjacency_list = self._convert_edgelist_to_adjacency_list(edge_list)
        self._nr_nodes = nr_nodes

    @property
    def adjacency_list(self):
        return self._adjacency_list

    @property
    def nr_nodes(self):
        return self._nr_nodes

    def _convert_edgelist_to_adjacency_list(self, edge_list):
        adjacency_list = defaultdict(list)
        for node_from, node_to, weight in edge_list:
            adjacency_list[node_from].append((node_to, weight))
        return dict(adjacency_list)


def update_distances(graph, distances, collect_relaxed_nodes):
    relaxed_nodes = set()
    for node, neighbours in graph.adjacency_list.items():
        dist_node = distances[node-1]
        if isfinite(dist_node):
            for neighbour, weight in neighbours:
                distance_through_node = dist_node + weight
                if distances[neighbour-1] > distance_through_node:
                    distances[neighbour-1] = distance_through_node
                    relaxed_nodes.add(neighbour)
    if collect_relaxed_nodes:
        return relaxed_nodes


def bellmann_ford(graph, s):
    n = graph.nr_nodes
    distances = [inf] * n
    distances[s-1] = 0
    for ix in range(1, n):
        update_distances(graph, distances, collect_relaxed_nodes=False)
    relaxed_nodes = update_distances(graph, distances, collect_relaxed_nodes=True)
    return distances, relaxed_nodes


def find_nodes_reachable_from_relaxed_nodes(relaxed_nodes, graph):
    visited_nodes = relaxed_nodes
    queue = deque(list(relaxed_nodes))
    while queue:
        node = queue.popleft()
        neighbours = graph.adjacency_list.get(node, [])
        for neighbour, _ in neighbours:
            if neighbour not in visited_nodes:
                queue.append(neighbour)
                visited_nodes.add(neighbour)
    return visited_nodes


def calculate_shortest_paths(edge_list, nr_nodes, start_node):
    graph = DirectedGraph(edge_list, nr_nodes)
    distances, relaxed_nodes = bellmann_ford(graph, start_node)
    infinite_distance_nodes = find_nodes_reachable_from_relaxed_nodes(relaxed_nodes, graph)
    for node in infinite_distance_nodes:
        distances[node-1] = -inf
    return distances


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def read_weighted_edges(reader, nr_edges):
    edges = []
    for _ in range(nr_edges):
        edge = tuple(convert_to_intlist(next(reader)))
        edges.append(edge)
    return edges


def print_distances(distances):
    for dist in distances:
        if isfinite(dist):
            print(dist)
        elif dist > 0:
            print("*")
        else:
            print("-")


def main():
    reader = sys.stdin
    nr_nodes, nr_edges = convert_to_intlist(next(reader))
    edge_list = read_weighted_edges(reader, nr_edges)
    start_node = int(next(reader))
    distances = calculate_shortest_paths(edge_list, nr_nodes, start_node)
    print_distances(distances)


if __name__ == "__main__":
    main()

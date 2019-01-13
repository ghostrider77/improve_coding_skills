import sys
from collections import defaultdict


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


def update_distances(graph, distances):
    update_happened = False
    for node, neighbours in graph.adjacency_list.items():
        dist_node = distances[node-1]
        for neighbour, weight in neighbours:
            distance_through_node = dist_node + weight
            if distances[neighbour-1] > distance_through_node:
                distances[neighbour-1] = distance_through_node
                update_happened = True
    return update_happened


def has_negative_cycle(graph):
    n = graph.nr_nodes
    distances = [0] * n
    complete_pass_on_edges = 1
    while complete_pass_on_edges <= n:
        is_some_edge_updated = update_distances(graph, distances)
        if complete_pass_on_edges == n and is_some_edge_updated:
            return True
        complete_pass_on_edges += 1
    return False


def detect_negative_cycle(edge_list, nr_nodes):
    graph = DirectedGraph(edge_list, nr_nodes)
    return has_negative_cycle(graph)


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
    result = detect_negative_cycle(edge_list, nr_nodes)
    print(int(result))


if __name__ == "__main__":
    main()

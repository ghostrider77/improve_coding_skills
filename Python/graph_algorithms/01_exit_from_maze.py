import sys
from collections import defaultdict


class Graph(object):
    def __init__(self, edge_list):
        self.adjacency_list = self._convert_edgelist_to_adjacency_list(edge_list)

    def _convert_edgelist_to_adjacency_list(self, edge_list):
        adjacency_list = defaultdict(list)
        for node_from, node_to in edge_list:
            adjacency_list[node_from].append(node_to)
            adjacency_list[node_to].append(node_from)
        return dict(adjacency_list)


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def read_edges(reader, nr_edges):
    edges = []
    for _ in range(nr_edges):
        edge = tuple(convert_to_intlist(next(reader)))
        edges.append(edge)
    return edges


def get_unvisited_neighbours(current_nodes, graph, visited_nodes):
    unvisited_neighbours = set()
    for node in current_nodes:
        neighbours = graph.adjacency_list.get(node, [])
        for neighbour in neighbours:
            if neighbour not in visited_nodes:
                unvisited_neighbours.add(neighbour)
    return unvisited_neighbours


def are_nodes_connected(edge_list, start_node, end_node):
    graph = Graph(edge_list)
    current_nodes = {start_node}
    visited_nodes = {start_node}
    while current_nodes:
        unvisited_neighbours = get_unvisited_neighbours(current_nodes, graph, visited_nodes)
        if end_node in unvisited_neighbours:
            return True
        current_nodes = unvisited_neighbours
        visited_nodes.update(unvisited_neighbours)
    return False


def main():
    reader = sys.stdin
    _, nr_edges = convert_to_intlist(next(reader))
    edge_list = read_edges(reader, nr_edges)
    start, end = convert_to_intlist(next(reader))
    result = are_nodes_connected(edge_list, start, end)
    print(int(result))


if __name__ == "__main__":
    main()

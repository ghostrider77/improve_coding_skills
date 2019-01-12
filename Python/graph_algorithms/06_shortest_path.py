import sys
from collections import defaultdict, deque


class Graph(object):
    def __init__(self, edge_list, nr_nodes):
        self._adjacency_list = self._convert_edgelist_to_adjacency_list(edge_list)
        self._nr_nodes = nr_nodes

    def _convert_edgelist_to_adjacency_list(self, edge_list):
        adjacency_list = defaultdict(list)
        for node_from, node_to in edge_list:
            adjacency_list[node_from].append(node_to)
            adjacency_list[node_to].append(node_from)
        return dict(adjacency_list)

    def breadth_first_search(self, start_node):
        distances = [-1] * self._nr_nodes
        distances[start_node-1] = 0
        queue = deque([start_node])
        while queue:
            node = queue.popleft()
            neighbours = self._adjacency_list.get(node, [])
            for neighbour in neighbours:
                if distances[neighbour-1] == -1:
                    queue.append(neighbour)
                    distances[neighbour-1] = distances[node-1] + 1
        return distances


def find_shortest_path(edge_list, nr_nodes, s, t):
    graph = Graph(edge_list, nr_nodes)
    distances_from_s = graph.breadth_first_search(s)
    distance_s_t = distances_from_s[t-1]
    return distance_s_t


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def read_edges(reader, nr_edges):
    edges = []
    for _ in range(nr_edges):
        edge = tuple(convert_to_intlist(next(reader)))
        edges.append(edge)
    return edges


def main():
    reader = sys.stdin
    nr_nodes, nr_edges = convert_to_intlist(next(reader))
    edge_list = read_edges(reader, nr_edges)
    s, t = convert_to_intlist(next(reader))
    result = find_shortest_path(edge_list, nr_nodes, s, t)
    print(result)


if __name__ == "__main__":
    main()

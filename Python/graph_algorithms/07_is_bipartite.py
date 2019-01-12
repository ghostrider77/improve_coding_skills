import sys
from collections import defaultdict, deque


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def read_edges(reader, nr_edges):
    edges = []
    for _ in range(nr_edges):
        edge = tuple(convert_to_intlist(next(reader)))
        edges.append(edge)
    return edges


class Graph(object):
    def __init__(self, edge_list, nr_nodes):
        self._adjacency_list = self._convert_edgelist_to_adjacency_list(edge_list)
        self._nr_nodes = nr_nodes
        self._coloring = [-1] * self._nr_nodes

    def _convert_edgelist_to_adjacency_list(self, edge_list):
        adjacency_list = defaultdict(list)
        for node_from, node_to in edge_list:
            adjacency_list[node_from].append(node_to)
            adjacency_list[node_to].append(node_from)
        return dict(adjacency_list)

    def _is_visited(self, node):
        return self._coloring[node-1] != -1

    def _get_consistently_colored_component(self, start_node):
        self._coloring[start_node-1] = 0
        queue = deque([start_node])
        component = {start_node}
        while queue:
            node = queue.popleft()
            node_color = self._coloring[node-1]
            neighbours = self._adjacency_list.get(node, [])
            for neighbour in neighbours:
                if self._is_visited(neighbour):
                    if node_color == self._coloring[neighbour-1]:
                        return
                else:
                    queue.append(neighbour)
                    self._coloring[neighbour-1] = 1 - node_color
                    component.add(neighbour)
        return component

    def breadth_first_search(self):
        unvisited_nodes = set(range(1, self._nr_nodes+1))
        while unvisited_nodes:
            start_node = unvisited_nodes.pop()
            bipartite_component = self._get_consistently_colored_component(start_node)
            if bipartite_component is None:
                return 0
            unvisited_nodes.difference_update(bipartite_component)
        return 1


def is_graph_bipartite(edge_list, nr_nodes):
    graph = Graph(edge_list, nr_nodes)
    return graph.breadth_first_search()


def main():
    reader = sys.stdin
    nr_nodes, nr_edges = convert_to_intlist(next(reader))
    edge_list = read_edges(reader, nr_edges)
    result = is_graph_bipartite(edge_list, nr_nodes)
    print(result)


if __name__ == "__main__":
    main()

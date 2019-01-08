import sys
from collections import defaultdict
from itertools import count

class Graph(object):
    def __init__(self, edge_list, nr_nodes):
        self._adjacency_list = self._convert_edgelist_to_adjacency_list(edge_list)
        self._nr_nodes = nr_nodes
        self._previsit_numbers = [0] * nr_nodes
        self._postvisit_numbers = [0] * nr_nodes
        self._previsit_id = count(1)
        self._postvisit_id = count(1)

    def get_connected_components(self):
        components = []
        for starting_node in range(1, self._nr_nodes + 1):
            if not self._is_visited_in_dfs(starting_node):
                current_component = self._explore(starting_node)
                components.append(frozenset(current_component))
        return components

    def _convert_edgelist_to_adjacency_list(self, edge_list):
        adjacency_list = defaultdict(list)
        for node_from, node_to in edge_list:
            adjacency_list[node_from].append(node_to)
            adjacency_list[node_to].append(node_from)
        return dict(adjacency_list)

    def _is_visited_in_dfs(self, node):
        return self._previsit_numbers[node-1] > 0

    def _find_unvisited_neighbour_of_a_node(self, node):
        neighbours = self._adjacency_list.get(node, [])
        for neighbour in neighbours:
            if not self._is_visited_in_dfs(neighbour):
                return neighbour

    def _explore(self, starting_node):
        self._previsit_numbers[starting_node-1] = next(self._previsit_id)
        previsit_stack = [starting_node]
        current_component = {starting_node}
        while previsit_stack:
            last_node = previsit_stack.pop(-1)
            unvisited_neighbour = self._find_unvisited_neighbour_of_a_node(last_node)
            if unvisited_neighbour is None:
                self._postvisit_numbers[last_node-1] = next(self._postvisit_id)
            else:
                self._previsit_numbers[unvisited_neighbour-1] = next(self._previsit_id)
                previsit_stack.append(last_node)
                previsit_stack.append(unvisited_neighbour)
                current_component.add(unvisited_neighbour)
        return current_component


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
    graph = Graph(edge_list, nr_nodes)
    components = graph.get_connected_components()
    print(len(components))


if __name__ == "__main__":
    main()

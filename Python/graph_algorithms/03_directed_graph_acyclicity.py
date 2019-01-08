#python3
import sys
from collections import defaultdict
from itertools import count


class DirectedGraph(object):
    def __init__(self, edge_list, nr_nodes):
        self._adjacency_list = self._convert_edgelist_to_adjacency_list(edge_list)
        self._nr_nodes = nr_nodes
        self._previsit_numbers = [0] * nr_nodes
        self._postvisit_numbers = [0] * nr_nodes
        self._previsit_id = count(1)
        self._postvisit_id = count(1)
        self._components = []

    @property
    def adjacency_list(self):
        return self._adjacency_list

    @property
    def postvisit_numbers(self):
        return self._postvisit_numbers

    def depth_first_search(self):
        for node in range(1, self._nr_nodes + 1):
            if not self._is_visited_in_dfs(node):
                current_component = self._explore(node)
                self._components.append(frozenset(current_component))

    def _convert_edgelist_to_adjacency_list(self, edge_list):
        adjacency_list = defaultdict(list)
        for node_from, node_to in edge_list:
            adjacency_list[node_from].append(node_to)
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


def has_cycle(graph):
    graph.depth_first_search()
    postvisit_numbers = graph.postvisit_numbers
    for node, neighbours in graph.adjacency_list.items():
        node_number = postvisit_numbers[node-1]
        if any(postvisit_numbers[neighbour-1] >= node_number for neighbour in neighbours):
            return True
    return False


def main():
    reader = sys.stdin
    nr_nodes, nr_edges = convert_to_intlist(next(reader))
    edge_list = read_edges(reader, nr_edges)
    graph = DirectedGraph(edge_list, nr_nodes)
    result = has_cycle(graph)
    print(int(result))


if __name__ == "__main__":
    main()

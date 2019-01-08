import sys
from collections import defaultdict
from itertools import count


class DirectedGraph(object):
    def __init__(self, edge_list, nr_nodes, node_order=None):
        self._adjacency_list = self._convert_edgelist_to_adjacency_list(edge_list)
        self._nr_nodes = nr_nodes
        self._ordered_nodes = self._get_ordered_nodes(node_order)

    @property
    def adjacency_list(self):
        return self._adjacency_list

    @property
    def nr_nodes(self):
        return self._nr_nodes

    @property
    def ordered_nodes(self):
        return self._ordered_nodes

    def _convert_edgelist_to_adjacency_list(self, edge_list):
        adjacency_list = defaultdict(list)
        for node_from, node_to in edge_list:
            adjacency_list[node_from].append(node_to)
        return dict(adjacency_list)

    def _get_ordered_nodes(self, node_order):
        if node_order is None:
            return range(1, self._nr_nodes+1)
        return node_order


class DepthFirstSearch(object):
    def __init__(self, graph):
        self._graph = graph
        self._previsit_numbers = [0] * graph.nr_nodes
        self._postvisit_numbers = [0] * graph.nr_nodes
        self._previsit_id = count(start=1)
        self._postvisit_id = count(start=1)
        self._components = []
        self._topological_sorting = None

    @property
    def components(self):
        return self._components

    @property
    def postvisit_numbers(self):
        return self._postvisit_numbers

    def calc_topological_ordering(self):
        if self._topological_sorting is None:
            self.depth_first_search()
        return self._topological_sorting[::-1]

    def _is_visited_in_dfs(self, node):
        return self._previsit_numbers[node-1] > 0

    def _find_unvisited_neighbour_of_a_node(self, node):
        neighbours = self._graph.adjacency_list.get(node, [])
        for neighbour in neighbours:
            if not self._is_visited_in_dfs(neighbour):
                return neighbour

    def depth_first_search(self):
        if self._topological_sorting is None:
            self._topological_sorting = []
            for starting_node in self._graph.ordered_nodes:
                if not self._is_visited_in_dfs(starting_node):
                    current_component = self._explore(starting_node)
                    self._components.append(frozenset(current_component))

    def _explore(self, starting_node):
        self._previsit_numbers[starting_node-1] = next(self._previsit_id)
        previsit_stack = [starting_node]
        current_component = {starting_node}
        while previsit_stack:
            last_node = previsit_stack.pop(-1)
            unvisited_neighbour = self._find_unvisited_neighbour_of_a_node(last_node)
            if unvisited_neighbour is None:
                self._postvisit_numbers[last_node-1] = next(self._postvisit_id)
                self._topological_sorting.append(last_node)
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


def create_graph_with_edges_reversed(edge_list, nr_nodes, node_ordering):
    reversed_edges = ([(node_to, node_from) for node_from, node_to in edge_list])
    ordering, _ = zip(*sorted(zip(range(1, nr_nodes+1), node_ordering), key=lambda x: x[1], reverse=True))
    return DirectedGraph(reversed_edges, nr_nodes, ordering)


def calc_strongly_connected_components(edge_list, nr_nodes):
    graph = DirectedGraph(edge_list, nr_nodes)
    forward_dfs = DepthFirstSearch(graph)
    forward_dfs.depth_first_search()
    reversed_graph = create_graph_with_edges_reversed(edge_list, nr_nodes, forward_dfs.postvisit_numbers)
    backward_dfs = DepthFirstSearch(reversed_graph)
    backward_dfs.depth_first_search()
    return backward_dfs.components


def main():
    reader = sys.stdin
    nr_nodes, nr_edges = convert_to_intlist(next(reader))
    edge_list = read_edges(reader, nr_edges)
    scc = calc_strongly_connected_components(edge_list, nr_nodes)
    print(len(scc))


if __name__ == "__main__":
    main()

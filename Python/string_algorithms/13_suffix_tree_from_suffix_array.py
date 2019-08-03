import sys
from itertools import count
from collections import defaultdict, namedtuple

Edge = namedtuple("Edge", ["child_id", "edge_start", "edge_end"])


class Node(object):
    def __init__(self, parent_id, distance_from_root, edge_start, edge_end, children=None):
        self.parent_id = parent_id
        self.distance_from_root = distance_from_root
        self.edge_start = edge_start
        self.edge_end = edge_end
        self.children = {} if children is None else children


class SuffixTreeBuilder(object):
    def __init__(self, text):
        self._text = text
        self._length = len(text)
        self._node_id_generator = count(0)
        self._nodes = {}

    def build_suffix_tree_from_suffix_array(self, suffix_array, lcp_array):
        current_node_id = next(self._node_id_generator)
        current_node = Node(parent_id=None, distance_from_root=0, edge_start=-1, edge_end=-1)
        self._nodes[current_node_id] = current_node
        previous_lcp = 0
        for ix, suffix_index in enumerate(suffix_array):
            while current_node.distance_from_root > previous_lcp:
                current_node_id = current_node.parent_id
                current_node = self._nodes[current_node_id]

            if current_node.distance_from_root == previous_lcp:
                current_node_id, current_node = self._create_new_leaf(current_node, current_node_id, suffix_index)
            else:
                edge_start = suffix_array[ix-1] + current_node.distance_from_root
                offset = previous_lcp - current_node.distance_from_root
                middle_node_id, middle_node = self._break_edge(current_node, current_node_id, edge_start, offset)
                current_node_id, current_node = self._create_new_leaf(middle_node, middle_node_id, suffix_index)
            if ix < self._length - 1:
                previous_lcp = lcp_array[ix]
        return self._nodes

    def _break_edge(self, current_node, current_node_id, edge_start, offset):
        first_character = self._text[edge_start]
        mid_character = self._text[edge_start+offset]
        mid_node_id = next(self._node_id_generator)
        mid_node = Node(parent_id=current_node_id,
                        distance_from_root=current_node.distance_from_root+offset,
                        edge_start=edge_start,
                        edge_end=edge_start+offset)
        self._nodes[mid_node_id] = mid_node
        mid_node.children[mid_character] = current_node.children[first_character]
        child_id = current_node.children[first_character]
        child = self._nodes[child_id]
        child.parent_id = mid_node_id
        child.edge_start += offset
        current_node.children[first_character] = mid_node_id
        return mid_node_id, mid_node

    def _create_new_leaf(self, current_node, current_node_id, suffix_index):
        leaf_id = next(self._node_id_generator)
        leaf = Node(parent_id=current_node_id,
                    distance_from_root=self._length - suffix_index,
                    edge_start=suffix_index + current_node.distance_from_root,
                    edge_end=self._length)
        self._nodes[leaf_id] = leaf
        first_character_of_edge = self._text[leaf.edge_start]
        current_node.children[first_character_of_edge] = leaf_id
        return leaf_id, leaf


def create_edges(tree):
    edges = defaultdict(list)
    for node_id, node in tree.items():
        children = tuple(node.children[k] for k in sorted(node.children))
        for child_id in children:
            child = tree[child_id]
            edges[node_id].append(Edge(child_id=child_id, edge_start=child.edge_start, edge_end=child.edge_end))
    return dict(edges)


def output_edges(edges_dict, text):
    print(text)
    stack = [(0, 0)]
    while stack:
        (node, edge_index) = stack.pop(-1)
        if node in edges_dict:
            edges_from_node = edges_dict[node]
            if edge_index + 1 < len(edges_from_node):
                stack.append((node, edge_index + 1))
            edge = edges_from_node[edge_index]
            print("{} {}".format(edge.edge_start, edge.edge_end))
            stack.append((edge.child_id, 0))


def convert_to_intlist(line):
    return tuple(int(elem) for elem in line.split())


def main():
    reader = sys.stdin
    text = next(reader).rstrip()
    suffix_array = convert_to_intlist(next(reader))
    lcp_array = convert_to_intlist(next(reader))
    builder = SuffixTreeBuilder(text)
    suffix_tree = builder.build_suffix_tree_from_suffix_array(suffix_array, lcp_array)
    edges = create_edges(suffix_tree)
    output_edges(edges, text)


if __name__ == "__main__":
    main()

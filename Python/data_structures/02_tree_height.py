import sys


def convert_to_intlist(line):
    return [int(item) for item in line.split()]


class Node(object):
    def __init__(self, elem):
        self._key = elem
        self._children = set()

    @property
    def key(self):
        return self._key

    @property
    def children(self):
        return self._children

    def add_child(self, key):
        self._children.add(key)


class Tree(object):
    def __init__(self, number_of_nodes, parents_of_nodes):
        self._root = None
        self._nodes = self._build_tree(parents_of_nodes, number_of_nodes)

    def _build_tree(self, parents_of_nodes, number_of_nodes):
        nodes = [Node(k) for k in range(number_of_nodes)]
        for node_id, parent_id in enumerate(parents_of_nodes):
            if parent_id == -1:
                self._root = nodes[node_id]
            else:
                nodes[parent_id].add_child(node_id)
        return nodes

    def _get_children_of_nodes(self, keys):
        children = set()
        for key in keys:
            children.update(self._nodes[key].children)
        return children

    def calc_depth(self):
        new_keys = {self._root.key}
        depth = 0
        while new_keys:
            depth += 1
            new_keys = self._get_children_of_nodes(new_keys)
        return depth


def main():
    reader = sys.stdin
    number_of_nodes = int(next(reader))
    parent_ids = convert_to_intlist(next(reader))
    tree = Tree(number_of_nodes, parent_ids)
    depth = tree.calc_depth()
    print(depth)


if __name__ == "__main__":
    main()

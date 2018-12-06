import sys
from collections import namedtuple

Node = namedtuple("Node", ["key", "left", "right"])

class Stack(object):
    def __init__(self):
        self._stack = []

    @property
    def elements(self):
        return self._stack

    @property
    def is_empty(self):
        if self._stack:
            return False
        return True

    def top(self):
        return self._stack[-1]

    def push(self, elem):
        self._stack.append(elem)

    def pop(self):
        elem = self._stack.pop(-1)
        return elem


class BinaryTree(object):
    def __init__(self, node_list):
        self._tree = node_list
        self._root_index = 0

    def inorder_traversal(self):
        keys = []
        stack = Stack()
        node_index = self._root_index
        while True:
            if node_index != -1:
                node = self._tree[node_index]
                stack.push(node)
                node_index = node.left
            elif not stack.is_empty:
                node = stack.pop()
                keys.append(node.key)
                node_index = node.right
            else:
                return keys


def convert_to_intlist(line):
    return [int(item) for item in line.split()]


def read_node_information(reader, nr_nodes):
    return tuple(Node(*convert_to_intlist(next(reader))) for _ in range(nr_nodes))


def is_sorted(lst):
    return all(elem1 <= elem2 for elem1, elem2 in zip(lst, lst[1:]))


def is_valid_binary_search_tree(tree, number_of_nodes):
    if number_of_nodes <= 1:
        return True
    keys = tree.inorder_traversal()
    return is_sorted(keys)


def main():
    reader = sys.stdin
    number_of_nodes = int(next(reader))
    nodes = read_node_information(reader, number_of_nodes)
    tree = BinaryTree(nodes)
    result = is_valid_binary_search_tree(tree, number_of_nodes)
    if result:
        print("CORRECT")
    else:
        print("INCORRECT")


if __name__ == "__main__":
    main()

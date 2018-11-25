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

    def preorder_traversal(self):
        keys = []
        stack = Stack()
        node_index = self._root_index
        while True:
            if node_index != -1:
                node = self._tree[node_index]
                keys.append(node.key)
                node_index = node.left
                stack.push(node.right)
            elif not stack.is_empty:
                node_index = stack.pop()
            else:
                return keys

    def postorder_traversal(self):
        stack1 = Stack()
        stack2 = Stack()
        node_index = self._root_index
        stack1.push(node_index)
        while not stack1.is_empty:
            node_index = stack1.pop()
            if node_index != -1:
                stack2.push(node_index)
                node = self._tree[node_index]
                stack1.push(node.left)
                stack1.push(node.right)

        keys = []
        while not stack2.is_empty:
            node_index = stack2.pop()
            node = self._tree[node_index]
            keys.append(node.key)
        return keys


def convert_to_intlist(line):
    return [int(item) for item in line.split()]


def read_node_information(reader, nr_nodes):
    return tuple(Node(*convert_to_intlist(next(reader))) for _ in range(nr_nodes))


def traverse_tree(tree):
    inorder = tree.inorder_traversal()
    preorder = tree.preorder_traversal()
    postorder = tree.postorder_traversal()
    return inorder, preorder, postorder


def main():
    reader = sys.stdin
    nr_nodes = int(next(reader))
    nodes = read_node_information(reader, nr_nodes)
    tree = BinaryTree(nodes)
    traversals = traverse_tree(tree)
    for traversal in traversals:
        print(" ".join([str(key) for key in traversal]))


if __name__ == "__main__":
    main()

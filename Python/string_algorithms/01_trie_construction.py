import sys
from collections import defaultdict
from itertools import count


class Trie(object):
    def __init__(self, words=None):
        self._node_counter = count(0)
        self._root = next(self._node_counter)
        self._trie = defaultdict(list)
        if words is not None:
            self.add_words_to_trie(words)

    @property
    def trie(self):
        return dict(self._trie)

    def add_words_to_trie(self, words):
        for word in words:
            self._add_to_trie(word)

    def _add_to_trie(self, word):
        current_node = self._root
        for letter in word:
            node = self._get_neighbour_with_given_label(current_node, letter)
            if node is not None:
                current_node = node
            else:
                next_node = next(self._node_counter)
                self._trie[current_node].append((next_node, letter))
                current_node = next_node

    def _get_neighbour_with_given_label(self, current_node, letter):
        neighbours = self._trie[current_node]
        for node, label in neighbours:
            if label == letter:
                return node


def print_trie_adjacency_list(trie):
    for node, neighbours in trie.items():
        for neighbour, label in neighbours:
            print("{}->{}:{}".format(node, neighbour, label))


def main():
    data = sys.stdin.read().splitlines()
    patterns = data[1:]
    trie = Trie(patterns)
    print_trie_adjacency_list(trie.trie)


if __name__ == "__main__":
    main()

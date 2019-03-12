import sys
from collections import defaultdict
from itertools import count


class Trie(object):
    def __init__(self, words):
        self._node_counter = count(0)
        self._root = next(self._node_counter)
        self._trie = defaultdict(list)
        self._add_words_to_trie(words)

    def _add_words_to_trie(self, words):
        for word in words:
            self._add_to_trie(word)
        self._trie = dict(self._trie)

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

    def _has_leaf_child(self, node):
        children = self._trie.get(node, [])
        return any(label == "$" for _, label in children)

    def does_text_match_with_pattern(self, text, trie):
        current_node = self._root
        for letter in text:
            next_node = self._get_neighbour_with_given_label(current_node, letter)
            if next_node is None:
                return False
            if self._has_leaf_child(next_node):
                return True
            current_node = next_node
        return False


def multiple_pattern_matching(text, patterns):
    text_length = len(text)
    trie = Trie(patterns)
    indices = []
    for ix in range(text_length):
        if trie.does_text_match_with_pattern(text[ix:], trie):
            indices.append(ix)
    return indices


def main():
    data = sys.stdin.read().splitlines()
    text = data[0]
    n = int(data[1])
    patterns = (pattern + "$" for pattern in data[2:2+n])
    indices_of_matching_patterns = multiple_pattern_matching(text, patterns)
    print(" ".join([str(elem) for elem in indices_of_matching_patterns]))


if __name__ == "__main__":
    main()

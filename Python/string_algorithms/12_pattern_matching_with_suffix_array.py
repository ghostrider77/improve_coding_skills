#python3
import sys
from collections import defaultdict, Counter
from itertools import chain, count


class SuffixArrayBuilder(object):
    def __init__(self, text):
        self._text = text
        self._length = len(self._text)

    @property
    def text_length(self):
        return self._length

    def _sort_single_characters(self):
        occurrences = defaultdict(list)
        for ix, character in enumerate(self._text):
            occurrences[character].append(ix)
        return tuple(chain(*(occurrences[letter] for letter in sorted(occurrences.keys()))))

    def _compute_character_classes(self, order):
        label_generator = count(0)
        classes = [0] * self._length
        label = next(label_generator)
        classes[order[0]] = label
        for ix, character_index in enumerate(order[1:]):
            if self._text[character_index] != self._text[order[ix]]:
                label = next(label_generator)
            classes[character_index] = label
        return tuple(classes)

    def _count_cumulative_class_sizes(self, classes):
        counts = [0] * self._length
        class_sizes = Counter(classes)
        for class_id, class_size in class_sizes.items():
            counts[class_id] = class_size
        for ix in range(1, self._length):
            counts[ix] += counts[ix-1]
        return counts

    def _sort_doubled_shifts(self, cyclic_shift_size, order, classes):
        counts = self._count_cumulative_class_sizes(classes)
        new_order = [0] * self._length
        for ix in range(self._length-1, -1, -1):
            start = (order[ix] - cyclic_shift_size) % self._length
            class_id = classes[start]
            counts[class_id] -= 1
            new_order[counts[class_id]] = start
        return tuple(new_order)

    def _update_classes(self, order, classes, cyclic_shift_size):
        label_generator = count(0)
        new_classes = [0] * self._length
        label = next(label_generator)
        new_classes[order[0]] = label
        for current, previous in zip(order[1:], order):
            mid = (current + cyclic_shift_size) % self._length
            mid_previous = (previous + cyclic_shift_size) % self._length
            if classes[current] != classes[previous] or classes[mid] != classes[mid_previous]:
                label = next(label_generator)
            new_classes[current] = label
        return tuple(new_classes)

    def build_suffix_array(self):
        order = self._sort_single_characters()
        classes = self._compute_character_classes(order)
        cyclic_shift_size = 1
        while cyclic_shift_size < self._length:
            order = self._sort_doubled_shifts(cyclic_shift_size, order, classes)
            classes = self._update_classes(order, classes, cyclic_shift_size)
            cyclic_shift_size *= 2
        return order


def match_pattern_with_suffix_array(suffix_array, text, length, pattern):
    min_index = 0
    max_index = length
    while min_index < max_index:
        middle_index = (min_index + max_index) // 2
        if pattern > text[suffix_array[middle_index]:]:
            min_index = middle_index + 1
        else:
            max_index = middle_index
    first = min_index

    max_index = length
    while min_index < max_index:
        middle_index = (min_index + max_index) // 2
        suffix = text[suffix_array[middle_index]:]
        if suffix.startswith(pattern):
            min_index = middle_index + 1
        elif pattern < suffix:
            max_index = middle_index
        else:
            min_index = middle_index + 1
    last = max_index

    if first > last:
        return set()
    return {suffix_array[index] for index in range(first, last)}


def multiple_pattern_matching(text, patterns):
    builder = SuffixArrayBuilder(text)
    suffix_array = builder.build_suffix_array()
    length = builder.text_length
    matched_indices = set()
    for pattern in patterns:
        indices = match_pattern_with_suffix_array(suffix_array, text, length, pattern)
        matched_indices.update(indices)
    return matched_indices


def main():
    data = sys.stdin.read().splitlines()
    text = data[0] + "$"
    patterns = data[2].split(" ")
    result = multiple_pattern_matching(text, patterns)
    print(" ".join([str(ix) for ix in result]))


if __name__ == "__main__":
    main()

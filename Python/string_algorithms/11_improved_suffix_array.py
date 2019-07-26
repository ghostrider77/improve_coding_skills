import sys
from collections import defaultdict, Counter
from itertools import chain, count


def sort_single_characters(string):
    occurrences = defaultdict(list)
    for ix, character in enumerate(string):
        occurrences[character].append(ix)
    return tuple(chain(*(occurrences[letter] for letter in sorted(occurrences.keys()))))


def compute_character_classes(string, order, length):
    label_generator = count(0)
    classes = [0] * length
    label = next(label_generator)
    classes[order[0]] = label
    for ix, character_index in enumerate(order[1:]):
        if string[character_index] != string[order[ix]]:
            label = next(label_generator)
        classes[character_index] = label
    return tuple(classes)


def count_cumulative_class_sizes(classes, length):
    counts = [0] * length
    class_sizes = Counter(classes)
    for class_id, class_size in class_sizes.items():
        counts[class_id] = class_size
    for ix in range(1, length):
        counts[ix] += counts[ix-1]
    return counts


def sort_doubled_shifts(cyclic_shift_size, order, classes, length):
    counts = count_cumulative_class_sizes(classes, length)
    new_order = [0] * length
    for ix in range(length-1, -1, -1):
        start = (order[ix] - cyclic_shift_size) % length
        class_id = classes[start]
        counts[class_id] -= 1
        new_order[counts[class_id]] = start
    return tuple(new_order)


def update_classes(order, classes, cyclic_shift_size, length):
    label_generator = count(0)
    new_classes = [0] * length
    label = next(label_generator)
    new_classes[order[0]] = label
    for current, previous in zip(order[1:], order):
        mid = (current + cyclic_shift_size) % length
        mid_previous = (previous + cyclic_shift_size) % length
        if classes[current] != classes[previous] or classes[mid] != classes[mid_previous]:
            label = next(label_generator)
        new_classes[current] = label
    return tuple(new_classes)


def build_suffix_array(string):
    length = len(string)
    order = sort_single_characters(string)
    classes = compute_character_classes(string, order, length)
    cyclic_shift_size = 1
    while cyclic_shift_size < length:
        order = sort_doubled_shifts(cyclic_shift_size, order, classes, length)
        classes = update_classes(order, classes, cyclic_shift_size, length)
        cyclic_shift_size *= 2
    return order


def main():
    reader = sys.stdin
    text = next(reader).rstrip()
    result = build_suffix_array(text)
    print(" ".join([str(ix) for ix in result]))


if __name__ == "__main__":
    main()

import sys
from collections import Counter


def calc_first_occurrence_dict(transformed_string):
    letter_counts = Counter(transformed_string)
    first_occurrence = {}
    ix = 0
    for letter in sorted(letter_counts.keys()):
        first_occurrence[letter] = ix
        ix += letter_counts[letter]
    return first_occurrence


def calc_count_matrix(transformed_string, unique_characters, length):
    count_matrix = {letter: [0] * (length+1) for letter in unique_characters}
    for ix, current_letter in enumerate(transformed_string):
        for char in unique_characters:
            counts = count_matrix[char]
            counts[ix+1] = counts[ix] + 1 if current_letter == char else counts[ix]
    return count_matrix


def letter_occurred_in_last_column_between_pointers(letter, last_column, top_pointer, bottom_pointer):
    return any(letter == char for char in last_column[top_pointer:bottom_pointer+1])


def pattern_matching(pattern, last_column, length, first_occurrences, count_matrix):
    top_pointer = 0
    bottom_pointer = length - 1
    while pattern:
        letter = pattern.pop(-1)
        if not letter_occurred_in_last_column_between_pointers(letter, last_column, top_pointer, bottom_pointer):
            return 0
        top_pointer = first_occurrences[letter] + count_matrix[letter][top_pointer]
        bottom_pointer = first_occurrences[letter] + count_matrix[letter][bottom_pointer+1] - 1
    return bottom_pointer - top_pointer + 1


def improved_BW_pattern_matching(transformed_string, patterns):
    string_length = len(transformed_string)
    first_occurrences = calc_first_occurrence_dict(transformed_string)
    unique_letters = frozenset(first_occurrences.keys())
    count_matrix = calc_count_matrix(transformed_string, unique_letters, string_length)

    pattern_occurrences = []
    for pattern in patterns:
        number_of_matches = pattern_matching(list(pattern), transformed_string, string_length,
                                             first_occurrences, count_matrix)
        pattern_occurrences.append(number_of_matches)
    return pattern_occurrences


def main():
    data = sys.stdin.read().splitlines()
    transformed_string = data[0]
    patterns = data[2].split(" ")
    pattern_matches = improved_BW_pattern_matching(transformed_string, patterns)
    print(" ".join([str(count) for count in pattern_matches]))


if __name__ == "__main__":
    main()

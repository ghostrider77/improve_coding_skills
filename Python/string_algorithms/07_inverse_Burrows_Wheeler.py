import sys
from collections import defaultdict


def create_indexed_column(string):
    counts = defaultdict(int)
    string_and_index = []
    for letter in string:
        count = counts[letter]
        string_and_index.append((letter, count))
        counts[letter] += 1
    return string_and_index


def inverse_Burrows_Wheeler_transform(transformed_string):
    last_column = create_indexed_column(transformed_string)
    first_column = {numbered_letter: ix for ix, numbered_letter
                    in enumerate(create_indexed_column(sorted(transformed_string)))}

    first_column_position = 0
    string = []
    for _ in transformed_string:
        letter_count = last_column[first_column_position]
        string.append(letter_count[0])
        first_column_position = first_column[letter_count]
    return "".join(string[-2::-1] + ["$"])


def main():
    reader = sys.stdin
    transformed = next(reader).rstrip()
    text = inverse_Burrows_Wheeler_transform(transformed)
    print(text)


if __name__ == "__main__":
    main()

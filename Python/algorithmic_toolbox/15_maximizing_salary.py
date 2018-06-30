import sys


def find_largest_number_from_pieces(number_strings):
    class Number(object):
        def __init__(self, as_string):
            self.as_string = as_string

        def __lt__(n1, n2):
            number_1_as_string, number_2_as_string = n1.as_string, n2.as_string
            return number_1_as_string + number_2_as_string < number_2_as_string + number_1_as_string

    return "".join(sorted(number_strings, key=Number, reverse=True))


def main():
    data = sys.stdin.read().splitlines()
    number_strings = data[1].split()
    result = find_largest_number_from_pieces(number_strings)
    print(result)


if __name__ == "__main__":
    main()

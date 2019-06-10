import sys


def calc_suffix_array(string):
    class StartingIndex:
        def __init__(self, value):
            self.value = value

        def __lt__(I, J):
            i, j = I.value, J.value
            return string[i:] < string[j:]

    return sorted(range(len(string)), key=StartingIndex)


def main():
    reader = sys.stdin
    text = next(reader).rstrip()
    suffix_array = calc_suffix_array(text)
    print(" ".join([str(ix) for ix in suffix_array]))


if __name__ == "__main__":
    main()

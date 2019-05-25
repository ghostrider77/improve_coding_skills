import sys


def calc_BW_transform(string):
    length = len(string)
    double_string = string * 2

    class StartingIndex:
        def __init__(self, value):
            self.value = value

        def __lt__(I, J):
            i, j = I.value, J.value
            for k in range(length):
                if double_string[i+k] < double_string[j+k]:
                    return True
                if double_string[i+k] > double_string[j+k]:
                    return False
            return False

    index_order = sorted(range(length), key=StartingIndex)
    return "".join(double_string[ix+length-1] for ix in index_order)


def main():
    reader = sys.stdin
    text = next(reader).rstrip()
    transformed = calc_BW_transform(text)
    print(transformed)


if __name__ == "__main__":
    main()

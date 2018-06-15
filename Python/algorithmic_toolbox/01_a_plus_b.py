import sys


def add_two_numbers(a, b):
    return a + b


def main():
    lines = sys.stdin
    a = int(next(lines))
    b = int(next(lines))
    result = add_two_numbers(a, b)
    print(result)


if __name__ == "__main__":
    main()

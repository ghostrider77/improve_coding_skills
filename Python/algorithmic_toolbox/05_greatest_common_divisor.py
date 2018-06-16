import sys


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def calc_gcd(a, b):
    while b > 0:
        a, b = b, a % b
    return a


def main():
    data = sys.stdin.read().splitlines()
    a, b = convert_to_intlist(data[0])
    result = calc_gcd(a, b)
    print(result)


if __name__ == "__main__":
    main()

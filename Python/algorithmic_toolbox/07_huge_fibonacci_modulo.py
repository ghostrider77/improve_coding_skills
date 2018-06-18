import sys


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def calc_pisano_period(modulus):
    p = 1
    a, b = 1, 1
    while not (a == 0 and b == 1):
        a, b = b, ((a + b) % modulus)
        p += 1
    return p


def calc_fibonacci_modulo(n, modulus):
    k = 1
    a, b = 0, 1
    while k <= n:
        a, b = b, (a + b) % modulus
        k += 1
    return a


def calc_huge_fibonacci_modulo(n, modulus):
    p = calc_pisano_period(modulus)
    return calc_fibonacci_modulo(n % p, modulus)


def main():
    data = sys.stdin.read().splitlines()
    n, modulus = convert_to_intlist(data[0])
    f = calc_huge_fibonacci_modulo(n, modulus)
    print(f)


if __name__ == "__main__":
    main()

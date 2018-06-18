import sys

MODULUS = 10

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


def calc_last_digit_of_the_sum_of_fibonacci_numbers(n):
    p = calc_pisano_period(MODULUS)
    return (calc_fibonacci_modulo((n + 2) % p, MODULUS) - 1) % MODULUS


def main():
    data = sys.stdin.read().splitlines()
    n = int(data[0])
    digit = calc_last_digit_of_the_sum_of_fibonacci_numbers(n)
    print(digit)


if __name__ == "__main__":
    main()

import sys

MODULUS = 10

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


def calc_last_digit_of_partial_sum(m, n):
    p = calc_pisano_period(MODULUS)
    last_digit_m_minus_one = (calc_fibonacci_modulo((m + 1) % p, MODULUS) - 1) % MODULUS
    last_digit_n = (calc_fibonacci_modulo((n + 2) % p, MODULUS) - 1) % MODULUS
    return (last_digit_n - last_digit_m_minus_one) % MODULUS


def main():
    data = sys.stdin.read().splitlines()
    m, n = convert_to_intlist(data[0])
    digit = calc_last_digit_of_partial_sum(m, n)
    print(digit)


if __name__ == "__main__":
    main()

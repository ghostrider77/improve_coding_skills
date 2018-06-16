import sys

MODULUS = 10

def fibonacci_last_digit(n):
    k = 1
    a, b = 0, 1
    while k <= n:
        a, b = b, (a + b) % MODULUS
        k += 1
    return a


def main():
    data = sys.stdin.read().splitlines()
    n = int(data[0])
    digit = fibonacci_last_digit(n)
    print(digit)


if __name__ == "__main__":
    main()

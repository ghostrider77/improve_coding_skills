import sys


def fibonacci(n):
    k = 1
    a, b = 0, 1
    while k <= n:
        a, b = b, a + b
        k += 1
    return a


def main():
    data = sys.stdin.read().splitlines()
    n = int(data[0])
    f = fibonacci(n)
    print(f)


if __name__ == "__main__":
    main()

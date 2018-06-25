import sys

COINS = (10, 5, 1)


def calc_minimum_number_of_changes(amount):
    number_of_changes = 0
    for coin in COINS:
        number_of_changes += (amount // coin)
        amount = amount % coin
    return number_of_changes


def main():
    data = sys.stdin.read().splitlines()
    amount = int(data[0])
    result = calc_minimum_number_of_changes(amount)
    print(result)


if __name__ == "__main__":
    main()

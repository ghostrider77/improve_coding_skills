import sys
from collections import namedtuple

Item = namedtuple("Item", ["value", "weight"])


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def read_items(lines, nr_items):
    items = []
    ix = 0
    while ix < nr_items:
        items.append(Item(*convert_to_intlist(lines[ix])))
        ix += 1
    return items


def fractional_knapsack(items, capacity):
    sorted_items = sorted(items, key=lambda x: x.value / x.weight, reverse=True)
    total_value = 0
    for value, weight in sorted_items:
        if capacity == 0:
            return total_value
        amount = min(weight, capacity)
        total_value += amount * (value / weight)
        capacity -= amount
    return total_value


def main():
    data = sys.stdin.read().splitlines()
    nr_items, capacity = convert_to_intlist(data[0])
    items = read_items(data[1:], nr_items)
    value = fractional_knapsack(items, capacity)
    print(value)

if __name__ == "__main__":
    main()

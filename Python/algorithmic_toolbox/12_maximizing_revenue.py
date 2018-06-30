import sys


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def calc_maximal_revenue(profit_per_click, average_click):
    revenue = 0
    for p, a in zip(sorted(profit_per_click), sorted(average_click)):
        revenue += p * a
    return revenue


def main():
    data = sys.stdin.read().splitlines()
    profit_per_click = convert_to_intlist(data[1])
    average_click = convert_to_intlist(data[2])
    result = calc_maximal_revenue(profit_per_click, average_click)
    print(result)


if __name__ == "__main__":
    main()

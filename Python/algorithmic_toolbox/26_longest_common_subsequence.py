import sys
from collections import namedtuple

Sequence = namedtuple("Sequence", ["seq", "length"])


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def read_input_data(data):
    sequences = []
    k = 0
    while k < 3:
        ix = 2 * k
        length = int(data[ix])
        sequence = tuple(convert_to_intlist(data[ix+1]))
        sequences.append(Sequence(seq=sequence, length=length))
        k += 1
    return sequences


def calc_longest_common_subsequence(data):
    sequences, lengths = zip(*data)
    s1, s2, s3 = sequences
    n1, n2, n3 = lengths

    longest_path = [[[0] * (n3+1) for _ in range(n2+1)] for _ in range(n1+1)]
    for i in range(n1):
        for j in range(n2):
            for k in range(n3):
                if s1[i] == s2[j] and s1[i] == s3[k]:
                    longest_path[i+1][j+1][k+1] = longest_path[i][j][k] + 1
                else:
                    longest_path[i+1][j+1][k+1] = max(longest_path[i][j+1][k+1],
                                                      longest_path[i+1][j][k+1],
                                                      longest_path[i+1][j+1][k])
    return longest_path[-1][-1][-1]


def main():
    data = sys.stdin.read().splitlines()
    sequences = read_input_data(data)
    result = calc_longest_common_subsequence(sequences)
    print(result)


if __name__ == "__main__":
    main()

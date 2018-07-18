import sys


def calc_Levenshtein_distance(string_1, string_2):
    n = len(string_1)
    m = len(string_2)
    edit_distance = [[0] * (m+1) for _ in range(n+1)]
    for ix in range(1, n+1):
        edit_distance[ix][0] = ix
    for jy in range(1, m+1):
        edit_distance[0][jy] = jy

    for ix in range(n):
        for jy in range(m):
            deletion = edit_distance[ix][jy+1] + 1
            insertion = edit_distance[ix+1][jy] + 1
            match = edit_distance[ix][jy]
            if string_1[ix] != string_2[jy]:
                match += 1

            edit_distance[ix+1][jy+1] = min(insertion, deletion, match)
    return edit_distance[-1][-1]


def main():
    data = sys.stdin.read().splitlines()
    s1 = data[0]
    s2 = data[1]
    dist = calc_Levenshtein_distance(s1, s2)
    print(dist)


if __name__ == "__main__":
    main()

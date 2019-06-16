import sys


def calc_prefix_function(text):
    length = len(text)
    prefix_array = [0] * length
    border = 0
    for ix, letter in enumerate(text[1:]):
        while border > 0 and letter != text[border]:
            border = prefix_array[border-1]

        if letter == text[border]:
            border += 1
        else:
            border = 0
        prefix_array[ix+1] = border
    return prefix_array


def find_pattern_in_genome(genome, pattern):
    pattern_length = len(pattern)
    prefix_function = calc_prefix_function(pattern + "$" + genome)
    return [ix - 2*pattern_length for ix, index in enumerate(prefix_function)
            if ix > pattern_length and index == pattern_length]


def main():
    reader = sys.stdin
    pattern = next(reader).rstrip()
    genome = next(reader).rstrip()
    indices = find_pattern_in_genome(genome, pattern)
    print(" ".join([str(ix) for ix in indices]))


if __name__ == "__main__":
    main()

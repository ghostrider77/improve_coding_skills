import sys
from random import randint, seed

PRIME = 1000000007


def polynomial_hashing(string, prime, x):
    value = 0
    for letter in reversed(string):
        value = (value * x + ord(letter)) % prime
    return value


def calc_power_of_x(x, prime, exponent):
    power = 1
    for _ in range(exponent):
        power = (power * x) % prime
    return power


def precompute_hashes_for_substrings_of_text(text, text_length, pattern_length, p, x):
    hash_values = [0] * (text_length - pattern_length + 1)
    last_substring_in_text = text[-pattern_length:]
    hash_values[-1] = polynomial_hashing(last_substring_in_text, p, x)
    x_power = calc_power_of_x(x, p, pattern_length)
    for ix in range(text_length-pattern_length-1, -1, -1):
        hash_values[ix] = (x*hash_values[ix+1] + ord(text[ix]) - x_power*ord(text[ix+pattern_length])) % p
    return hash_values


def rabin_karp_algorithm(text, pattern, prime):
    x = randint(1, prime-1)
    pattern_length = len(pattern)
    text_length = len(text)
    matching_indices = []
    pattern_hash = polynomial_hashing(pattern, prime, x)
    substring_hashes = precompute_hashes_for_substrings_of_text(text, text_length, pattern_length, prime, x)
    for ix in range(text_length-pattern_length+1):
        if pattern_hash == substring_hashes[ix] and text[ix:ix+pattern_length] == pattern:
            matching_indices.append(ix)
    return matching_indices


def main():
    data = sys.stdin.read().splitlines()
    pattern = data[0]
    text = data[1]
    seed(2112)
    result = rabin_karp_algorithm(text, pattern, PRIME)
    print(" ".join([str(ix) for ix in result]))


if __name__ == "__main__":
    main()

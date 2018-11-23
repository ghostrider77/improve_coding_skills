import sys
from collections import namedtuple

Params = namedtuple("Params", ["prime", "x"])

PARAMETERS = Params(prime=1000000007, x=263)

class HashTable(object):
    def __init__(self, prime, x, cardinality):
        self._prime = prime
        self._x = x
        self._cardinality = cardinality
        self._table = [[] for _  in range(self._cardinality)]

    def _polynomial_hashing(self, string):
        value = 0
        for letter in reversed(string):
            value = (value * self._x + ord(letter)) % self._prime
        return value % self._cardinality

    def add(self, string):
        hash_value = self._polynomial_hashing(string)
        chain = self._table[hash_value]
        if string not in chain:
            chain.append(string)

    def delete(self, string):
        hash_value = self._polynomial_hashing(string)
        chain = self._table[hash_value]
        for ix, item in enumerate(chain):
            if item == string:
                del chain[ix]
                return

    def find(self, string):
        hash_value = self._polynomial_hashing(string)
        chain = self._table[hash_value]
        if string in chain:
            return "yes"
        return "no"

    def check(self, k):
        return " ".join([item for item in reversed(self._table[k])])


def read_queries(reader, nr_queries):
    return [tuple(next(reader).split()) for _ in range(nr_queries)]


def process_queries(queries, cardinality):
    hash_table = HashTable(*PARAMETERS, cardinality)
    results = []
    for operation, string in queries:
        if operation == "add":
            hash_table.add(string)
        elif operation == "del":
            hash_table.delete(string)
        elif operation == "find":
            results.append(hash_table.find(string))
        else:
            content = hash_table.check(int(string))
            results.append(content)
    return results


def main():
    reader = sys.stdin
    cardinality = int(next(reader))
    nr_queries = int(next(reader))
    queries = read_queries(reader, nr_queries)
    result = process_queries(queries, cardinality)
    for item in result:
        print(item)


if __name__ == "__main__":
    main()

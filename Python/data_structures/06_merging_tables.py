import sys
from collections import namedtuple

TableOperation = namedtuple("TableOperation", ["destination", "source"])


class UnionFind(object):
    def __init__(self, number_of_tables, table_rows):
        self._table_rows = table_rows
        self._parents = list(range(number_of_tables))
        self._ranks = [0] * number_of_tables
        self._largest_table_size = max(table_rows)

    def _find(self, child_index):
        parent_index = self._parents[child_index]
        node_indices_on_path = []
        while child_index != parent_index:
            node_indices_on_path.append(child_index)
            child_index = parent_index
            parent_index = self._parents[child_index]
        root = parent_index
        for ix in node_indices_on_path:
            self._parents[ix] = root
        return root

    def union(self, source, dest):
        id_s = self._find(source)
        id_d = self._find(dest)
        if id_s == id_d:
            return self._largest_table_size
        if self._ranks[id_s] > self._ranks[id_d]:
            self._parents[id_d] = id_s
            self._table_rows[id_s] += self._table_rows[id_d]
            self._table_rows[id_d] = 0
            self._largest_table_size = max(self._largest_table_size, self._table_rows[id_s])
        else:
            self._parents[id_s] = id_d
            self._table_rows[id_d] += self._table_rows[id_s]
            self._table_rows[id_s] = 0
            if self._ranks[id_s] == self._ranks[id_d]:
                self._ranks[id_d] += 1
            self._largest_table_size = max(self._largest_table_size, self._table_rows[id_d])
        return self._largest_table_size


def convert_to_intlist(line):
    return [int(item) for item in line.split()]


def read_table_operations(reader, number_of_operations):
    operations = []
    for _ in range(number_of_operations):
        d, s = convert_to_intlist(next(reader))
        operations.append(TableOperation(destination=d-1, source=s-1))
    return operations


def process_merge_requests(table_rows, number_of_tables, operations):
    tables = UnionFind(number_of_tables, table_rows)
    return [tables.union(source, destination) for destination, source in operations]


def main():
    reader = sys.stdin
    number_of_tables, number_of_operations = convert_to_intlist(next(reader))
    table_rows = convert_to_intlist(next(reader))
    operations = read_table_operations(reader, number_of_operations)
    result = process_merge_requests(table_rows, number_of_tables, operations)
    for item in result:
        print(item)


if __name__ == "__main__":
    main()

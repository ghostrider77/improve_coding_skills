import sys

MAX_SIZE = 10000000

class PhoneBook(object):
    def __init__(self, max_size):
        self._phone_book = [None] * max_size

    def add(self, number, name):
        self._phone_book[number] = name

    def delete(self, number):
        self._phone_book[number] = None

    def find(self, number):
        return self._phone_book[number]


def read_queries(reader, nr_queries):
    return [tuple(next(reader).split()) for _ in range(nr_queries)]


def process_queries(queries):
    phone_book = PhoneBook(MAX_SIZE)
    result = []
    for query in queries:
        operation = query[0]
        number = int(query[1])
        if operation == "add":
            phone_book.add(number, query[2])
        elif operation == "del":
            phone_book.delete(number)
        else:
            res = phone_book.find(number)
            if res is None:
                result.append("not found")
            else:
                result.append(str(res))
    return result


def main():
    reader = sys.stdin
    nr_queries = int(next(reader))
    queries = read_queries(reader, nr_queries)
    result = process_queries(queries)
    for item in result:
        print(item)


if __name__ == "__main__":
    main()

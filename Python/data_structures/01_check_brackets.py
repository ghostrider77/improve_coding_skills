import sys
from collections import namedtuple

OpenedBracket = namedtuple("OpenedBracket", ["bracket", "position"])

OPENING_BRACKETS = frozenset({"(", "[", "{"})
CLOSING_BRACKETS = frozenset({")", "]", "}"})


class Stack(object):
    def __init__(self):
        self._stack = []

    @property
    def elements(self):
        return self._stack

    @property
    def is_empty(self):
        if self._stack:
            return False
        return True

    def top(self):
        return self._stack[-1]

    def push(self, elem):
        self._stack.append(elem)

    def pop(self):
        elem = self._stack.pop(-1)
        return elem


def do_brackets_match(opening_bracket, closing_bracket):
    return ((opening_bracket == "[" and closing_bracket == "]") or
            (opening_bracket == "(" and closing_bracket == ")") or
            (opening_bracket == "{" and closing_bracket == "}"))


def retrieve_failed_opening_index_from_stack(stack):
    if stack.is_empty:
        return None
    _, ix = stack.pop()
    return ix


def find_index_of_non_matching_bracket(string):
    stack = Stack()
    for ix, letter in enumerate(string):
        if letter in OPENING_BRACKETS:
            stack.push(OpenedBracket(letter, ix))
        elif letter in CLOSING_BRACKETS:
            if stack.is_empty:
                return ix
            opening_bracket, _ = stack.pop()
            if not do_brackets_match(opening_bracket, letter):
                return ix
    return retrieve_failed_opening_index_from_stack(stack)


def main():
    reader = sys.stdin
    string = next(reader)
    failed_index = find_index_of_non_matching_bracket(string)
    if failed_index is None:
        print("Success")
    else:
        print(failed_index + 1)


if __name__ == "__main__":
    main()

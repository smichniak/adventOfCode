from typing import List, Set


def getPreambleElements(codes: List[int], preamble: int) -> Set[int]:
    return set(codes[:preamble])


def codeValid(code: int, elements: Set[int]) -> bool:
    for el in elements:
        if el * 2 != code and code - el in elements:
            return True
    return False


def findInvalid(codes: List[int], preamble: int) -> int:
    elements = getPreambleElements(codes, preamble)

    for i in range(preamble, len(codes)):
        code = codes[i]
        if not codeValid(code, elements):
            return code
        else:
            elements.remove(codes[i - preamble])
            elements.add(code)


def main() -> None:
    with open('9.in') as f:
        numbers = list(map(int, f.readlines()))
    print(findInvalid(numbers, 25))


# main()

from typing import List


def addToSet(s: set, inputInts: List[int]) -> None:
    for element in inputInts:
        s.add(element)


def productOfAddingTwo(targetSum: int, inputInts: List[int], s: set) -> int:
    for element in inputInts:
        if (targetSum - element) in s:
            if not (targetSum % 2 == 0 and element == targetSum / 2 and inputInts.count(element) == 1):
                return (targetSum - element) * element
    return -1


def main() -> None:
    with open("1.in") as file:
        inputInts = list(map(int, file.read().split()))
    s = set()
    addToSet(s, inputInts)
    print(productOfAddingTwo(2020, inputInts, s))

# main()

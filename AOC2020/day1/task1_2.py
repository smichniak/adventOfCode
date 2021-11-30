from day1.task1_1 import addToSet, productOfAddingTwo, List

with open("1.in") as file:
    inputInts = list(map(int, file.read().split()))


def productOfAddingThree(targetSum: int, inputInts: List[int], s: set) -> int:
    for element in inputInts:
        prod = productOfAddingTwo(targetSum - element, inputInts, s)
        if prod != -1:
            return prod * element


def main() -> None:
    s = set()
    addToSet(s, inputInts)
    print(productOfAddingThree(2020, inputInts, s))


main()

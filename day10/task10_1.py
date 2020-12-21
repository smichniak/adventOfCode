from typing import List, Tuple


def getDifferences(adapters: List[int]) -> Tuple[int, int]:
    adapters.append(0)
    adapters.append(max(adapters) + 3)
    difference_1 = 0
    difference_3 = 0
    adapters.sort()
    for i, joltage in enumerate(adapters[:-1]):
        if adapters[i + 1] - joltage == 1:
            difference_1 += 1
        elif adapters[i + 1] - joltage == 3:
            difference_3 += 1
    return difference_1, difference_3


def main() -> None:
    with open('10.in') as f:
        numbers = list(map(int, f.readlines()))
    difference_1, difference_3 = getDifferences(numbers)
    print(difference_1 * difference_3)


main()

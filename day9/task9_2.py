from day9.task9_1 import findInvalid, List


def findencryptionWeakness(invalid: int, codes: List[int]):
    start = 0
    end = 1
    contiguous_sum = codes[start] + codes[end]
    while contiguous_sum != invalid:
        if contiguous_sum < invalid:
            end += 1
            contiguous_sum += codes[end]
        else:
            contiguous_sum -= codes[start]
            start += 1
    return min(codes[start: end + 1]) + max(codes[start: end + 1])


def main() -> None:
    with open('9.in') as f:
        numbers = list(map(int, f.readlines()))
    invalid = findInvalid(numbers, 25)
    print(findencryptionWeakness(invalid, numbers))


main()

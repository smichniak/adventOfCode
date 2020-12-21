from typing import List


def arrangements(adapters: List[int]) -> int:
    adapters.sort()
    device_joltage = max(adapters)
    adapters.append(device_joltage)
    # for given key stores how many arrangements end with the number key
    how_many_ways = {0: 1}
    for joltage in adapters:
        how_many_ways[joltage] = 0
        for can_adapt in [joltage - 3, joltage - 2, joltage - 1]:
            if can_adapt in how_many_ways.keys():
                how_many_ways[joltage] += how_many_ways[can_adapt]

    return how_many_ways[device_joltage]


def main() -> None:
    with open('10.in') as f:
        numbers = list(map(int, f.readlines()))
    print(arrangements(numbers))

main()
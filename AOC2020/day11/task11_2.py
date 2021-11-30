from typing import List
from day11 import task11_1


def sumOfNeighbours(array: List[List[str]], i: int, j: int) -> int:
    neighbours = range(-1, 2)
    neighbour_sum = 0
    m = len(array[0])
    n = len(array)
    for di in neighbours:
        for dj in neighbours:
            if di != 0 or dj != 0:
                dj_change = 0
                di_change = 0
                while 0 <= i + di + di_change < n and 0 <= j + dj + dj_change < m and array[i + di + di_change][
                    j + dj + dj_change] == '.':
                    if dj < 0:
                        dj_change -= 1
                    elif dj > 0:
                        dj_change += 1
                    if di < 0:
                        di_change -= 1
                    elif di > 0:
                        di_change += 1
                if 0 <= i + di + di_change < n and 0 <= j + dj + dj_change < m and array[i + di + di_change][
                    j + dj + dj_change] == '#':
                    neighbour_sum += 1
    return neighbour_sum


task11_1.sumOfNeighbours = sumOfNeighbours

def main() -> None:
    with open("11.in") as file:
        f = file.read()
        lines = f.splitlines()
    with_guards = task11_1.addGuards(lines)
    steps = -1
    different = True
    while different:
        with_guards, different, occupied = task11_1.nextStep(with_guards, 5)
        steps += 1
    print(occupied)


main()

from typing import List, Tuple
import copy


def addGuards(lines: List[str]) -> List[List[str]]:
    line_len = len(lines[0]) + 2
    array = [['.'] * line_len ]
    for i, line in enumerate(lines):
        array.append(list('.' + line + '.'))
    array.append(['.'] * line_len)
    return array


def sumOfNeighbours(array: List[List[str]], i: int, j: int) -> int:
    neighbours = range(-1, 2)
    neighbour_sum = 0
    for di in neighbours:
        for dj in neighbours:
            if (di != 0 or dj != 0) and array[i + di][j + dj] == '#':
                neighbour_sum += 1
    return neighbour_sum


# true if something changed
def nextStep(array: List[List[str]], occupied_threshold: int) -> Tuple[List[List[str]], bool, int]:
    m = len(array[0]) - 2
    n = len(array) - 2
    buffor = copy.deepcopy(array)
    occupied = 0
    for i in range(1, n + 1):
        for j in range(1, m + 1):
            if array[i][j] == '.':
                buffor[i][j] = array[i][j]
            else:
                neighbour_sum = sumOfNeighbours(array, i, j)
                if array[i][j] == 'L' and neighbour_sum == 0:
                    buffor[i][j] = '#'
                elif array[i][j] == '#' and neighbour_sum >= occupied_threshold:
                    buffor[i][j] = 'L'
                else:
                    buffor[i][j] = array[i][j]
                if buffor[i][j] == '#':
                    occupied += 1
    return buffor, buffor != array, occupied


def main() -> None:
    with open("11.in") as file:
        f = file.read()
        lines = f.splitlines()
    with_guards = addGuards(lines)
    steps = -1
    different = True
    while different:
        with_guards, different, occupied = nextStep(with_guards, 4)
        steps += 1
    print(occupied)


# main()

from typing import List, Tuple


def run(lines: List[str]) -> Tuple[int, bool]:
    accumulator = 0
    visited = set()
    i = 0
    while i not in visited and i < len(lines):
        visited.add(i)
        instruction, value_str = lines[i].split(' ')
        value = int(value_str)
        if instruction == 'jmp':
            i += value
        else:
            accumulator += (instruction == 'acc') * value
            i += 1

    return accumulator, i < len(lines)


def main() -> None:
    with open("8.in") as file:
        lines = file.read().splitlines()
    print(run(lines)[0])


# main()

from typing import List


def countTrees(lines: List[str], step_x, step_y, x=0, y=0, trees=0) -> int:
    if y == len(lines) - 1:
        return trees + int(lines[y][x] == '#')
    elif y > len(lines) - 1:
        return trees
    else:
        new_x = (x + step_x) % len(lines[0])
        new_y = y + step_y
        return countTrees(lines, step_x, step_y, new_x, new_y, trees + int(lines[y][x] == '#'))


def main() -> None:
    with open("3.in") as file:
        f = file.read()
        lines = f.splitlines()
    print(countTrees(lines, 3, 1))


# main()

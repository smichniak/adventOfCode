from typing import List
import queue


def getOuterColor(line: str) -> str:
    words = line.split(' ')
    return words[0] + ' ' + words[1]


def getInnerColors(line: str) -> List[str]:
    colors = []
    if 'other' in line:
        return colors
    i = 5
    words = line.split(' ')
    while i + 1 < len(words):
        colors.append(words[i] + ' ' + words[i + 1])
        i += 4
    return colors


def makeCanBeIn(lines) -> dict:
    can_be_in = {}
    for line in lines:
        outer = getOuterColor(line)
        for color in getInnerColors(line):
            if color not in can_be_in.keys():
                can_be_in[color] = []
            can_be_in[color].append(outer)
    return can_be_in


def BFS(can_be_in: dict, start: str) -> set:
    q = queue.Queue()
    visited = set()
    for color in can_be_in[start]:
        q.put(color)
        visited.add(color)
    while not q.empty():
        col = q.get()
        if col in can_be_in.keys():
            for outer in can_be_in[col]:
                if outer not in visited:
                    visited.add(outer)
                    q.put(outer)
    return visited


def main() -> None:
    with open("7.in") as file:
        f = file.read()
        lines = f.splitlines()
    can_be_in = makeCanBeIn(lines)
    can_be_outer = BFS(can_be_in, 'shiny gold')
    print(len(can_be_outer))


# main()

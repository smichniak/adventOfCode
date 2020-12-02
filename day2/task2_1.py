from typing import Tuple


def getBound(s: str) -> Tuple[int, int]:
    parts = s.split('-')
    return int(parts[0]), int(parts[-1])


def getLetter(s: str) -> str:
    return s[0]


def isValidPassword(line: str) -> bool:
    line_parts = line.split(' ')
    lower_bound, upper_bound = getBound(line_parts[0])
    letter = getLetter(line_parts[1])
    how_may = line_parts[2].count(letter)
    return how_may <= upper_bound and how_may >= lower_bound


def main() -> None:
    valid = 0
    with open("2.in") as file:
        f = file.read()
        for line in f.splitlines():
            if isValidPassword(line):
                valid += 1
    print(valid)

# main()
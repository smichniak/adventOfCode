from day2.task2_1 import getBound, getLetter


def isValidPassword(line: str) -> bool:
    line_parts = line.split(' ')
    first_index, second_index = getBound(line_parts[0])
    letter = getLetter(line_parts[1])
    return (line_parts[2][first_index - 1] == letter) != (line_parts[2][second_index - 1] == letter)


def main() -> None:
    valid = 0
    with open("2.in") as file:
        f = file.read()
        for line in f.splitlines():
            if isValidPassword(line):
                valid += 1
    print(valid)


main()

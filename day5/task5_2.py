from day5.task5_1 import getRowOrColumn


def main() -> None:
    with open("5.in") as file:
        f = file.read()
        lines = f.splitlines()
    ids = set()
    for line in lines:
        col = getRowOrColumn(line[7:], 7)
        row = getRowOrColumn(line[:7], 127)
        ids.add(8 * row + col)

    for row in range(1, 127):
        for col in range(8):
            id = 8 * row + col
            if id not in ids and id + 1 in ids and id - 1 in ids:
                print(id)


main()

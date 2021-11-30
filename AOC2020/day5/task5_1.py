def getRowOrColumn(directions: str, end) -> int:
    left = 0
    right = end
    for char in directions:
        if char == 'F' or char == 'L':
            right = left + (right - left) // 2
        else:
            left = left + (right - left) // 2 + 1
    return right


def main() -> None:
    with open("5.in") as file:
        f = file.read()
        lines = f.splitlines()
    max_id = 0
    for line in lines:
        col = getRowOrColumn(line[7:], 7)
        row = getRowOrColumn(line[:7], 127)
        id = 8 * row + col
        max_id = max(max_id, id)
    print(max_id)


# main()

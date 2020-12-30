from typing import Tuple


def movePoint(instruction: Tuple[str, int], distance_x: int, distance_y: int, direction: str, scale_x=1, scale_y=1) -> \
        Tuple[int, int, str]:
    directions = ['E', 'S', 'W', 'N']
    instruction_type, number = instruction
    if instruction_type == 'F':
        instruction_type = direction

    if instruction_type == 'N':
        distance_y += number * scale_y
    elif instruction_type == 'S':
        distance_y -= number * scale_y
    elif instruction_type == 'E':
        distance_x += number * scale_x
    elif instruction_type == 'W':
        distance_x -= number * scale_x
    elif instruction_type == 'R':
        direction = directions[(directions.index(direction) + number // 90) % len(directions)]
    elif instruction_type == 'L':
        direction = directions[(directions.index(direction) - number // 90) % len(directions)]

    return distance_x, distance_y, direction


def getInstruction(line: str) -> Tuple[str, int]:
    return line[0], int(line[1:])


def main() -> None:
    with open('12.in') as f:
        instructions = list(map(getInstruction, f.readlines()))
    distance_x, distance_y, direction = 0, 0, 'E'

    for instruction in instructions:
        distance_x, distance_y, direction = movePoint(instruction, distance_x, distance_y, direction)
    print(abs(distance_x) + abs(distance_y))

# main()

from day12.task12_1 import movePoint, getInstruction, Tuple
from math import sin, cos, radians


def rotate(waypoint_x: int, waypoint_y: int, degrees: int) -> Tuple[int, int]:
    return int(round(waypoint_x * cos(radians(degrees)) - waypoint_y * sin(radians(degrees)))), \
           int(round(waypoint_x * sin(radians(degrees)) + waypoint_y * cos(radians(degrees))))


def evaluateInstruction(instruction: Tuple[str, int], ship_x: int, ship_y: int, waypoint_x: int,
                        waypoint_y: int) -> Tuple[int, int, int, int]:
    instruction_type, number = instruction
    if instruction_type == 'L':
        waypoint_x, waypoint_y = rotate(waypoint_x, waypoint_y, number)
    elif instruction_type == 'R':
        waypoint_x, waypoint_y = rotate(waypoint_x, waypoint_y, -number)
    elif instruction_type == 'F':
        ship_x, ship_y, _ = movePoint(('E', number), ship_x, ship_y, '', waypoint_x)
        ship_x, ship_y, _ = movePoint(('N', number), ship_x, ship_y, '', 1, waypoint_y)

    else:
        waypoint_x, waypoint_y, _ = movePoint(instruction, waypoint_x, waypoint_y, '')
    return ship_x, ship_y, waypoint_x, waypoint_y


def main() -> None:
    with open('12.in') as f:
        instructions = list(map(getInstruction, f.readlines()))
    distance_x, distance_y = 0, 0
    waypoint_x, waypoint_y = 10, 1

    for instruction in instructions:
        distance_x, distance_y, waypoint_x, waypoint_y = evaluateInstruction(instruction, distance_x, distance_y, waypoint_x, waypoint_y)
    print(abs(distance_x) + abs(distance_y))


main()

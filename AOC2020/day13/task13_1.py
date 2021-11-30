from typing import List, Tuple


def closestTime(min_time: int, interval: int) -> int:
    min_bus_time = (min_time // interval) * interval
    return min_bus_time + (min_bus_time < min_time) * interval


def bestBus(min_time: int, ids: List[int]) -> Tuple[int, int]:
    best_time = -1
    best_id = -1
    for id in ids:
        curr_time = closestTime(min_time, id)
        if best_time == -1 or curr_time < best_time:
            best_time = curr_time
            best_id = id
    return best_time, best_id


def main() -> None:
    with open('13.in') as f:
        lines = f.readlines()
    min_time = int(lines[0])
    ids = list(map(int, lines[1].replace('x,', '').split(',')))
    best_time, best_id = bestBus(min_time, ids)
    print((best_time - min_time) * best_id)


# main()

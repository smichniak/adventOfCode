from typing import List, Tuple
from math import prod

# For list [(n_1, a_1), ..., (n_k, a_k)] and product = n_1 * ... * n_k
# Solves system of equations:
# x = a_1 mod n_1
# ...
# x = a_k mod n_k
def solveEquations(positions: List[Tuple[int, int]], product: int) -> int:
    x = 0
    for n, a in positions:
        m = product // n
        x += a * m * pow(m, -1, n)

    return x % product


def getPositions(ids: List[int]) -> List[Tuple[int, int]]:
    positions = []
    for i, id in enumerate(ids):
        if id != -1:
            positions.append((id, -i % id))
    return positions


def main() -> None:
    with open('13.in') as f:
        lines = f.readlines()
    ids = list(map(int, lines[1].replace('x', '-1').split(',')))
    product = -prod(ids)
    positions = getPositions(ids)
    print(solveEquations(positions, product))


main()

from day3.task3_1 import countTrees


def main() -> None:
    STEPS_TO_TEST = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

    product = 1
    with open("3.in") as file:
        f = file.read()
        lines = f.splitlines()
    for (step_x, step_y) in STEPS_TO_TEST:
        product *= countTrees(lines, step_x, step_y)
    print(product)


main()

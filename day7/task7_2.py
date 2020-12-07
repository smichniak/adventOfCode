from day7.task7_1 import getInnerColors, getOuterColor, List


def howMany(line: str) -> List[int]:
    other_bags = []
    if 'other' in line:
        return other_bags
    words = line.split(' ')
    for word in words:
        if word.isnumeric():
            other_bags.append(int(word))
    return other_bags

def getHowMany(bags: dict, color: str) -> int:
    inner_colors, how_many = bags[color]
    if len(inner_colors) == 0:
        return 0
    else:
        bag_sum = sum(how_many)
        for inner, how_many_bags in zip(inner_colors, how_many):
            bag_sum += how_many_bags * getHowMany(bags, inner)
        return bag_sum



def main() -> None:
    bag_colors = {}
    with open("7.in") as file:
        f = file.read()
        lines = f.splitlines()
    for line in lines:
        bag_colors[getOuterColor(line)] = [getInnerColors(line), howMany(line)]
    print(getHowMany(bag_colors, 'shiny gold'))




main()

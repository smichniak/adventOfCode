from day6.task6_1 import getGroups


def allPositiveAnswers(group: str) -> int:
    answers = group.splitlines()
    positive = set(list(answers[0]))

    for answer in answers[1:]:
        to_remove = []
        for positive_answer in positive:
            if positive_answer not in answer:
                to_remove.append(positive_answer)
        positive.difference_update(to_remove)
    return len(positive)


def main() -> None:
    total = 0
    for group in getGroups('6.in'):
        total += allPositiveAnswers(group)
    print(total)


main()

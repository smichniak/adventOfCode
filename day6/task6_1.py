from typing import List

def getGroups(intput_file: str) -> List[str]:
    with open(intput_file) as file:
        f = file.read()
        groups = f.split('\n\n')
    return groups

def positiveAnswers(group: str) -> int:
    answers = group.replace('\n', '')
    postive = set()
    for answer in answers:
        postive.add(answer)
    return len(postive)

def main() -> None:
    total = 0
    for group in getGroups('6.in'):
        total += positiveAnswers(group)
    print(total)


# main()
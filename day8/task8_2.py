from day8.task8_1 import run, List


def runSubstitutions(lines: List[str]) -> int:
    switch = {'jmp': 'nop', 'nop': 'jmp'}
    for i, line in enumerate(lines):
        instruction, value_str = line.split(' ')
        if instruction in switch:
            lines[i] = switch[instruction] + ' ' + value_str
            acc, loops = run(lines)
            if not loops:
                return acc
            else:
                lines[i] = instruction + ' ' + value_str




def main() -> None:
    with open("8.in") as file:
        f = file.read()
        lines = f.splitlines()
    print(runSubstitutions(lines))


main()

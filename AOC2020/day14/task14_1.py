from typing import Tuple


def getIndexVal(line: str, mask: str) -> Tuple[int, str]:
    parts = line.split(' ')
    index = int(parts[0][4:-1])
    value = int(parts[2])
    binary_value = str(bin(value))[2:]
    val_36_bit = '0' * (len(mask) - len(binary_value) - 1) + binary_value
    return index, val_36_bit


def changeMemory(line: str, mask: str) -> Tuple[int, int]:
    index, val_36_bit = getIndexVal(line, mask)
    new_val = []
    for mask_bit, val_bit in zip(mask, val_36_bit):
        if mask_bit == 'X':
            new_val.append(val_bit)
        else:
            new_val.append(mask_bit)

    return index, int(''.join(new_val), 2)


def getMask(line: str) -> str:
    return line.split(' ')[2]


def main() -> None:
    with open('14.in') as f:
        lines = f.readlines()
    memory = {}
    mask = ''
    for line in lines:
        if 'mask' in line:
            mask = getMask(line)
        else:
            index, value = changeMemory(line, mask)
            memory[index] = value
    print(sum(memory.values()))

# main()

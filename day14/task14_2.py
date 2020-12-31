from day14.task14_1 import getMask, getIndexVal
from typing import Dict, List


def modifyMemory(memory: Dict[int, int], address: List[str], value: int):
    if 'X' not in address:
        memory[int(''.join(address), 2)] = value
    else:
        first_X = address.index('X')
        copy0 = address.copy()
        copy1 = address.copy()
        copy0[first_X] = '0'
        copy1[first_X] = '1'
        modifyMemory(memory, copy0, value)
        modifyMemory(memory, copy1, value)


def newAdress(index: int, mask: str) -> List[str]:
    binary_index = str(bin(index))[2:]
    index36 = '0' * (len(mask) - len(binary_index) - 1) + binary_index
    new_index = []
    for mask_bit, index_bit in zip(mask, index36):
        if mask_bit == '0':
            new_index.append(index_bit)
        elif mask_bit == '1':
            new_index.append('1')
        else:
            new_index.append('X')

    return new_index


def main() -> None:
    with open('14.in') as f:
        lines = f.readlines()
    memory = {}
    mask = ''
    for line in lines:
        if 'mask' in line:
            mask = getMask(line)
        else:
            index, value = getIndexVal(line, mask)
            index_list = newAdress(index, mask)
            modifyMemory(memory, index_list, int(value, 2))
    print(sum(memory.values()))

main()
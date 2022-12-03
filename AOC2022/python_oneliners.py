print('Day 1:')
print(max([sum([int(x) for x in elf.split('\n') if x != '']) for elf in ''.join(open('input/day1.in', 'r')).split('\n\n')]))
print(sum(sorted([sum([int(x) for x in elf.split('\n') if x != '']) for elf in ''.join(open('input/day1.in', 'r')).split('\n\n')])[-3:]))
print()

print('Day 2:')
print(sum([(my_move := ord(me) - ord('X')) + 1 + 3 * ((op_move := ord(opponent) - ord('A')) == my_move) + 6 * ((my_move - 1) % 3 == op_move) for opponent, me in map(lambda x: x.strip().split(' '), open('input/day2.in', 'r'))]))
print(sum([3 * (my_move := ord(me) - ord('X')) + (ord(opponent) - ord('A') + (my_move - 1)) % 3 + 1  for opponent, me in map(lambda x: x.strip().split(' '), open('input/day2.in', 'r'))]))
print()

print('Day 3:')
print(sum([ord(letter) - ord('A') + 27 if (letter := list(set(line[:len(line) // 2]) & set(line[len(line) // 2:]))[0]) < 'a' else ord(letter) - ord('a') + 1 for line in open('input/day3.in', 'r')]))
print(sum([ord(letter) - ord('A') + 27 if (letter := list(set(a.strip()) & set(b.strip()) & set(c.strip()))[0]) < 'a' else ord(letter) - ord('a') + 1 for a, b, c in zip(list(open('input/day3.in', 'r'))[::3], list(open('input/day3.in', 'r'))[1::3], list(open('input/day3.in', 'r'))[2::3])]))

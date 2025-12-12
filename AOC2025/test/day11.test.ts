import { part1, part2 } from "../src/day11";

describe("Day 11", () => {
    const exampleInput = `
aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out
`;

    const exampleInput2 = `
svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out
`;

    test("Part 1 Example", () => {
        expect(part1(exampleInput.trim())).toBe(5);
    });

    test("Part 2 Example", () => {
        expect(part2(exampleInput2.trim())).toBe(2);
    });
});

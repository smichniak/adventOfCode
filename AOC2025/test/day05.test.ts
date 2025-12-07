import { part1, part2 } from "../src/day05";

describe("Day 5", () => {
    const exampleInput = `
3-5
10-14
16-20
12-18

1
5
8
11
17
32
`;

    test("Part 1 Example", () => {
        expect(part1(exampleInput.trim())).toBe(3);
    });

    test("Part 2 Example", () => {
        expect(part2(exampleInput.trim())).toBe(14);
    });
});

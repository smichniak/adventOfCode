import { part1, part2 } from "../src/day09";

describe("Day 9", () => {
    const exampleInput = `
7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3
`;

    test("Part 1 Example", () => {
        expect(part1(exampleInput.trim())).toBe(50);
    });

    test("Part 2 Example", () => {
        expect(part2(exampleInput.trim())).toBe(24);
    });
});

import { part1, part2 } from "../src/day03";

describe("Day 3", () => {
    const exampleInput = `
987654321111111
811111111111119
234234234234278
818181911112111
`;

    test("Part 1 Example", () => {
        expect(part1(exampleInput.trim())).toBe(357);
    });

    test("Part 2 Example", () => {
        expect(part2(exampleInput.trim())).toBe(3121910778619);
    });
});

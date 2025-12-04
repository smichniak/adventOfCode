import { part1, part2 } from "../src/day04";

describe("Day 4", () => {
    const exampleInput = `
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
`;

    test("Part 1 Example", () => {
        expect(part1(exampleInput.trim())).toBe(13);
    });

    test("Part 2 Example", () => {
        expect(part2(exampleInput.trim())).toBe(43);
    });
});

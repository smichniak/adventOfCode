import { part1, part2 } from "../src/day01";

describe("Day 1", () => {
    const exampleInput = `
    L68
    L30
    R48
    L5
    R60
    L55
    L1
    L99
    R14
    L82
`;

    test("Part 1 Example", () => {
        expect(part1(exampleInput.trim())).toBe(3);
    });

    test("Part 2 Example", () => {
        expect(part2(exampleInput.trim())).toBe("TODO");
    });
});

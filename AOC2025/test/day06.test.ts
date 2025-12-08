import { part1, part2 } from "../src/day06";

describe("Day 6", () => {
    const exampleInput = `
123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  
`;

    test("Part 1 Example", () => {
        expect(part1(exampleInput.trim())).toBe(4277556);
    });

    test("Part 2 Example", () => {
        expect(part2(exampleInput.trim())).toBe(3263827);
    });
});

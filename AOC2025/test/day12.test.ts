import { part1, part2 } from "../src/day12";

describe("Day 12", () => {
    const exampleInput = `
0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2
`;

    test("Part 1 Example", () => {
        expect(part1(exampleInput.trim())).toBe(2);
    });

    test("Part 2 Example", () => {
        expect(part2(exampleInput.trim())).toBe("No puzzle in part 2!");
    });
});

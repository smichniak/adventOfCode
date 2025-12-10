import { parseLines, findAllIndices, StringSet, StringMap, sum, Coordinate2D } from "./utils";

type Input = {
    start: Coordinate2D;
    splitters: StringSet<Coordinate2D>;
    height: number;
};

function parseInput(input: string): Input {
    const lines = parseLines(input);
    const start = { x: lines[0].indexOf("S"), y: 0 };
    const splitters = new StringSet<Coordinate2D>(
        lines.flatMap((line, y) =>
            findAllIndices(Array.from(line), (c) => c === "^").map((x) => ({ x, y }))
        )
    );
    const height = lines.length;

    return { start, splitters, height };
}

export function part1(input: string): number | string {
    const { start, splitters, height } = parseInput(input);
    const visited = new StringSet<Coordinate2D>();
    const usedSplitters = new StringSet<Coordinate2D>();
    const stack = [start];
    while (stack.length > 0) {
        const { x, y } = stack.pop()!;
        if (visited.has({ x, y }) || y >= height) continue;
        visited.add({ x, y });
        if (splitters.has({ x, y })) {
            usedSplitters.add({ x, y });
            stack.push({ x: x - 1, y }, { x: x + 1, y });
        } else {
            stack.push({ x, y: y + 1 });
        }
    }

    return usedSplitters.size;
}

function addPath(
    overlappingPaths: StringMap<Coordinate2D, number>,
    { x, y }: Coordinate2D,
    count: number
) {
    overlappingPaths.set({ x, y }, (overlappingPaths.get({ x, y }) ?? 0) + count);
}

export function part2(input: string): number | string {
    const { start, splitters, height } = parseInput(input);
    const overlappingPaths = new StringMap<Coordinate2D, number>();
    const visited = new StringSet<Coordinate2D>();

    const queue = [start];
    overlappingPaths.set(start, 1);

    while (queue.length > 0) {
        const { x, y } = queue.shift()!;
        if (y >= height || visited.has({ x, y })) continue;
        visited.add({ x, y });
        if (splitters.has({ x, y })) {
            queue.push({ x: x - 1, y: y + 1 }, { x: x + 1, y: y + 1 });
            addPath(overlappingPaths, { x: x - 1, y: y + 1 }, overlappingPaths.get({ x, y })!);
            addPath(overlappingPaths, { x: x + 1, y: y + 1 }, overlappingPaths.get({ x, y })!);
        } else {
            addPath(overlappingPaths, { x, y: y + 1 }, overlappingPaths.get({ x, y })!);
            queue.push({ x, y: y + 1 });
        }
    }

    return sum(
        Array.from(overlappingPaths.entries())
            .filter(([{ y }]) => y == height - 1)
            .map(([, count]) => count)
    );
}

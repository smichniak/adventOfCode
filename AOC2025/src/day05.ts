type Input = {
    ranges: [number, number][];
    numbers: number[];
};

function parseInput(input: string): Input {
    const [ranges, numbers] = input.split("\n\n");
    return {
        ranges: ranges.split("\n").map((range) => range.split("-").map(Number) as [number, number]),
        numbers: numbers.split("\n").map(Number),
    };
}

function isInAnyRange(number: number, ranges: [number, number][]): boolean {
    return ranges.some((range) => range[0] <= number && number <= range[1]);
}

export function part1(input: string): number | string {
    const { ranges, numbers } = parseInput(input);
    return numbers.filter((number) => isInAnyRange(number, ranges)).length;
}

function mergeRanges(ranges: [number, number][]): [number, number][] {
    return ranges
        .sort((a, b) => a[0] - b[0])
        .reduce((acc, [start, end]) => {
            if (acc.length === 0 || acc[acc.length - 1][1] < start) {
                acc.push([start, end]);
            } else {
                acc[acc.length - 1][1] = Math.max(acc[acc.length - 1][1], end);
            }
            return acc;
        }, [] as [number, number][]);
}

export function part2(input: string): number | string {
    const { ranges } = parseInput(input);
    const mergedRanges = mergeRanges(ranges);
    return mergedRanges.reduce((acc, [start, end]) => acc + (end - start + 1), 0);
}

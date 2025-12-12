import { parseLines } from "./utils";

const START_NODE = "you";
const END_NODE = "out";

const START_NODE2 = "svr";
const DAC = "dac";
const FFT = "fft";

function parseInput(input: string): Map<string, string[]> {
    return new Map(
        parseLines(input).map((line) => {
            const [key, value] = line.split(": ");
            return [key, value.split(" ")];
        })
    );
}

export function part1(input: string): number | string {
    const graph = parseInput(input);
    const memo = new Map<string, number>();

    const countPaths = (node: string): number => {
        if (node === END_NODE) return 1;
        if (memo.has(node)) return memo.get(node)!;

        const count =
            graph.get(node)?.reduce((sum, neighbor) => sum + countPaths(neighbor), 0) ?? 0;
        memo.set(node, count);
        return count;
    };

    return countPaths(START_NODE);
}

export function part2(input: string): number | string {
    const graph = parseInput(input);
    const memo = new Map<string, { none: number; dac: number; fft: number; both: number }>();

    const countPaths = (node: string) => {
        if (node === END_NODE) return { none: 1, dac: 0, fft: 0, both: 0 };
        if (memo.has(node)) return memo.get(node)!;

        const counts = { none: 0, dac: 0, fft: 0, both: 0 };
        for (const neighbor of graph.get(node) ?? []) {
            const res = countPaths(neighbor);
            counts.none += res.none;
            counts.dac += res.dac;
            counts.fft += res.fft;
            counts.both += res.both;
        }

        if (node === DAC) {
            counts.dac += counts.none;
            counts.both += counts.fft;
            counts.none = 0;
            counts.fft = 0;
        } else if (node === FFT) {
            counts.fft += counts.none;
            counts.both += counts.dac;
            counts.none = 0;
            counts.dac = 0;
        }

        memo.set(node, counts);
        return counts;
    };

    return countPaths(START_NODE2).both;
}

import { parseLines, transpose, sum, zip } from "./utils";

type Input = {
    grid: number[][];
    operations: string[];
};

function parseInput(input: string): Input {
    const lines = parseLines(input);
    const operations = lines[lines.length - 1].split(/\s+/).filter((s) => s !== "");
    const grid = lines.slice(0, -1).map((line) =>
        line
            .split(/\s+/)
            .filter((s) => s !== "")
            .map(Number)
    );
    return { grid, operations };
}

function applyOperation(numList: number[], operation: string): number {
    switch (operation) {
        case "+":
            return sum(numList);
        case "*":
            return numList.reduce((a, b) => a * b, 1);
        default:
            throw new Error(`Unknown operation: ${operation}`);
    }
}

export function part1(input: string): number | string {
    const { grid, operations } = parseInput(input);
    const columns = transpose(grid);

    return sum(
        zip(columns, operations).map(([numList, operation]) => applyOperation(numList, operation))
    );
}

export function part2(input: string): number | string {
    const { grid } = parseInput(input);
    const lines = parseLines(input);
    const opLine = lines.pop()!;

    const colWidths = transpose(grid).map((col) =>
        Math.max(...col.map((n) => n.toString().length))
    );

    const ops = opLine
        .split("")
        .map((op, index) => ({ op, index }))
        .filter(({ op }) => op === "+" || op === "*");

    return sum(
        ops.map(({ op, index }, i) => {
            const width = colWidths[i];
            const colNums = Array.from({ length: width }, (_, offset) => {
                const numStr = lines.map((line) => line[index + offset]).join("");
                return Number(numStr);
            });
            return applyOperation(colNums, op);
        })
    );
}

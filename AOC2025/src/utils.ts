import * as fs from "fs";
import * as path from "path";

export function readInput(day: number): string {
    const dayStr = day.toString().padStart(2, "0");
    const inputPath = path.join(__dirname, "..", "input", `day${dayStr}.in`);
    try {
        return fs.readFileSync(inputPath, "utf-8").trim();
    } catch (error) {
        console.error(`Error reading input for day ${day}:`, error);
        process.exit(1);
    }
}

export function parseLines(input: string): string[] {
    return input.split(/\r?\n/);
}

export function sum(numbers: Array<number>): number {
    return numbers.reduce((a, b) => a + b, 0);
}

export function transpose<T>(matrix: Array<Array<T>>): Array<Array<T>> {
    if (matrix.length === 0) {
        return [];
    }
    return matrix[0].map((col, i) => matrix.map((row) => row[i]));
}

export function zip<T, U>(a: Array<T>, b: Array<U>): Array<[T, U]> {
    return a.map((item, i) => [item, b[i]]);
}

export function findAllIndices<T>(l: T[], predicate: (x: T) => boolean): number[] {
    return l.map((x, i) => (predicate(x) ? i : -1)).filter((x) => x !== -1);
}

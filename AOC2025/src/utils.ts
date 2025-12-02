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

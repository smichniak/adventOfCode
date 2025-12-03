import { parseLines, sum } from "./utils";

function parseLine(line: string): number[] {
    return line.split("").map(Number);
}

function bankJoltageDigits(bank: number[], digitsLeft: number): number {
    if (digitsLeft === 0) {
        return 0;
    }

    const usableSlice = digitsLeft > 1 ? bank.slice(0, -digitsLeft + 1) : bank;
    const maxNum = Math.max(...usableSlice);
    const maxNumIndex = usableSlice.indexOf(maxNum);
    return (
        maxNum * Math.pow(10, digitsLeft - 1) +
        bankJoltageDigits(bank.slice(maxNumIndex + 1), digitsLeft - 1)
    );
}

export function part1(input: string): number | string {
    const banks = parseLines(input).map(parseLine);
    const joltage = banks.map((bank) => bankJoltageDigits(bank, 2));
    return sum(joltage);
}

export function part2(input: string): number | string {
    const banks = parseLines(input).map(parseLine);
    const joltage = banks.map((bank) => bankJoltageDigits(bank, 12));
    return sum(joltage);
}

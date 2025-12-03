import { parseLines } from "./utils";

function getRange(range: string): [number, number] {
    const [start, end] = range.split("-").map(Number);
    return [start, end];
}

function parseLine(line: string): [number, number][] {
    return line.split(",").map(getRange);
}

function isInRange(number: number, range: [number, number]): boolean {
    return number >= range[0] && number <= range[1];
}

function isInAnyRange(number: number, ranges: [number, number][]): boolean {
    return ranges.some((range) => isInRange(number, range));
}

function numDigits(number: number): number {
    return (Math.log(number) * Math.LOG10E + 1) | 0;
}

function decimalShift(number: number, shift: number): number {
    return number * Math.pow(10, shift);
}

export function part1(input: string): number | string {
    const ranges = parseLine(parseLines(input)[0]);
    const maxNum = Math.max(...ranges.flat());
    const halfMaxNum = decimalShift(maxNum, -numDigits(maxNum) / 2);
    let idSum = 0;

    for (let i = 1; i < halfMaxNum; i++) {
        const doubleNum = decimalShift(i, numDigits(i)) + i;
        if (isInAnyRange(doubleNum, ranges)) {
            idSum += doubleNum;
        }
    }

    return idSum;
}

export function part2(input: string): number | string {
    const ranges = parseLine(parseLines(input)[0]);
    const maxNum = Math.max(...ranges.flat());
    const maxNumDigits = numDigits(maxNum);
    const halfMaxNum = decimalShift(maxNum, -maxNumDigits / 2);
    const usedNums = new Set<number>();
    let idSum = 0;

    for (let i = 1; i < halfMaxNum; i++) {
        const iDigits = numDigits(i);
        let copiedNum = i;
        for (let j = 2; j <= maxNumDigits; j++) {
            copiedNum = decimalShift(copiedNum, iDigits) + i;

            if (isInAnyRange(copiedNum, ranges) && !usedNums.has(copiedNum)) {
                idSum += copiedNum;
                usedNums.add(copiedNum);
            }
        }
    }

    return idSum;
}

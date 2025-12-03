import { parseLines } from "./utils";

const DIAL_START = 50;
const DIAL_SIZE = 100;

enum Direction {
    LEFT = "L",
    RIGHT = "R",
}

interface Movement {
    direction: Direction;
    steps: number;
}

function positiveModulo(a: number, b: number): number {
    return ((a % b) + b) % b;
}

function lineParser(line: string): Movement {
    const direction = line[0] as Direction;
    const steps = parseInt(line.slice(1), 10);
    return { direction, steps };
}

export function part1(input: string): number | string {
    const movements = parseLines(input).map(lineParser);
    let currentPosition: number = DIAL_START;

    let zeros: number = 0;
    for (const movement of movements) {
        let diff = movement.steps;
        if (movement.direction === Direction.LEFT) {
            diff = -diff;
        }

        currentPosition = (currentPosition + diff) % DIAL_SIZE;

        if (currentPosition === 0) {
            zeros++;
        }
    }

    return zeros;
}

export function part2(input: string): number | string {
    const movements = parseLines(input).map(lineParser);
    let currentPosition: number = DIAL_START;

    let zeros: number = 0;
    for (const movement of movements) {
        let diff = movement.steps;
        if (movement.direction === Direction.LEFT) {
            diff = -diff;
        }

        if (diff > 0) {
            zeros += Math.floor((diff + currentPosition) / DIAL_SIZE);
        } else if (diff < 0) {
            zeros += Math.floor((DIAL_SIZE - currentPosition - diff) / DIAL_SIZE);
            if (currentPosition === 0) {
                zeros--;
            }
        }

        currentPosition = positiveModulo(currentPosition + diff, DIAL_SIZE);
    }

    return zeros;
}

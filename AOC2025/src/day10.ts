import { solve, equalTo } from "yalps";
import { Model, Constraint, Coefficients } from "yalps";

import { parseLines, sum } from "./utils";

type Machine = {
    targetLights: number;
    switches: number[][];
    joltage: number[];
};

function parseMachine(line: string): Machine {
    const lightsMatch = line.match(/^\[([.#]+)\]/);
    const joltageMatch = line.match(/\{([\d,]+)\}$/);

    if (!lightsMatch || !joltageMatch) {
        throw new Error(`Invalid machine format: ${line}`);
    }

    const targetLightsList = lightsMatch[1].split("").map((c) => c === "#");
    const targetLights = targetLightsList.reverse().reduce((res, x) => (res << 1) | (x ? 1 : 0), 0);
    const joltage = joltageMatch[1].split(",").map(Number);

    const switches: number[][] = [];
    const switchMatches = line.matchAll(/\(([\d,]+)\)/g);
    for (const match of switchMatches) {
        switches.push(match[1].split(",").map(Number));
    }

    return {
        targetLights,
        switches,
        joltage,
    };
}

function switchNumber(switchIndices: number[]): number {
    return switchIndices.reduce((res, index) => res | (1 << index), 0);
}

function pressButtons(buttonBitMap: number, switchNums: number[]) {
    let index = 0;
    let result = 0;

    while (buttonBitMap > 0) {
        if (buttonBitMap % 2 == 1) {
            result ^= switchNums[index];
        }
        index++;
        buttonBitMap >>= 1;
    }

    return result;
}

function onBits(bitMap: number) {
    let result = 0;
    while (bitMap > 0) {
        result += bitMap % 2;
        bitMap >>= 1;
    }
    return result;
}

export function part1(input: string): number | string {
    const machines = parseLines(input).map(parseMachine);

    return sum(
        machines.map((machine) => {
            const switchNums = machine.switches.map(switchNumber);
            return Array.from({ length: 1 << switchNums.length }, (_, offset) => [
                offset,
                onBits(offset),
            ])
                .sort((a, b) => a[1] - b[1])
                .find(([bitMap]) => pressButtons(bitMap, switchNums) === machine.targetLights)![1];
        })
    );
}

function solveMachine(machine: Machine): number {
    const NUM_PRESSES_KEY = -1;

    const constraints = new Map<number, Constraint>(
        machine.joltage.map((joltage, index) => [index, equalTo(joltage)])
    );

    const variables = new Map<number, Coefficients<number>>(
        machine.switches.map((switchIndices, index) => {
            const coeffMap = new Map<number, number>(
                switchIndices.map((switchIndex) => [switchIndex, 1])
            );
            coeffMap.set(NUM_PRESSES_KEY, 1);
            return [index, coeffMap as Coefficients<number>];
        })
    );

    const model: Model<number, number> = {
        direction: "minimize",
        objective: NUM_PRESSES_KEY,
        constraints,
        variables,
        integers: true,
    };

    const solution = solve(model);
    return solution.result;
}

export function part2(input: string): number | string {
    const machines = parseLines(input).map(parseMachine);

    return sum(machines.map(solveMachine));
}

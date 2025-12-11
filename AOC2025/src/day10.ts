import { solve, equalTo } from "yalps";
import { Model, Constraint, Coefficients } from "yalps";

import { parseLines, bfsDistance, sum } from "./utils";

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

function switchNumbers(switchIndices: number[][]): number[] {
    return switchIndices.map(switchNumber);
}

function switchLights(lights: number, switchNums: number[]): number[] {
    return switchNums.map((switchNum) => lights ^ switchNum);
}

export function part1(input: string): number | string {
    const machines = parseLines(input).map(parseMachine);

    return sum(
        machines.map((machine) =>
            bfsDistance<number>(0, machine.targetLights, (lights) =>
                switchLights(lights, switchNumbers(machine.switches))
            )
        )
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

import { PriorityQueue } from "priority-queue-typescript";

import { parseLines, Coordinate3D, FindUnionString } from "./utils";

const CONNECT_PAIRS = 1000;

function parseInput(input: string): Coordinate3D[] {
    return parseLines(input)
        .map((line) => line.split(",").map(Number) as [number, number, number])
        .map(([x, y, z]) => ({ x, y, z }));
}

function euclidianDistance(a: Coordinate3D, b: Coordinate3D): number {
    return (a.x - b.x) ** 2 + (a.y - b.y) ** 2 + (a.z - b.z) ** 2;
}

export function part1(input: string, connectPairs = CONNECT_PAIRS): number | string {
    const coordinates = parseInput(input);
    const findUnion = new FindUnionString(coordinates);
    const pariPriorityQueue = new PriorityQueue<[Coordinate3D, Coordinate3D]>(
        1,
        (a: [Coordinate3D, Coordinate3D], b: [Coordinate3D, Coordinate3D]) =>
            euclidianDistance(a[0], a[1]) - euclidianDistance(b[0], b[1])
    );

    for (let i = 0; i < coordinates.length; i++) {
        for (let j = i + 1; j < coordinates.length; j++) {
            pariPriorityQueue.add([coordinates[i], coordinates[j]]);
        }
    }

    for (let i = 0; i < connectPairs; i++) {
        const [a, b] = pariPriorityQueue.poll()!;
        findUnion.union(a, b);
    }

    const sizesQueue = new PriorityQueue<number>(1, (a: number, b: number) => b - a);
    for (const size of findUnion.getSizes()) {
        sizesQueue.add(size);
    }

    return sizesQueue.poll()! * sizesQueue.poll()! * sizesQueue.poll()!;
}

export function part2(input: string): number | string {
    const coordinates = parseInput(input);
    const findUnion = new FindUnionString(coordinates);
    const pariPriorityQueue = new PriorityQueue<[Coordinate3D, Coordinate3D]>(
        1,
        (a: [Coordinate3D, Coordinate3D], b: [Coordinate3D, Coordinate3D]) =>
            euclidianDistance(a[0], a[1]) - euclidianDistance(b[0], b[1])
    );

    for (let i = 0; i < coordinates.length; i++) {
        for (let j = i + 1; j < coordinates.length; j++) {
            pariPriorityQueue.add([coordinates[i], coordinates[j]]);
        }
    }

    let lastTwo1: Coordinate3D | null = null;
    let lastTwo2: Coordinate3D | null = null;

    while (findUnion.getNumConnectedComponents() > 1) {
        const [a, b] = pariPriorityQueue.poll()!;
        findUnion.union(a, b);
        lastTwo1 = a;
        lastTwo2 = b;
    }

    return lastTwo1!.x * lastTwo2!.x;
}

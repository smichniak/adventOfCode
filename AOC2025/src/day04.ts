import { PriorityQueue } from "priority-queue-typescript";

import { parseLines } from "./utils";

function rollNeighbors(grid: string[][], x: number, y: number): number {
    const width = grid[0].length;
    const height = grid.length;
    let neighbors = 0;

    for (let dx = -1; dx <= 1; dx++) {
        for (let dy = -1; dy <= 1; dy++) {
            if (dx === 0 && dy === 0) continue;
            const nx = x + dx;
            const ny = y + dy;
            if (0 <= nx && nx < width && 0 <= ny && ny < height && grid[ny][nx] === "@") {
                neighbors++;
            }
        }
    }
    return neighbors;
}

export function part1(input: string): number | string {
    const grid = parseLines(input).map((line) => line.split(""));
    const width = grid[0].length;
    const height = grid.length;
    let accessible = 0;

    for (let x = 0; x < width; x++) {
        for (let y = 0; y < height; y++) {
            if (grid[y][x] === "@" && rollNeighbors(grid, x, y) < 4) {
                accessible++;
            }
        }
    }

    return accessible;
}

export function part2(input: string): number | string {
    const grid = parseLines(input).map((line) => line.split(""));
    const width = grid[0].length;
    const height = grid.length;
    const removed = new Set<string>();

    type Roll = { x: number; y: number; neighbors: number };
    const rollQueue = new PriorityQueue<Roll>(1, (a: Roll, b: Roll) => a.neighbors - b.neighbors);
    for (let x = 0; x < width; x++) {
        for (let y = 0; y < height; y++) {
            const neighbors = rollNeighbors(grid, x, y);
            if (grid[y][x] === "@" && neighbors < 4) {
                rollQueue.add({ x, y, neighbors });
            }
        }
    }

    while (rollQueue.size() > 0 && rollQueue.peek()!.neighbors < 4) {
        const { x, y } = rollQueue.poll()!;

        removed.add(JSON.stringify([x, y]));
        grid[y][x] = ".";
        for (let dx = -1; dx <= 1; dx++) {
            for (let dy = -1; dy <= 1; dy++) {
                if (dx === 0 && dy === 0) continue;
                const nx = x + dx;
                const ny = y + dy;
                if (
                    0 <= nx &&
                    nx < width &&
                    0 <= ny &&
                    ny < height &&
                    grid[ny][nx] === "@" &&
                    !removed.has(JSON.stringify([nx, ny]))
                ) {
                    rollQueue.add({ x: nx, y: ny, neighbors: rollNeighbors(grid, nx, ny) });
                }
            }
        }
    }

    return removed.size;
}

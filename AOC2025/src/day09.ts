import { parseLines, Coordinate2D } from "./utils";

type Rectangle = {
    corner1: Coordinate2D;
    corner2: Coordinate2D;
};

function parseInput(input: string): Coordinate2D[] {
    return parseLines(input)
        .map((line) => line.split(",").map(Number) as [number, number])
        .map(([x, y]) => ({ x, y }));
}

function rectangleArea(corner1: Coordinate2D, corner2: Coordinate2D): number {
    return (Math.abs(corner1.x - corner2.x) + 1) * (Math.abs(corner1.y - corner2.y) + 1);
}

function maxAreaBetweenCoords(coords: Coordinate2D[]): number {
    return coords.reduce(
        (max, c1) => Math.max(max, ...coords.map((c2) => rectangleArea(c1, c2))),
        0
    );
}

export function part1(input: string): number | string {
    const coords = parseInput(input);
    return maxAreaBetweenCoords(coords);
}

function rectangleInterior(rectangle: Rectangle): Rectangle {
    const { corner1, corner2 } = rectangle;
    const minX = Math.min(corner1.x, corner2.x);
    const maxX = Math.max(corner1.x, corner2.x);
    const minY = Math.min(corner1.y, corner2.y);
    const maxY = Math.max(corner1.y, corner2.y);

    if (maxX - minX <= 1 || maxY - minY <= 1) {
        return { corner1, corner2 };
    }

    const interior1: Coordinate2D = { x: minX + 1, y: minY + 1 };
    const interior2: Coordinate2D = { x: maxX - 1, y: maxY - 1 };

    return { corner1: interior1, corner2: interior2 };
}

function lineIntersectsRectangle(
    line: [Coordinate2D, Coordinate2D],
    rectangle: Rectangle
): boolean {
    const [l1, l2] = line;
    const { corner1, corner2 } = rectangle;

    const lineMinX = Math.min(l1.x, l2.x);
    const lineMaxX = Math.max(l1.x, l2.x);
    const lineMinY = Math.min(l1.y, l2.y);
    const lineMaxY = Math.max(l1.y, l2.y);

    const rectMinX = Math.min(corner1.x, corner2.x);
    const rectMaxX = Math.max(corner1.x, corner2.x);
    const rectMinY = Math.min(corner1.y, corner2.y);
    const rectMaxY = Math.max(corner1.y, corner2.y);

    return (
        lineMinX <= rectMaxX && lineMaxX >= rectMinX && lineMinY <= rectMaxY && lineMaxY >= rectMinY
    );
}

export function part2(input: string): number | string {
    const coords = parseInput(input);
    const edges = coords.map(
        (p, i) => [p, coords[(i + 1) % coords.length]] as [Coordinate2D, Coordinate2D]
    );

    let maxArea = 0;

    for (let i = 0; i < coords.length; i++) {
        for (let j = i; j < coords.length; j++) {
            const area = rectangleArea(coords[i], coords[j]);

            if (area > maxArea) {
                const interior = rectangleInterior({ corner1: coords[i], corner2: coords[j] });
                if (!edges.some((edge) => lineIntersectsRectangle(edge, interior))) {
                    maxArea = area;
                }
            }
        }
    }

    return maxArea;
}

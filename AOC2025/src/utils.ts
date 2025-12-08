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

export class StringSet<T> {
    private map: Map<string, T>;
    private keyFn: (item: T) => string;

    constructor(items?: Iterable<T>, keyFn: (item: T) => string = JSON.stringify) {
        this.map = new Map();
        this.keyFn = keyFn;
        if (items) {
            for (const item of items) {
                this.add(item);
            }
        }
    }

    add(item: T): this {
        const key = this.keyFn(item);
        this.map.set(key, item);
        return this;
    }

    has(item: T): boolean {
        return this.map.has(this.keyFn(item));
    }

    delete(item: T): boolean {
        return this.map.delete(this.keyFn(item));
    }

    clear(): void {
        this.map.clear();
    }

    get size(): number {
        return this.map.size;
    }

    *[Symbol.iterator](): IterableIterator<T> {
        yield* this.map.values();
    }

    values(): IterableIterator<T> {
        return this.map.values();
    }
}

export class StringMap<K, V> {
    private map: Map<string, [K, V]>;
    private keyFn: (key: K) => string;

    constructor(entries?: Iterable<[K, V]>, keyFn: (key: K) => string = JSON.stringify) {
        this.map = new Map();
        this.keyFn = keyFn;
        if (entries) {
            for (const [key, value] of entries) {
                this.set(key, value);
            }
        }
    }

    set(key: K, value: V): this {
        const k = this.keyFn(key);
        this.map.set(k, [key, value]);
        return this;
    }

    get(key: K): V | undefined {
        const entry = this.map.get(this.keyFn(key));
        return entry ? entry[1] : undefined;
    }

    has(key: K): boolean {
        return this.map.has(this.keyFn(key));
    }

    delete(key: K): boolean {
        return this.map.delete(this.keyFn(key));
    }

    clear(): void {
        this.map.clear();
    }

    get size(): number {
        return this.map.size;
    }

    *[Symbol.iterator](): IterableIterator<[K, V]> {
        yield* this.map.values();
    }

    *keys(): IterableIterator<K> {
        for (const [k] of this.map.values()) {
            yield k;
        }
    }

    *values(): IterableIterator<V> {
        for (const [, v] of this.map.values()) {
            yield v;
        }
    }

    entries(): IterableIterator<[K, V]> {
        return this.map.values();
    }
}

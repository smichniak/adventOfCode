import { readInput } from "./utils";
import { performance } from "perf_hooks";
import * as fs from "fs";

async function runDay(dayNum: number, partArg: number | null) {
    const dayStr = dayNum.toString().padStart(2, "0");
    const modulePath = `./day${dayStr}`;

    try {
        const dayModule = await import(modulePath);
        const input = readInput(dayNum);

        if (partArg === 1 || partArg === null) {
            const start = performance.now();
            const result = dayModule.part1(input);
            const end = performance.now();
            console.log(`Day ${dayNum} Part 1: ${result} (${(end - start).toFixed(2)}ms)`);
        }

        if (partArg === 2 || partArg === null) {
            const start = performance.now();
            const result = dayModule.part2(input);
            const end = performance.now();
            console.log(`Day ${dayNum} Part 2: ${result} (${(end - start).toFixed(2)}ms)`);
        }
    } catch (error) {
        console.error(`Could not load or run solution for day ${dayNum}`, error);
    }
}

async function run() {
    const args = process.argv.slice(2);

    if (args.length < 1) {
        console.log("No day specified, running all days...");
        const srcDir = __dirname;
        const files = fs.readdirSync(srcDir);
        const dayFiles = files.filter((f) => /^day\d+\.ts$/.test(f)).sort();

        for (const file of dayFiles) {
            const dayNum = parseInt(file.match(/\d+/)![0], 10);
            await runDay(dayNum, null);
            console.log("---");
        }
        return;
    }

    const dayNum = parseInt(args[0], 10);
    if (isNaN(dayNum)) {
        console.error("Invalid day number");
        process.exit(1);
    }

    const partArg = args[1] ? parseInt(args[1], 10) : null;
    await runDay(dayNum, partArg);
}

run();

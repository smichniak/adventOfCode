import { readInput } from './utils';
import { performance } from 'perf_hooks';

async function run() {
    const args = process.argv.slice(2);
    if (args.length < 1) {
        console.error("Usage: npm start <day> [part]");
        process.exit(1);
    }

    const dayNum = parseInt(args[0], 10);
    if (isNaN(dayNum)) {
        console.error("Invalid day number");
        process.exit(1);
    }

    const dayStr = dayNum.toString().padStart(2, '0');
    const modulePath = `./day${dayStr}`;

    try {
        const dayModule = await import(modulePath);
        const input = readInput(dayNum);

        const partArg = args[1] ? parseInt(args[1], 10) : null;

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
        console.error(`Could not load or run solution for day ${dayNum}`);
    }
}

run();

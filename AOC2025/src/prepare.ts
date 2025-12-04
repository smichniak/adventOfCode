import * as fs from "fs";
import * as path from "path";

const args = process.argv.slice(2);
let day = args[0];

if (!day) {
    const today = new Date();
    // Default to today if it's December, otherwise require argument
    if (today.getMonth() === 11) {
        // Month is 0-indexed (0-11)
        day = today.getDate().toString();
    } else {
        console.error("Please provide a day number (e.g., npm run gen 1)");
        process.exit(1);
    }
}

const dayNum = parseInt(day, 10);
if (isNaN(dayNum)) {
    console.error("Invalid day number.");
    process.exit(1);
}

const dayStr = dayNum.toString().padStart(2, "0");
const projectRoot = path.join(__dirname, "..");

// Paths
const inputDir = path.join(projectRoot, "input");
const srcDir = path.join(projectRoot, "src");
const testDir = path.join(projectRoot, "test");

const inputFile = path.join(inputDir, `day${dayStr}.in`);
const sourceFile = path.join(srcDir, `day${dayStr}.ts`);
const testFile = path.join(testDir, `day${dayStr}.test.ts`);

// Ensure directories exist
[inputDir, srcDir, testDir].forEach((dir) => {
    if (!fs.existsSync(dir)) {
        fs.mkdirSync(dir, { recursive: true });
    }
});

// Create input file
if (!fs.existsSync(inputFile)) {
    fs.writeFileSync(inputFile, "");
    console.log(`Created input file: ${inputFile}`);
} else {
    console.log(`Input file already exists: ${inputFile}`);
}

// Create source file
if (!fs.existsSync(sourceFile)) {
    const template = `import { parseLines } from "./utils";

export function part1(input: string): number | string {
    return "TODO";
}

export function part2(input: string): number | string {
    return "TODO";
}
`;
    fs.writeFileSync(sourceFile, template);
    console.log(`Created source file: ${sourceFile}`);
} else {
    console.log(`Source file already exists: ${sourceFile}`);
}

// Create test file
if (!fs.existsSync(testFile)) {
    const testTemplate = `import { part1, part2 } from '../src/day${dayStr}';

describe('Day ${dayNum}', () => {
    const exampleInput = \`
\`;

    test('Part 1 Example', () => {
        expect(part1(exampleInput.trim())).toBe("TODO");
    });

    test('Part 2 Example', () => {
        expect(part2(exampleInput.trim())).toBe("TODO");
    });
});
`;
    fs.writeFileSync(testFile, testTemplate);
    console.log(`Created test file: ${testFile}`);
} else {
    console.log(`Test file already exists: ${testFile}`);
}

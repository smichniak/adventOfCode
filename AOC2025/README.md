# Advent of Code 2025 - TypeScript

Solutions for Advent of Code 2025 in TypeScript.

## Setup

1. Install dependencies:
   ```bash
   npm install
   ```

## Running Solutions

Run a specific day's solution:

```bash
npm start <day> [part]
```

Examples:

```bash
# Run both parts for Day 1
npm start 1

# Run only Part 2 for Day 5
npm start 5 2
```

## Project Structure

- `src/`: Source code for solutions
  - `dayXX.ts`: Solution for Day XX
  - `index.ts`: Runner script
  - `utils.ts`: Utility functions
- `input/`: Input files
  - `dayXX.in`: Input for Day XX

## Creating a New Day

To scaffold files for a new day (creates `src/dayXX.ts`, `input/dayXX.in`, and `test/dayXX.test.ts`):

```bash
npm run gen <day>
```

Example:

```bash
npm run gen 2
```

If no day is provided and it is December, it defaults to the current day.

## Testing

Tests are built using Jest. To run tests:

```bash
npm test
```

To run tests for a specific day:

```bash
npm test day01
```

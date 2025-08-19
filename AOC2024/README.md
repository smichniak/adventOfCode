# Advent of Code 2024 - Elixir Solutions

This repository contains my solutions for [Advent of Code 2024](https://adventofcode.com/2024) implemented in Elixir.

## Project Structure

```
AOC2024/
├── lib/
│   ├── aoc.ex              # Main AOC helper module
│   ├── day01.ex            # Day 1 solution
│   ├── day02.ex            # Day 2 solution
│   ├── ...                 # Additional day solutions
│   ├── utils.ex            # Utility functions
│   └── mix/
│       └── tasks/          # Custom Mix tasks
├── input/                  # Input files for each day
├── test/                   # Test files for each day
└── mix.exs                 # Project configuration
```

## Setup

1. Install Elixir (version 1.17 or later)
2. Clone this repository
3. Install dependencies:
   ```bash
   mix deps.get
   ```

## Available Mix Tasks

### `mix prepare` - Set up boilerplate for a new day

Creates the necessary files for solving a new Advent of Code day.

**Usage:**
```bash
# Prepare files for a specific day
mix prepare 1

# Prepare files for today's date (if running during December)
mix prepare
```

**What it creates:**
- `input/dayXX.in` - Empty input file
- `lib/dayXX.ex` - Solution module template with parser and solution functions
- `test/dayXX_test.exs` - Test file template

**Generated module template includes:**
- `parser1/1` and `parser2/1` functions for parsing input
- `solution1/1` and `solution2/1` functions for solving parts 1 and 2
- Proper type specifications

### `mix solve` - Run solutions

Executes your Advent of Code solutions with various options.

**Usage:**
```bash
# Solve both parts of a specific day
mix solve 1

# Solve a specific part of a specific day
mix solve 1 1    # Day 1, Part 1
mix solve 1 2    # Day 1, Part 2

# Solve today's puzzle (if running during December)
mix solve today

# Solve all implemented days
mix solve
```

**Output format:**
```
Day 01, part 1: 142
Day 01, part 2: 281
```

## Development Workflow

1. **Prepare a new day:**
   ```bash
   mix prepare 5
   ```

2. **Add your input data** to `input/day05.in`

3. **Implement your solution** in `lib/day05.ex`:
   - Update the `@type input_type` with the appropriate data type
   - Implement `parser1/1` to parse the input text
   - Implement `solution1/1` to solve part 1
   - Implement `parser2/1` and `solution2/1` for part 2

4. **Add test cases** in `test/day05_test.exs` using the provided examples

5. **Run your solution:**
   ```bash
   mix solve 5
   ```

6. **Run tests:**
   ```bash
   mix test test/day05_test.exs
   ```

## Running Tests

```bash
# Run all tests
mix test

# Run tests for a specific day
mix test test/day01_test.exs
```

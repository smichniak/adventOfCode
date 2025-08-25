# Advent of Code 2023 - Rust Solutions

This repository contains my solutions for [Advent of Code 2023](https://adventofcode.com/2023) implemented in Rust.

## Project Structure

```
AOC2023/
├── src/
│   ├── main.rs             # Main entry point
│   ├── lib.rs              # Library module declarations
│   ├── argparse.rs         # Command-line argument parsing
│   ├── day.rs              # Day trait and Runner implementation
│   ├── solutions.rs        # Solution registry
│   ├── utils.rs            # Utility functions
│   ├── day_template.tmp    # Template for new day solutions
│   ├── day01.rs            # Day 1 solution
│   ├── day02.rs            # Day 2 solution
│   ├── ...                 # Additional day solutions
│   └── day25.rs            # Day 25 solution
├── input/                  # Input files for each day
├── Cargo.toml              # Rust project configuration
└── README.md               # This file
```

## Setup

1. Install Rust (version 2021 edition or later)
2. Clone this repository
3. Build the project:
   ```bash
   cargo build --release
   ```

## Available Commands

### `cargo run -- prepare` - Set up boilerplate for a new day

Creates the necessary files for solving a new Advent of Code day.

**Usage:**
```bash
# Prepare files for a specific day
cargo run -- prepare --day 1

# Prepare files for today's date (if running during December)
cargo run -- prepare

# Overwrite existing files
cargo run -- prepare --day 1 --overwrite
```

**What it creates:**
- `input/dayXX.in` - Empty input file
- `src/dayXX.rs` - Solution module template implementing the Day trait

**Generated module template includes:**
- `Day` trait implementation with proper type specifications
- `parse_input()` and `parse_input2()` methods for parsing input
- `solve1()` and `solve2()` methods for solving parts 1 and 2
- Test module template with example test cases

### `cargo run -- solve` - Run solutions

Executes your Advent of Code solutions with various options.

**Usage:**
```bash
# Solve both parts of a specific day
cargo run -- solve --day 1

# Solve a specific part of a specific day
cargo run -- solve --day 1 --part 1    # Day 1, Part 1
cargo run -- solve --day 1 --part 2    # Day 1, Part 2

# Solve today's puzzle (if running during December)
cargo run -- solve

# Solve with timing information
cargo run -- solve --day 1 --time
```

### `cargo run -- all` - Run all implemented solutions

Executes all available day solutions.

**Usage:**
```bash
# Run all solutions
cargo run -- all

# Run all solutions with timing information
cargo run -- all --time
```

**Output format:**
```
Day: 01    Part: 1    Solution: 142                    
Day: 01    Part: 2    Solution: 281                    
```

**With timing enabled:**
```
Day: 01    Part: 1    Solution: 142                     Time: 1234     mus
Day: 01    Part: 2    Solution: 281                     Time: 5678     mus
```

## Development Workflow

1. **Prepare a new day:**
   ```bash
   cargo run -- prepare --day 5
   ```

2. **Add your input data** to `input/day05.in`

3. **Implement your solution** in `src/day05.rs`:
   - Update the `type Input` and `type Result` associated types
   - Implement `parse_input()` to parse the input text
   - Implement `solve1()` to solve part 1
   - Optionally override `parse_input2()` if part 2 needs different parsing
   - Implement `solve2()` to solve part 2

4. **Add test cases** using the provided examples from the problem description

5. **Run your solution:**
   ```bash
   cargo run -- solve --day 5
   ```

6. **Run tests:**
   ```bash
   cargo test day05
   ```

# Advent of Code 2022 - Haskell Solutions

This repository contains my solutions for [Advent of Code 2022](https://adventofcode.com/2022) implemented in Haskell.

## Project Structure

```
AOC2022/
├── src/
│   ├── Main.hs             # Command-line interface
│   ├── Solutions.hs        # Centralized solution registry
│   ├── Utils.hs            # Utility functions and types
│   ├── Day1.hs             # Day 1 solution
│   ├── Day2.hs             # Day 2 solution
│   ├── ...                 # Additional day solutions (Day3.hs - Day25.hs)
├── input/                  # Input files for each day
├── adventOfCode.cabal      # Cabal project configuration
└── CHANGELOG.md
```

## Setup

1. Install GHC and Cabal (GHC 9.2.4 or later recommended)
2. Clone this repository
3. Build the project:
   ```bash
   cabal build
   ```

## Usage

The project provides a command-line interface for running solutions:

```bash
# Display help message
cabal run adventOfCode -- --help

# Run both parts of a specific day
cabal run adventOfCode -- 1

# Run a specific part of a specific day
cabal run adventOfCode -- 1 1    # Day 1, Part 1
cabal run adventOfCode -- 1 2    # Day 1, Part 2

# Run both parts of the last implemented day
cabal run adventOfCode -- last

# Run all implemented days
cabal run adventOfCode -- all
```

## Development

Each day's solution is implemented as a separate module (`DayX.hs`) with:
- An `InputType` type alias defining the parsed input structure
- An `inputParser` function to parse the raw input text
- `solution1` and `solution2` functions for solving parts 1 and 2
- `main1` and `main2` functions as entry points

The `Utils.hs` module provides common types and helper functions used across multiple days.

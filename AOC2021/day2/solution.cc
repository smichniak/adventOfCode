#include <iostream>
#include <vector>
#include <sstream>
#include "../includes/utils.h"

using std::istringstream;

int const DAY = 2;
string const FORWARD = "forward";
string const UP = "up";

class Day2 : public Day {
public:
    explicit Day2(int day_number) : Day(day_number) {
        for (string const& line : input_lines) {
            string instruction;
            int instruction_modifier;

            istringstream instruction_stream(line);
            instruction_stream >> instruction >> instruction_modifier;

            instructions.push_back(instruction);
            instruction_modifiers.push_back(instruction_modifier);
        }
    }

    int part1() override;

    int part2() override;

private:
    vector<string> instructions;
    vector<int> instruction_modifiers;
};

int Day2::part1() {
    int depth = 0, position = 0;
    for (int i = 0; i < instructions.size(); ++i) {
        string instruction = instructions[i];
        int instruction_modifier = instruction_modifiers[i];

        if (instruction == FORWARD) {
            position += instruction_modifier;
        } else {
            // "up" instruction decreases depth, we multiply by -1
            // Else the instruction is "down"
            depth += instruction_modifier * (1 - 2 * (instruction == UP));
        }

    }

    return depth * position;
}

int Day2::part2() {
    int depth = 0, position = 0, aim = 0;
    for (int i = 0; i < instructions.size(); ++i) {
        string instruction = instructions[i];
        int instruction_modifier = instruction_modifiers[i];

        if (instruction == FORWARD) {
            position += instruction_modifier;
            depth += instruction_modifier * aim;
        } else {
            // "up" instruction decreases aim, we multiply by -1
            // Else the instruction is "down"
            aim += instruction_modifier * (1 - 2 * (instruction == UP));
        }

    }

    return depth * position;
}

int main() {
    Day2 day = Day2(DAY);

    cout << day.part1() << '\n';
    cout << day.part2() << '\n';

    return 0;
}

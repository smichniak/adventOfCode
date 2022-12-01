#include <iostream>
#include <regex>
#include <unordered_map>
#include <map>
#include <set>
#include <unordered_set>
#include <array>
#include "../includes/utils.h"

using std::regex, std::smatch, std::stoi, std::array, std::unordered_map, std::map, std::set, std::pair;
using std::make_pair, std::max, std::unordered_set, std::erase_if, std::prev, std::next;

// We will assume w is only used for input, only x, y and z change

using alu_state = array<int64_t, 3>; // Keeps x, y and z
using state_map = unordered_map<alu_state, set<int64_t>>;
using num_for_state = pair<alu_state, int64_t>;
using state_to_state_map = map<num_for_state, alu_state>;
using digit_array = array<bool, 9>;
using digit_2d_array = array<digit_array, 9>;

int const DAY = 24;

enum OperationType {
    inp, add, mul, divv, mod, eql
};

enum Variable {
    w = -1, x = 0, y = 1, z = 2
};

namespace std {
    template<>
    struct hash<alu_state> {
        inline size_t operator()(const alu_state& state) const {
            std::hash<int64_t> int_hasher;
            return int_hasher(state[0]) ^ int_hasher(state[1]) ^ int_hasher(state[2]);
        }
    };

}

unordered_map<string, OperationType> operations{{"inp", inp},
                                                {"add", add},
                                                {"mul", mul},
                                                {"div", divv},
                                                {"mod", mod},
                                                {"eql", eql}};

class ALU {
public:
    ALU() : w{}, state{} {}

    ALU(int64_t w, alu_state state) : w(w), state(state) {}

    void execute_instruction(OperationType operation, Variable a, Variable b) {
        if (b == w) {
            execute_instruction(operation, a, w);
        } else {
            execute_instruction(operation, a, state[b]);
        }
    }

    void execute_instruction(OperationType operation, Variable a, int64_t num) {
        if (operation == add) {
            state[a] = state[a] + num;
        } else if (operation == mul) {
            state[a] = state[a] * num;
        } else if (operation == divv) {
            state[a] = state[a] / num;
        } else if (operation == mod) {
            state[a] = state[a] % num;
        } else if (operation == eql) {
            state[a] = state[a] == num;
        }
    }

    alu_state get_state() const {
        return state;
    }

private:
    int64_t w;
    alu_state state;
};

struct Instruction {
    OperationType operation;
    Variable a;
    int64_t b;
    bool is_b_variable;
};

class InstructionBlock {
public:
    explicit InstructionBlock(vector<string> const& instructions_str) {
        regex instruction_regex = regex(R"(^([a-z]{3}) ([wxyz]) (\S+)$)");
        smatch instruction_match;

        for (string const& instruction_str : instructions_str) {
            Instruction instruction{};
            regex_match(instruction_str, instruction_match, instruction_regex);

            instruction.operation = operations[instruction_match[1]];
            instruction.a = static_cast<Variable>(instruction_match[2].str()[0] - 'x');
            string b_str = instruction_match[3].str();
            if (isalpha(b_str[0])) {
                instruction.b = b_str[0] - 'x';
                instruction.is_b_variable = true;
            } else {
                instruction.b = stoi(b_str);
                instruction.is_b_variable = false;
            }
            instructions.emplace_back(instruction);
        }
    }


    state_map get_possible_states(alu_state starting_state) const {
        state_map result;
        for (int64_t w = 1; w <= 9; ++w) {
            ALU alu(w, starting_state);
            for (Instruction const& instruction : instructions) {
                execute_instruction_on_alu(alu, instruction);
            }
            result[alu.get_state()].insert(w);
        }
        return result;
    }

private:
    vector<Instruction> instructions;

    static void execute_instruction_on_alu(ALU& alu, Instruction const& instruction) {
        if (instruction.is_b_variable) {
            alu.execute_instruction(instruction.operation, instruction.a, static_cast<Variable>(instruction.b));
        } else {
            alu.execute_instruction(instruction.operation, instruction.a, instruction.b);
        }
    }

};

class Day24 : public Day<int64_t> {
public:
    explicit Day24(int day_number) : Day(day_number) {
        vector<string> instructions;
        for (string const& line : input_lines) {
            if (line[0] == 'i' && !instructions.empty()) { // inp w
                blocks.emplace_back(instructions);
                instructions.clear();
            } else if (line[0] != 'i') {
                instructions.emplace_back(line);
            }
        }
        blocks.emplace_back(instructions);
    }

    int64_t part1() override;

    int64_t part2() override;


private:
    vector<InstructionBlock> blocks;

//    using state_to_state_map = unordered_map<num_for_state, alu_state>;
//    using num_for_state = pair<alu_state, int64_t>;

    static void remove_non_valid(vector<state_to_state_map>& state_map) {
        erase_if(state_map.back(), [](auto const& item) {
            auto const&[key, new_state] = item;
            return new_state[z] != 0;
        });

        for (int i = state_map.size() - 2; i >= 0; --i) {
            erase_if(state_map[i], [&state_map, i](auto const& item) {
                auto const&[key, new_state] = item;
                for (int w = 1; w <= 9; ++w) {
                    if (state_map[i + 1].contains(make_pair(new_state, w))) {
                        return false;
                    }
                }
                return true;
            });
        }
    }

    static vector<digit_2d_array> get_digit_paths(vector<state_to_state_map> const& state_maps) {
        vector<digit_2d_array> result;
        for (auto iter = state_maps.begin(); iter != prev(state_maps.end()); ++iter) {
            result.emplace_back();
            for (auto[old_state_pair, new_state] : *iter) {
                auto[old_state, num] = old_state_pair;
                for (int digit = 1; digit <= 9; ++digit) {
                    if (next(iter)->contains(make_pair(new_state, digit))) {
                        result.back()[num-1][digit-1] = true;
                    }
                }

            }

        }
        return result;
    }

    static int64_t dfs(vector<digit_2d_array> const& paths, int64_t previous_digit, int i, int64_t acc) {

        digit_2d_array current = paths[i];
        if (i == paths.size() - 1) {
            for (int digit = 9; digit >= 1; --digit) {
                if (current[previous_digit-1][digit-1]) {
                    return 10 * acc + digit;
                }
            }
            return 0;
        }

        for (int digit = 9; digit >= 1; --digit) {
            if (current[previous_digit-1][digit-1]) {
                int64_t next_digit = dfs(paths, digit, i + 1, 10 * acc + digit);
                if (next_digit > 0) {
                    return next_digit;
                }
            }
        }

    }
};


int64_t Day24::part1() {
    vector<state_to_state_map> best_num_for_state;
    set<alu_state> current_possible_states;
    set<alu_state> new_possible_states;

    alu_state starting_state{0, 0, 0};

    current_possible_states.insert(starting_state);

    int i = 0;
    for (InstructionBlock const& block : blocks) {
//        if (i == 4) {
//            break;
//        }


        state_to_state_map new_map;
        best_num_for_state.emplace_back();

        for (alu_state state: current_possible_states) {
            state_map possible_new_states = block.get_possible_states(state);
            for (auto[new_state, num_set] : possible_new_states) {
                int64_t best_num = *num_set.rbegin();

                if (i < blocks.size() - 1 || new_state[z] == 0) {
                    best_num_for_state.back().emplace(make_pair(state, best_num), new_state);
                }
                if (i < blocks.size() - 1) {
                    new_possible_states.insert(new_state);
                }

            }
        }
        current_possible_states = new_possible_states;
        new_possible_states.clear();
        ++i;
    }

    remove_non_valid(best_num_for_state);
    vector<digit_2d_array> digit_paths = get_digit_paths(best_num_for_state);

    for (int digit = 9; digit >= 1; --digit) {
        int64_t max_num = dfs(digit_paths, digit, 0, digit);
        if (max_num > 0) {
            return max_num;
        }
    }

    return 0;
}

int64_t Day24::part2() {

}


int main() {
    Day24 day = Day24(DAY);

    cout << day.part1() << '\n';
    cout << day.part2() << '\n';
    return 0;
}

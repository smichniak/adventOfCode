#include <iostream>
#include <vector>
#include <unordered_map>
#include <sstream>
#include "../includes/utils.h"

using std::vector, std::unordered_map, std::stoi, std::stringstream;

int const DAY = 4;
int const BOARD_SIZE = 5;
int const FIRST_GUARD = BOARD_SIZE * BOARD_SIZE;
int const SECOND_GUARD = BOARD_SIZE * BOARD_SIZE + 1;
int const NO_NODE = -3;

class Find_union {
public:
    bool added(int node) {
        return roots[node] != NO_NODE;
    }

    int find(int node) {
        int next_node = roots[node];
        if (next_node == node) {
            return next_node;
        }

        int root_node = find(next_node);
        roots[node] = root_node;
        return root_node;
    }

    void add(int node) {
        roots[node] = node;
    }

    void union_nodes(int node1, int node2) {
        if (added(node1) && added(node2)) {
            int root1 = find(node1);
            int root2 = find(node2);
            if (root1 != root2) {
                if (weights[root1] > weights[root2]) {
                    roots[root2] = root1;
                    weights[root1]++;
                } else {
                    roots[root1] = root2;
                    weights[root2]++;
                }
            }
        }
    }

private:
    vector<int> roots = vector<int>(BOARD_SIZE * BOARD_SIZE + 2, NO_NODE); // +2 for guards
    vector<int> weights = vector<int>(BOARD_SIZE * BOARD_SIZE + 2, 1);
};

class Board {
public:
    explicit Board(vector<int> const& numbers) {
        for (int i = 0; i < numbers.size(); ++i) {
            number_coordinates[numbers[i]] = i;
        }
        horizontal.add(FIRST_GUARD); // Left guard
        horizontal.add(SECOND_GUARD); // Right guard
        vertical.add(FIRST_GUARD); // Top guard
        vertical.add(SECOND_GUARD); // Bottom guard
    }

    bool winning() {
        return (horizontal.find(FIRST_GUARD) == horizontal.find(SECOND_GUARD) ||
                vertical.find(FIRST_GUARD) == vertical.find(SECOND_GUARD)) && !deleted;
    }

    void delete_board() {
        deleted = true;
    }

    bool is_deleted() const {
        return deleted;
    }

    void mark(int number) {
        auto it = number_coordinates.find(number);
        if (it != number_coordinates.end()) {
            int id = it->second;
            horizontal.add(id);
            vertical.add(id);

            if (id % BOARD_SIZE == 0) {
                horizontal.union_nodes(FIRST_GUARD, id);
                horizontal.union_nodes(id, id + 1);
            } else if (id % BOARD_SIZE == BOARD_SIZE - 1) {
                horizontal.union_nodes(SECOND_GUARD, id);
                horizontal.union_nodes(id, id - 1);
            } else {
                horizontal.union_nodes(id, id + 1);
                horizontal.union_nodes(id, id - 1);
            }

            if (id < BOARD_SIZE) {
                vertical.union_nodes(FIRST_GUARD, id);
                vertical.union_nodes(id, id + BOARD_SIZE);
            } else if (id >= BOARD_SIZE * (BOARD_SIZE - 1)) {
                vertical.union_nodes(SECOND_GUARD, id);
                vertical.union_nodes(id, id - BOARD_SIZE);
            } else {
                vertical.union_nodes(id, id + BOARD_SIZE);
                vertical.union_nodes(id, id - BOARD_SIZE);
            }
        }
    }

    int score(int last_number) {
        int sum_unmarked = 0;
        for (auto& it: number_coordinates) {
            if (!horizontal.added(it.second)) {
                sum_unmarked += it.first;
            }
        }
        return sum_unmarked * last_number;
    }


private:
    unordered_map<int, int> number_coordinates; // Map from number to the id of its field
    Find_union horizontal;
    Find_union vertical;
    bool deleted = false;
};

class Day4 : public Day {
public:
    explicit Day4(int day_number) : Day(day_number) {
        stringstream string_stream(input_lines[0]);
        string num_string;

        while (std::getline(string_stream, num_string, ',')) {
            numbers.push_back(stoi(num_string));
        }

        vector<int> board_nums;
        for (int i = 1; i < input_lines.size(); ++i) {
            string_stream = stringstream(input_lines[i]);
            while (std::getline(string_stream, num_string, ' ')) {
                if (!num_string.empty()) {
                    board_nums.push_back(stoi(num_string));
                }
            }

            if (board_nums.size() == BOARD_SIZE * BOARD_SIZE) {
                boards.emplace_back(Board(board_nums));
                board_nums.clear();
            }

        }

    }

    int part1() override;

    int part2() override;

private:
    vector<int> numbers;
    vector<Board> boards;

};

int Day4::part1() {
    for (int num : numbers) {
        for (Board& board : boards) {
            board.mark(num);
            if (board.winning()) {
                return board.score(num);
            }
        }
    }

    return 0;
}

int Day4::part2() {
    int boards_left = boards.size();

    for (int num : numbers) {
        for (Board& board : boards) {
            if (!board.is_deleted()) {
                board.mark(num);
                if (board.winning()) {
                    boards_left--;
                    board.delete_board();
                    if (boards_left == 0) {
                        return board.score(num);
                    }
                }
            }
        }
    }


    return 0;
}

int main() {
    Day4 day = Day4(DAY);

    cout << day.part1() << '\n';
    cout << day.part2() << '\n';

    return 0;
}

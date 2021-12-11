#include <iostream>
#include "../includes/utils.h"

using std::pair, std::make_pair;

int const DAY = 11;
int const STEPS = 100;

class Day11 : public Day<int> {
public:
    explicit Day11(int day_number) : Day(day_number) {
        for (string const& line : input_lines) {
            grid.emplace_back();
            for (char digit : line) {
                prev(grid.end())->push_back(digit - '0');
            }
        }
        grid_size = input_lines.size();
    }

    void print_grid() {
        for (vector<int> const& line : grid) {
            for (int num : line) {
                cout << num;
            }
            cout << '\n';
        }
        cout << '\n';
    }

    // Returns new flashes;
    int step() {
        int flashes = 0;
        vector<pair<int, int>> to_flash;

        for (int i = 0; i < grid_size; ++i) {
            for (int j = 0; j < grid_size; ++j) {
                grid[i][j]++;
                if (grid[i][j] == 10) {
                    to_flash.emplace_back(make_pair(i, j));
                }
            }
        }

        while (!to_flash.empty()) {
            auto[i, j] = to_flash.back();
            to_flash.pop_back();
            for (int di = -1; di <= 1; ++di) {
                if (i + di >= 0 && i + di < grid_size) {
                    for (int dj = -1; dj <= 1; ++dj) {
                        if (j + dj >= 0 && j + dj < grid_size) {
                            grid[i + di][j + dj]++;
                            if (grid[i + di][j + dj] == 10) {
                                to_flash.emplace_back(make_pair(i + di, j + dj));
                            }
                        }
                    }
                }
            }
        }

        for (int i = 0; i < grid_size; ++i) {
            for (int j = 0; j < grid_size; ++j) {
                if (grid[i][j] >= 10) {
                    flashes++;
                    grid[i][j] = 0;
                }
            }
        }
        return flashes;
    }

    int part1() override;

    int part2() override;


private:
    int grid_size;
    vector<vector<int>> grid;
};

int Day11::part1() {
    int flashes = 0;

    for (int step_num = 0; step_num < STEPS; ++step_num) {
        flashes += step();
    }

    return flashes;
}

int Day11::part2() {
    int step_num = 0;
    while (step() != grid_size * grid_size) {
        step_num++;
    }

    return step_num + 1;
}


int main() {
    Day11 day = Day11(DAY);
    cout << day.part1() << '\n';

    day = Day11(DAY); // Resetting the input grid
    cout << day.part2() << '\n';

    return 0;
}

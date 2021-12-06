#include <iostream>
#include "../includes/utils.h"

using std::stoi;

int const DAY = 1;

class Day1 : public Day {
public:
    explicit Day1(int day_number) : Day(day_number) {
        for (string const& line : input_lines) {
            depths.push_back(stoi(line));
        }
    }

    int part1() override;

    int part2() override;


private:
    vector<int> depths;
};

int Day1::part1() {
    int increased = 0, current_depth = INT_MAX;

    for (int depth : this->depths) {
        increased += depth > current_depth;
        current_depth = depth;
    }

    return increased;
}

int Day1::part2() {
    assert(depths.size() >= 3);

    int sum = depths[0] + depths[1] + depths[2];
    int current_sum, increased = 0;
    for (int i = 1; i < depths.size() - 2; ++i) {
        current_sum = depths[i] + depths[i + 1] + depths[i + 2];
        increased += current_sum > sum;
        sum = current_sum;
    }

    return increased;
}


int main() {
    Day1 day = Day1(DAY);

    cout << day.part1() << '\n';
    cout << day.part2() << '\n';

    return 0;
}

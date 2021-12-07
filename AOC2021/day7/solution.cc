#include <iostream>
#include <sstream>
#include <algorithm>
#include <numeric>
#include "../includes/utils.h"

using std::vector, std::stringstream, std::sort, std::reduce, std::min;

int const DAY = 7;

class Day7 : public Day<int> {
public:
    explicit Day7(int day_number) : Day(day_number) {
        stringstream string_stream(input_lines[0]);
        string num_string;

        while (getline(string_stream, num_string, ',')) {
            positions.push_back(stoi(num_string));
        }

        sort(positions.begin(), positions.end());
    }

    int part1() override;

    int part2() override;


private:
    vector<int> positions;

};

int Day7::part1() {
    int median = positions[positions.size() / 2];
    return reduce(positions.begin(), positions.end(), 0,
                  [median](int acc, int pos) { return acc + abs(pos - median); });
}

auto crab_metric(int target) {
    return [target](int acc, int pos) { return acc + abs(pos - target) * (abs(pos - target) + 1) / 2; };
}

int Day7::part2() {
    int mean = reduce(positions.begin(), positions.end(), 0) / positions.size();
    int mean_m = mean - 1;
    int mean_p = mean + 1;

    // Fuel is minimized by a target position = mean(positions) + eps, where |eps| < 1/2
    // We can check mean and two of its neighbours

    int res = reduce(positions.begin(), positions.end(), 0, crab_metric(mean));
    int res_m = reduce(positions.begin(), positions.end(), 0, crab_metric(mean_m));
    int res_p = reduce(positions.begin(), positions.end(), 0, crab_metric(mean_p));

    return min(min(res, res_p), res_m);
}


int main() {
    Day7 day = Day7(DAY);

    cout << day.part1() << '\n';
    cout << day.part2() << '\n';

    return 0;
}

#include <iostream>
#include <sstream>
#include <algorithm>
#include <numeric>
#include "../includes/utils.h"

using std::vector, std::stringstream, std::shift_left, std::accumulate;

int const DAY = 6;
int const MAX_COUNTER = 8;
int const FISH_CYCLE = 7;
int const ITERATIONS_PART1 = 80;
int const ITERATIONS_PART2 = 256;

class Day6 : public Day {
public:
    explicit Day6(int day_number) : Day(day_number) {
        stringstream string_stream(input_lines[0]);
        string num_string;

        while (getline(string_stream, num_string, ',')) {
            counters[stoi(num_string)]++;
        }

    }

    void iterate_days(int iterations) {
        for (int i = 0; i < iterations; ++i) {
            long long new_fish = counters[0];

            shift_left(counters.begin(), counters.end(), 1);

            counters[MAX_COUNTER] = new_fish;
            counters[FISH_CYCLE - 1] += new_fish;
        }
    }

    int part1() override;

    int part2() override { return 0; }

    long long part2_bignum();


private:
    vector<long long> counters = vector(MAX_COUNTER + 1, (long long) 0);
};

int Day6::part1() {
    iterate_days(ITERATIONS_PART1);

    return accumulate(counters.begin(), counters.end(), 0);
}

long long Day6::part2_bignum() {
    iterate_days(ITERATIONS_PART2 - ITERATIONS_PART1);

    return accumulate(counters.begin(), counters.end(), (long long) 0);
}

int main() {
    Day6 day = Day6(DAY);

    cout << day.part1() << '\n';
    cout << day.part2_bignum() << '\n';

    return 0;
}

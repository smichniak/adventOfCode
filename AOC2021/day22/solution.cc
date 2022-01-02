#include <iostream>
#include <regex>
#include <bitset>
#include <array>
#include <set>
#include <numeric>
#include "../includes/utils.h"

using std::regex, std::smatch, std::stoi, std::array, std::pair, std::bitset, std::abs, std::reduce, std::set;
using std::make_pair, std::min, std::max, std::erase_if, std::set_union;

using sextuple = array<int, 6>;

int const DAY = 22;
int const PART1_SIZE = 50;

class SplitIntervalSet {
public:
    SplitIntervalSet() = default;

    void insert(int left, int right) {
        set<pair<int, int>> to_erase;
        set<pair<int, int>> to_insert;
        for (pair<int, int> const& interval : intervals) {
            if (interval.first > right) {
                break;
            }

            int min_left = min(interval.first, left);
            int max_left = max(interval.first, left);
            int min_right = min(interval.second, right);

            if (max_left <= min_right) {
                to_erase.insert(interval);
                if (min_left <= max_left - 1) {
                    to_insert.emplace(min_left, max_left - 1);
                }
                to_insert.emplace(max_left, min_right);
                if (right + 1 <= interval.second) {
                    to_insert.emplace(right + 1, interval.second);
                }
                left = interval.second + 1;
            }
        }

        if (right >= left) {
            to_insert.emplace(left, right);
        }

        for (pair<int, int> const& interval : to_erase) {
            intervals.erase(interval);
        }
        for (pair<int, int> const& interval : to_insert) {
            intervals.insert(interval);
        }

    }

    void remove(int left, int right) {
        set<pair<int, int>> to_insert;

        erase_if(intervals, [&to_insert, left, right](pair<int, int> const& interval) {
            if (max(interval.first, left) <= min(interval.second, right)) {
                if (left - 1 >= interval.first) {
                    to_insert.emplace(interval.first, left - 1);
                }
                if (right + 1 <= interval.second) {
                    to_insert.emplace(right + 1, interval.second);
                }
                return true;
            }
            return false;
        });

        for (auto& interval : to_insert) {
            intervals.emplace(interval);
        }
    }

    size_t size() const {
        return reduce(intervals.begin(), intervals.end(), (size_t) 0, [](size_t acc, pair<int, int> interval) {
            return acc + (size_t) (interval.second - interval.first + 1);
        });
    }

    void print() const {
        for (auto[l, r] : intervals) {
            cout << '(' << l << ", " << r << ") ";
        }
        cout << '\n' << intervals.size() << ' ' << size() << ' ' << '\n';
    }

    set<pair<int, int>> const& get_intervals() const {
        return intervals;
    }

private:
    set<pair<int, int>> intervals;
};

class Day22 : public Day<size_t> {
public:
    explicit Day22(int day_number) : Day(day_number) {
        smatch line_match;
        for (string const& line : input_lines) {
            regex_match(line, line_match, line_regex);
            bool turn_on = line_match[1] == "on";
            sextuple coordinates;
            for (int i = 1; i <= 6; ++i) {
                coordinates[i - 1] = stoi(line_match[i + 1]);
            }

            actions.emplace_back(turn_on, coordinates);
        }
    }

    size_t part1() override;

    size_t part2() override;


private:
    inline static const regex line_regex = regex(
            R"(^([a-z]{2,3}) x=(-*\d+)\.\.(-*\d+),y=(-*\d+)\.\.(-*\d+),z=(-*\d+)\.\.(-*\d+)$)");

    vector<pair<bool, sextuple>> actions;
};


size_t Day22::part1() {
    int constexpr row_size = 2 * PART1_SIZE + 1;
    int constexpr grid_size = row_size * row_size * row_size;

    bitset<grid_size> on_cubes;
    for (auto[action, coordinates] : actions) {
        auto[x1, x2, y1, y2, z1, z2] = coordinates;
        if (abs(x1) <= PART1_SIZE && abs(x2) <= PART1_SIZE && abs(y1) <= PART1_SIZE && abs(y2) <= PART1_SIZE &&
            abs(z1) <= PART1_SIZE && abs(z2) <= PART1_SIZE) {
            for (int x = x1; x <= x2; ++x) {
                for (int y = y1; y <= y2; ++y) {
                    for (int z = z1; z <= z2; ++z) {
                        on_cubes[(x + PART1_SIZE) * row_size * row_size + (y + PART1_SIZE) * row_size + z +
                                 PART1_SIZE] = action;
                    }
                }
            }
        }


    }
    return on_cubes.count();
}

size_t Day22::part2() {
    size_t on_cubes = 0;

    SplitIntervalSet x_set;
    for (auto[action, coordinates] : actions) {
        auto[x1, x2, y1, y2, z1, z2] = coordinates;
        x_set.insert(x1, x2);
    }

    for (auto[interval_x1, interval_x2] : x_set.get_intervals()) {
        SplitIntervalSet y_set;
        for (auto[action, coordinates] : actions) {
            auto[x1, x2, y1, y2, z1, z2] = coordinates;
            if (interval_x1 >= x1 && interval_x2 <= x2) {
                y_set.insert(y1, y2);
            }
        }

        for (auto[interval_y1, interval_y2] : y_set.get_intervals()) {
            SplitIntervalSet z_set;
            for (auto[action, coordinates] : actions) {
                auto[x1, x2, y1, y2, z1, z2] = coordinates;
                if (interval_x1 >= x1 && interval_x2 <= x2 && interval_y1 >= y1 && interval_y2 <= y2) {
                    if (action) {
                        z_set.insert(z1, z2);
                    } else {
                        z_set.remove(z1, z2);
                    }
                }
            }

            on_cubes += z_set.size() * (interval_y2 - interval_y1 + 1) * (interval_x2 - interval_x1 + 1);
        }

    }

    return on_cubes;
}


int main() {
    Day22 day = Day22(DAY);

    cout << day.part1() << '\n';
    cout << day.part2() << '\n';
    return 0;
}


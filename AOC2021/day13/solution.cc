#include <iostream>
#include <unordered_map>
#include <sstream>
#include <unordered_set>
#include <algorithm>
#include "../includes/utils.h"

using std::pair, std::make_pair, std::unordered_set, std::stringstream, std::stoi, std::max, std::min;

int const DAY = 13;
int const X_AXIS = 0;
int const Y_AXIS = 1;

using dot_function = std::function<bool(int, int)>;

class Day13 : public Day<int> {
public:
    explicit Day13(int day_number) : Day(day_number) {
        for (string const& line : input_lines) {
            if (!line.empty()) {
                stringstream string_stream(line);
                if (line[0] != 'f') {
                    string x_str, y_str;
                    getline(string_stream, x_str, ',');
                    getline(string_stream, y_str);

                    int x = stoi(x_str), y = stoi(y_str);

                    dots.insert(make_pair(x, y));
                    max_x = max(max_x, x);
                    max_y = max(max_y, y);
                } else {
                    string coordinate_str;
                    getline(string_stream, coordinate_str, '=');
                    getline(string_stream, coordinate_str);

                    int axis = line[11] - 'x';
                    int coordinate = stoi(coordinate_str);
                    min_coordinates[axis] = min(min_coordinates[axis], coordinate);

                    folds.emplace_back(make_pair(axis, coordinate));
                }
            }
        }

    }

    dot_function fold_paper(dot_function const& check_dot_on_paper, int axis, int fold_coordinate) {
        return [check_dot_on_paper, axis, fold_coordinate](int x_arg, int y_arg) {
            int coordinates_before_fold[2] = {x_arg, y_arg};

            if (coordinates_before_fold[axis] > fold_coordinate) {
                return false;
            }

            coordinates_before_fold[axis] = 2 * fold_coordinate - coordinates_before_fold[axis];

            return check_dot_on_paper(x_arg, y_arg) ||
                   check_dot_on_paper(coordinates_before_fold[X_AXIS], coordinates_before_fold[Y_AXIS]);
        };
    }

    int part1() override;

    int part2() override;


private:
    unordered_set<pair<int, int>> dots;
    vector<pair<int, int>> folds;
    int max_x = 0, max_y = 0;
    int min_coordinates[2] = {INT_MAX, INT_MAX};

    dot_function check_dot = [this](int x, int y) {
        return dots.contains(make_pair(x, y));
    };

};

int Day13::part1() {
    check_dot = fold_paper(check_dot, folds[0].first, folds[0].second);

    int dot_count = 0;
    for (int i = 0; i < max_y; ++i) {
        for (int j = 0; j < max_x; ++j) {
            dot_count += check_dot(i, j);
        }
    }

    return dot_count;
}

int Day13::part2() {
    check_dot = [this](int x, int y) {
        return dots.contains(make_pair(x, y));
    };

    for (auto[axis, coordinate] : folds) {
        check_dot = fold_paper(check_dot, axis, coordinate);
    }

    for (int j = 0; j < min_coordinates[Y_AXIS]; ++j) {
        for (int i = 0; i < min_coordinates[X_AXIS]; ++i) {
            if (check_dot(i, j)) {
                cout << '#';
            } else {
                cout << '.';
            }
        }
        cout << '\n';
    }

    return 0;
}


int main() {
    Day13 day = Day13(DAY);

    cout << day.part1() << '\n';
    cout << day.part2() << '\n';

    return 0;
}

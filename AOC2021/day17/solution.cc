#include <iostream>
#include <sstream>
#include <cmath>
#include "../includes/utils.h"

using std::stringstream, std::stoi, std::sqrt;

int const DAY = 17;

class Day17 : public Day<int> {
public:
    explicit Day17(int day_number) : Day(day_number) {
        stringstream string_stream(input_lines[0]);
        string store_string;

        getline(string_stream, store_string, '=');
        getline(string_stream, store_string, '.');
        x_min = stoi(store_string);
        getline(string_stream, store_string, '.');
        getline(string_stream, store_string, ',');
        x_max = stoi(store_string);
        getline(string_stream, store_string, '=');
        getline(string_stream, store_string, '.');
        y_min = stoi(store_string);
        getline(string_stream, store_string, '.');
        getline(string_stream, store_string);
        y_max = stoi(store_string);
    }

    bool is_vy_possible(int vy) const {
        double delta_min = 1. + 4. * (vy * (vy + 1.) - 2 * y_min);
        double delta_max = 1. + 4. * (vy * (vy + 1.) - 2 * y_max);

        double n1_float = (-1. + sqrt(delta_min)) / 2.;
        double n2_float = (-1. + sqrt(delta_max)) / 2.;
        return (int) n1_float != (int) n2_float;
    }

    bool are_velocities_possible(int vx, int vy) const {
        int x = 0, y = 0;
        while (x <= x_max && y >= y_min) {
            if (x >= x_min && y <= y_max) {
                return true;
            }
            x += vx;
            y += vy;
            if (vx != 0) {
                vx -= vx / abs(vx);
            }
            vy--;
        }
        return false;
    }

    int part1() override;

    int part2() override;


private:
    int x_min, x_max;
    int y_min, y_max;
    int max_vy = 0;

};

int Day17::part1() {
    for (int i = 10000; i > 0; --i) {
        if (is_vy_possible(i)) {
            max_vy = i;
            break;
        }
    }

    return max_vy * (max_vy + 1) / 2;
}

int Day17::part2() {
    int possible_count = 0;
    for (int vx = 0; vx < x_max + 1; ++vx) {
        for (int vy = y_min - 1; vy <= max_vy; ++vy) {
            possible_count += are_velocities_possible(vx, vy);
        }
    }
    return possible_count;
}


int main() {
    Day17 day = Day17(DAY);

    cout << day.part1() << '\n';
    cout << day.part2() << '\n';

    return 0;
}


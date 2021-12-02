#include <iostream>
#include <vector>
#include "../includes/utils.h"

using std::cout, std::vector;

int const DAY = 1;

using data_type = int;
using type_container = data_container<data_type>;

int part1(type_container& depths) {
    int increased = 0, current_depth = INT_MAX;

    for (int depth : depths) {
        increased += depth > current_depth;
        current_depth = depth;
    }

    return increased;
}

int part2(type_container& depths) {
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
    type_container depths = input_lines<data_type>(DAY);

    cout << part1(depths) << '\n';
    cout << part2(depths) << '\n';

    return 0;
}

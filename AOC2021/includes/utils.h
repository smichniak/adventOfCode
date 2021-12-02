#ifndef AOC2021_UTILS_H
#define AOC2021_UTILS_H

#include <vector>
#include <fstream>
#include <string>

template<typename T>
using data_container = std::vector<T>;

template<typename T>
std::vector<T> input_lines(int day)  {
    std::string input_file_name = "day" + std::to_string(day) + "/input.in";
    std::ifstream file_stream(input_file_name);

    std::vector<T> line_items;
    T line_item;
    while (file_stream >> line_item) {
        line_items.push_back(line_item);
    }
    return line_items;
}

#endif //AOC2021_UTILS_H

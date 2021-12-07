#ifndef AOC2021_UTILS_H
#define AOC2021_UTILS_H

#include <vector>
#include <fstream>
#include <string>

using std::cout, std::vector, std::string;

template<typename T>
class Day {
public:
    explicit Day(int day_number) {
        string input_file_name = "day" + std::to_string(day_number) + "/input.in";
        std::ifstream file_stream(input_file_name);

        string line;
        while (std::getline(file_stream, line, '\n')) {
            input_lines.push_back(line);
        }

    }

    virtual T part1() = 0;

    virtual T part2() = 0;

protected:
    vector<string> input_lines;
};

#endif //AOC2021_UTILS_H

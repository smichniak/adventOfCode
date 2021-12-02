#ifndef AOC2021_UTILS_H
#define AOC2021_UTILS_H

#include <vector>
#include <fstream>
#include <string>

using std::cout, std::vector, std::string;

class Day {
public:
    Day(int day_number) {
        this->day_number = day_number;

        string input_file_name = "day" + std::to_string(day_number) + "/input.in";
        std::ifstream file_stream(input_file_name);

        string line;
        while (std::getline(file_stream, line, '\n')) {
            input_lines.push_back(line);
        }

    }

    virtual int part1() = 0;

    virtual int part2() = 0;

protected:
    vector<string> input_lines;

private:
    int day_number;
};

#endif //AOC2021_UTILS_H

#ifndef AOC2021_UTILS_H
#define AOC2021_UTILS_H

#include <vector>
#include <fstream>
#include <string>

using std::cout, std::vector, std::string;

int const DX[4] = {-1, 1, 0, 0};
int const DY[4] = {0, 0, -1, 1};

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

namespace std {
    template<>
    struct hash<std::pair<int, int>> {
        inline size_t operator()(const std::pair<int, int>& v) const {
            std::hash<int> int_hasher;
            return int_hasher(v.first) ^ int_hasher(v.second);
        }
    };

    template<>
    struct hash<std::pair<char, char>> {
        inline size_t operator()(const std::pair<int, int>& v) const {
            std::hash<char> char_hasher;
            return char_hasher(v.first) ^ char_hasher(v.second);
        }
    };
}

#endif //AOC2021_UTILS_H

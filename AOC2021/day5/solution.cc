#include <iostream>
#include <sstream>
#include <set>
#include <algorithm>
#include "../includes/utils.h"

using std::tuple, std::stringstream, std::getline, std::stoi, std::make_tuple, std::set, std::max, std::min;

using quadruple = tuple<int, int, int, int>;

int const DAY = 5;

class Board {
public:
    Board() = default;

    Board(int x, int y) {
        width = x + 1;
        fields = vector<int>(width * (y + 1), 0);
    }

    void mark(int x, int y) {
        fields[width * y + x]++;
    }

    int overlaps(int k) {
        int count = 0;
        for (int point : fields) {
            count += point >= k;
        }
        return count;
    }

private:
    int width;
    vector<int> fields;
};

class Day5 : public Day<int> {
public:
    explicit Day5(int day_number) : Day(day_number) {
        int max_x = 0, max_y = 0;

        for (string const& line : input_lines) {
            int x1, y1, x2, y2;
            stringstream string_stream(line);
            string string_part;

            getline(string_stream, string_part, ',');
            x1 = stoi(string_part);
            getline(string_stream, string_part, ' ');
            y1 = stoi(string_part);
            getline(string_stream, string_part, ' ');
            getline(string_stream, string_part, ',');
            x2 = stoi(string_part);
            string_stream >> y2;
            points.emplace_back(make_tuple(x1, y1, x2, y2));
            max_x = max(max(max_x, x1), x2);
            max_y = max(max(max_y, y1), y2);
        }
        board = Board(max_x, max_y);

        for (auto[x1, y1, x2, y2] : points) {
            if (y1 == y2) {
                for (int i = min(x1, x2); i <= max(x1, x2); ++i) {
                    board.mark(i, y1);
                }
            } else if (x1 == x2) {
                for (int i = min(y1, y2); i <= max(y1, y2); ++i) {
                    board.mark(x1, i);
                }
            }
        }

    }

    int part1() override;

    int part2() override;

private:
    vector<quadruple> points; // x1, y1, x2, y2
    Board board;
};

int Day5::part1() {
    return board.overlaps(2);
}

int Day5::part2() {
    for (auto[x1, y1, x2, y2] : points) {
        if (x1 != x2 && y1 != y2) {
            for (int i = 0; i <= abs(x2 - x1); ++i) {
                board.mark(x1 + i - 2 * i * (x1 > x2), y1 + i - 2 * i * (y1 > y2));
            }
        }
    }

    return board.overlaps(2);
}

int main() {
    Day5 day = Day5(DAY);

    cout << day.part1() << '\n';
    cout << day.part2() << '\n';

    return 0;
}

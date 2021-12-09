#include <iostream>
#include <sstream>
#include <unordered_map>
#include <set>
#include <numeric>
#include "../includes/utils.h"

using std::vector, std::stringstream, std::unordered_map, std::reduce, std::set;

int const DAY = 8;
int const PATTERNS = 10;
int const OUTPUTS = 4;

enum Segment {
    a = 1, b = 2, c = 4, d = 8, e = 16, f = 32, g = 64
};

enum Digit {
    zero = a + b + c + e + f + g,
    one = c + f,
    two = a + c + d + e + g,
    three = a + c + d + f + g,
    four = b + c + d + f,
    five = a + b + d + f + g,
    six = a + b + d + e + f + g,
    seven = a + c + f,
    eight = a + b + c + d + e + f + g,
    nine = a + b + c + d + f + g,
};

unordered_map<char, int> letters({{'a', a},
                                  {'b', b},
                                  {'c', c},
                                  {'d', d},
                                  {'e', e},
                                  {'f', f},
                                  {'g', g}});

int count_bits(int n) {
    int count = 0;
    while (n) {
        count += n & 1;
        n >>= 1;
    }
    return count;
}

void get_digits(stringstream& string_stream, vector<int>& storage, int iterations) {
    string letter_string;

    for (int i = 0; i < iterations; ++i) {
        int digit = 0;
        getline(string_stream, letter_string, ' ');

        for (char letter : letter_string) {
            digit += letters[letter];
        }
        storage.push_back(digit);
    }
}


class Day8 : public Day<int> {
public:
    explicit Day8(int day_number) : Day(day_number) {
        for (string const& line : input_lines) {
            stringstream string_stream(line);
            string _;

            get_digits(string_stream, patterns, PATTERNS);
            getline(string_stream, _, ' ');
            get_digits(string_stream, outputs, OUTPUTS);
        }

        for (int digit : {zero, one, two, three, four, five, six, seven, eight, nine}) {
            bit_counter[digit].push_back(count_bits(digit));
        }
    }

    int part1() override;

    int part2() override;


private:
    vector<int> patterns;
    vector<int> outputs;
    unordered_map<int, vector<int>> bit_counter;
};

int Day8::part1() {
    auto simple_digit = [this](int acc, int digit) {
        int bits = count_bits(digit);
        return acc +
               (bits == bit_counter[one][0] || bits == bit_counter[four][0] || bits == bit_counter[seven][0] ||
                bits == bit_counter[eight][0]);
    };

    return reduce(outputs.begin(), outputs.end(), 0, simple_digit);
}

int Day8::part2() {
    int out_sum = 0;
    for (int i = 0; i < patterns.size(); i += PATTERNS) {
        unordered_map<int, set<int>> possible_connection;
        for (int index = 0; index< PATTERNS; ++ index) {
            int signal = patterns[i + index];
            int bits = count_bits(signal);
            // Find all possible ditis in bit_counter
            // For each of those digits
            // For each bit in signal
            // possible_connection[bit].add/intersect(digit)
            
        }

    }

}


int main() {
    Day8 day = Day8(DAY);

    cout << day.part1() << '\n';
    cout << day.part2() << '\n';

    return 0;
}

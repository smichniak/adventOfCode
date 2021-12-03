#include <iostream>
#include <vector>
#include <bitset>
#include <algorithm>
#include "../includes/utils.h"

using std::vector, std::bitset, std::copy_if, std::back_inserter;

int const DAY = 3;
int constexpr NUMBER_LENGTH = 12;

inline int num_bit(int number, int bit) {
    return (number >> bit) % 2;
}

int bit_sum(vector<int> const& numbers, int bit) {
    int sum = 0;
    for (int num : numbers) {
        sum += num_bit(num, bit);
    }
    return sum;
}

bitset<NUMBER_LENGTH> most_common_bits(vector<int> const& numbers) {
    vector<int> bit_sums = vector<int>(NUMBER_LENGTH, 0);

    for (int bit = 0; bit < NUMBER_LENGTH; ++bit) {
        bit_sums[bit] = bit_sum(numbers, bit);
    }

    bitset<NUMBER_LENGTH> common_bits;

    for (int i = NUMBER_LENGTH - 1; i >= 0; --i) {
        common_bits[i] = 2 * bit_sums[i] >= numbers.size();
    }
    return common_bits;
}

int best_bit_match(vector<int> numbers, bool swap) {
    int bit = NUMBER_LENGTH - 1;
    while (numbers.size() > 1) {
        vector<int> new_numbers;

        int numbers_bit_sum = bit_sum(numbers, bit);

        int common_bit = (2 * numbers_bit_sum >= numbers.size()) ^swap;

        copy_if(numbers.begin(), numbers.end(), back_inserter(new_numbers),
                [bit, common_bit](int num) { return num_bit(num, bit) == common_bit; });

        numbers = new_numbers;
        bit--;
    }

    return numbers[0];
}

class Day3 : public Day {
public:
    explicit Day3(int day_number) : Day(day_number) {
        for (string const& line : input_lines) {
            numbers.push_back(stoi(line, nullptr, 2));
        }
    }

    int part1() override;

    int part2() override;

private:
    vector<int> numbers;
};

int Day3::part1() {
    bitset<NUMBER_LENGTH> common_bits = most_common_bits(numbers);

    return common_bits.to_ulong() * (~common_bits).to_ulong();
}

int Day3::part2() {
    return best_bit_match(numbers, false) * best_bit_match(numbers, true);
}

int main() {
    Day3 day = Day3(DAY);

    cout << day.part1() << '\n';
    cout << day.part2() << '\n';

    return 0;
}

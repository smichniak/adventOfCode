#include <iostream>
#include <unordered_map>
#include <algorithm>
#include "../includes/utils.h"

using std::unordered_map, std::minmax_element, std::remove_if, std::make_pair, std::pair, std::for_each;

int const DAY = 14;
int const STEPS1 = 10;
int const STEPS2 = 40;

class Day14 : public Day<long long> {
public:
    explicit Day14(int day_number) : Day(day_number) {
        first_letter = input_lines[0][0];
        last_letter = input_lines[0][input_lines[0].size() - 1];

        for (int i = 0; i < input_lines[0].size() - 1; ++i) {
            auto letter_pair = make_pair(input_lines[0][i], input_lines[0][i + 1]);
            if (!pair_count.contains(letter_pair)) {
                pair_count[letter_pair] = 0;
            }

            pair_count[letter_pair]++;
        }

        for (int i = 2; i < input_lines.size(); ++i) {
            insertion_rules[make_pair(input_lines[i][0], input_lines[i][1])] = input_lines[i][6];
        }
    }

    static void
    add_key(unordered_map<pair<char, char>, long long>& count_map, pair<char, char> key, long long count) {
        if (!count_map.contains(key)) {
            count_map[key] = 0;
        }
        count_map[key] += count;
    }

    void insertion_step() {
        unordered_map<pair<char, char>, long long> new_pair_count;

        for (auto[key, count] : pair_count) {
            if (!insertion_rules.contains(key)) {
                add_key(new_pair_count, key, count);
            } else {
                char middle_char = insertion_rules[key];
                auto[first_char, second_char] = key;
                add_key(new_pair_count, make_pair(first_char, middle_char), count);
                add_key(new_pair_count, make_pair(middle_char, second_char), count);
            }
        }
        pair_count = new_pair_count;
    }

    long long max_min_difference() {
        vector<long long> letter_count('Z' - 'A' + 1, 0);
        for (auto[key, count] : pair_count) {
            letter_count[key.first - 'A'] += count;
            letter_count[key.second - 'A'] += count;
        }
        letter_count[first_letter - 'A']++;
        letter_count[last_letter - 'A']++;

        for_each(letter_count.begin(), letter_count.end(), [](long long& n) { n /= 2; });
        letter_count.erase(
                remove_if(letter_count.begin(), letter_count.end(), [](long long i) { return i == 0; }),
                letter_count.end());
        auto[min, max] = minmax_element(letter_count.begin(), letter_count.end());
        return *max - *min;
    }

    long long part1() override;

    long long part2() override;


private:
    char first_letter, last_letter;
    unordered_map<pair<char, char>, long long> pair_count;
    unordered_map<pair<char, char>, char> insertion_rules;
};

long long Day14::part1() {
    for (int i = 0; i < STEPS1; ++i) {
        insertion_step();
    }

    return max_min_difference();
}

long long Day14::part2() {
    for (int i = 0; i < STEPS2 - STEPS1; ++i) {
        insertion_step();
    }

    return max_min_difference();
}


int main() {
    Day14 day = Day14(DAY);

    cout << day.part1() << '\n';
    cout << day.part2() << '\n';

    return 0;
}

#include <iostream>
#include <stack>
#include <queue>
#include <set>
#include <algorithm>
#include <unordered_map>
#include "../includes/utils.h"

using std::stack, std::unordered_map, std::queue, std::sort;

int const DAY = 10;
unordered_map<char, int> scores_corrupted = {{')', 3},
                                             {']', 57},
                                             {'}', 1197},
                                             {'>', 25137}};

unordered_map<char, int> scores_incomplete = {{'(', 1},
                                              {'[', 2},
                                              {'{', 3},
                                              {'<', 4}};

unordered_map<char, char> valid_pairs = {{'(', ')'},
                                         {'[', ']'},
                                         {'{', '}'},
                                         {'<', '>'}};

class Day10 : public Day<long long> {
public:
    explicit Day10(int day_number) : Day(day_number) {}

    long long part1() override;

    long long part2() override;


private:
    queue<int> corrupted_lines;
};

long long Day10::part1() {

    int score = 0;
    int line_index = 0;
    for (string const& line : input_lines) {
        stack<char> brackets;
        for (char bracket : line) {
            if (!scores_corrupted.contains(bracket)) {
                brackets.push(bracket);
            } else {
                char top_bracket = brackets.top();
                brackets.pop();
                if (valid_pairs[top_bracket] != bracket) {
                    score += scores_corrupted[bracket];
                    corrupted_lines.push(line_index);
                    break;
                }

            }
        }
        line_index++;
    }

    return score;
}

long long Day10::part2() {
    int line_index = 0;
    vector<long long> line_scores;

    for (string const& line : input_lines) {
        long long score = 0;
        if (corrupted_lines.front() == line_index) {
            corrupted_lines.pop();
        } else {
            stack<char> brackets;
            for (char bracket : line) {
                if (!scores_corrupted.contains(bracket)) {
                    brackets.push(bracket);
                } else {
                    brackets.pop();
                }
            }

            while (!brackets.empty()) {
                char top_bracket = brackets.top();
                brackets.pop();
                score *= 5;
                score += scores_incomplete[top_bracket];
            }
            line_scores.push_back(score);
        }

        line_index++;
    }

    sort(line_scores.begin(), line_scores.end());
    return line_scores[line_scores.size() / 2];
}


int main() {
    Day10 day = Day10(DAY);

    cout << day.part1() << '\n';
    cout << day.part2() << '\n';

    return 0;
}

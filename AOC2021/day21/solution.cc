#include <iostream>
#include <regex>
#include <map>
#include <algorithm>
#include "../includes/utils.h"

using std::regex, std::smatch, std::stoi, std::min, std::map, std::pair, std::max;

int const DAY = 21;
int const BOARD_SIZE = 10;
int const MAX_SCORE1 = 1000;
int const MAX_SCORE2 = 21;

class Day21 : public Day<long long> {
public:
    explicit Day21(int day_number) : Day(day_number) {
        smatch line_match;
        regex_match(input_lines[0], line_match, line_regex);
        player1_position = stoi(line_match[1]);
        regex_match(input_lines[1], line_match, line_regex);
        player2_position = stoi(line_match[1]);

        for (int i = 1; i <= 3; ++i) {
            for (int j = 1; j <= 3; ++j) {
                for (int k = 1; k <= 3; ++k) {
                    dirac_dice_outcomes[i + j + k]++;
                }
            }
        }
    }


    long long part1() override;

    long long part2() override;


private:
    inline static const regex line_regex = regex(R"(.*:\s(\d+)$)");
    int player1_position;
    int player2_position;

    map<int, int> dirac_dice_outcomes;

    static int new_position(int old_position, int roll_sum) {
        int new_player_position = (old_position + roll_sum) % BOARD_SIZE;
        if (new_player_position == 0) {
            new_player_position += BOARD_SIZE;
        }
        return new_player_position;
    }

    pair<long long, long long> play_game(int position1, int position2, int score1, int score2) {
        if (score2 >= MAX_SCORE2) {
            return {0, 1};
        }

        long long wins1 = 0, wins2 = 0;
        for (auto[roll_sum, frequency] : dirac_dice_outcomes) {
            int new_position1 = new_position(position1, roll_sum);
            auto[won2, won1] = play_game(position2, new_position1, score2, score1 + new_position1);
            wins1 += won1 * frequency;
            wins2 += won2 * frequency;
        }
        return {wins1, wins2};
    }

};


long long Day21::part1() {
    int rolls = 0;
    int player1_score = 0, player2_score = 0;
    int player1_current_position = player1_position, player2_current_position = player2_position;
    while (player1_score < MAX_SCORE1 && player2_score < MAX_SCORE1) {
        int current_roll_sum = 6 + 3 * rolls;
        if (rolls % 6 == 0) {
            player1_current_position = new_position(player1_current_position, current_roll_sum);
            player1_score += player1_current_position;
        } else {
            player2_current_position = new_position(player2_current_position, current_roll_sum);
            player2_score += player2_current_position;
        }
        rolls += 3;
    }

    return rolls * min(player1_score, player2_score);
}

long long Day21::part2() {
    auto [wins1, wins2] = play_game(player1_position, player2_position, 0, 0);
    return max(wins1, wins2);
}


int main() {
    Day21 day = Day21(DAY);

    cout << day.part1() << '\n';
    cout << day.part2() << '\n';
    return 0;
}

